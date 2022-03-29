use crate::cart::{Cartridge, CartMem, Mirroring};
use bitflags::bitflags;


bitflags! {
    // PPU Control register is a series of flags:
    // V P H B S I N N
    // ^ ^ ^ ^ ^ ^ ^ ^ 
    // | | | | | | +-+- Base Nametable Address
    // | | | | | +----- VRAM Address Increment (when reading PPUDATA)
    // | | | | +------- Sprite pattern table addr (for 8x8 sprites)
    // | | | +--------- Background pattern table addr (0: $0000, 1: $1000)
    // | | +----------- Sprite size (0: 8x8, 1: 8x16)
    // | +------------- PPU Master/Slave select (0: read backdrop from EXT, 1: write)
    // +--------------- VBlank Interrupt enable 
    pub struct PpuControl: u8 {
        const NMI_ENABLE       = 0b1000_0000;
        const PPU_MASTER       = 0b0100_0000;
        const SPRITE_SIZE      = 0b0010_0000;
        const BG_TBL           = 0b0001_0000;
        const SPRITE_TBL       = 0b0000_1000;
        const VRAM_ADDR_INCR   = 0b0000_0100;
        const BASE_TBL2        = 0b0000_0010;
        const BASE_TBL1        = 0b0000_0001;
    }
}

impl PpuControl {
    pub fn new() -> Self { PpuControl::from_bits_truncate(0b0000_0000) }
    pub fn vram_addr_increment(&self) -> u8 { if self.contains(PpuControl::VRAM_ADDR_INCR) { 32 } else { 1 } }
    pub fn update(&mut self, data: u8) { self.bits = data; }
}

bitflags! {
    // PPU Mask is also a series of flags:
    // B G R s b M m G
    // ^ ^ ^ ^ ^ ^ ^ ^
    // | | | | | | | +- Grayscale (0: normal, 1: grayscale)
    // | | | | | | +--- Show bg in leftmost 8 pixels of screen (0: hide)
    // | | | | | +----- Show sprites in leftmost 8 pixels (0: hide)
    // | | | | +------- Show background (0: hide)
    // | | | +--------- Show sprites (0: hide)
    // | | +----------- Emphasize red
    // | +------------- Emphasize green
    // +--------------- Emphasize blue
    pub struct PpuMask: u8 {
        const EMPH_BLUE = 0b1000_0000;
        const EMPH_GRN = 0b0100_0000;
        const EMPH_RED = 0b0010_0000;
        const SHOW_SPRITES = 0b0001_0000;
        const SHOW_BG = 0b0000_1000;
        const LEFT_SPRITES = 0b0000_0100;
        const LEFT_BG = 0b0000_0010;
        const GRAYSCALE = 0b0000_0001;
    }
}

impl PpuMask {
    pub fn new() -> Self { PpuMask::from_bits_truncate(0b0000_0000) }
    pub fn update(&mut self, data: u8) { self.bits = data; }
}

struct ScrollReg {
    scroll_x: u8,
    scroll_y: u8,

    set_x: bool,
}

impl ScrollReg {
    pub fn new() -> Self { ScrollReg { scroll_x: 0, scroll_y: 0, set_x: true } }
    pub fn update(&mut self, data: u8) {
        match self.set_x {
            true => self.scroll_x = data,
            false => self.scroll_y = data,
        };
        self.set_x = !self.set_x;
    }
    pub fn reset_latch(&mut self) { self.set_x = true; }
}

struct AddrReg {
    lo: u8,
    hi: u8,

    set_hi: bool,
}

impl AddrReg {
    pub fn new() -> Self { AddrReg { lo: 0, hi: 0, set_hi: true } }
    
    pub fn update(&mut self, data: u8) {
        match self.set_hi {
            true => self.hi = data,
            false => self.lo = data,
        };
        // Shift hi byte to mirror address
        self.mirror();
    }

    pub fn increment(&mut self, rhs: u8) {
        // Do increment across both bytes: if the lo byte overflows then
        // the hi byte must increment by 1 (carry in)
        let (new_lo, overflowed) = self.lo.overflowing_add(rhs);
        self.lo = new_lo;
        if overflowed {
            self.hi = self.hi.wrapping_add(1);
        }

        // Mirror addressed down to applicable range
        self.mirror();
    }
    
    fn mirror(&mut self) {
        // The Address Register can only point to values from 0x0000 to 0x3FFF.
        // So, the hi byte can only be between 0x00 and 0x3F.
        self.hi &= 0b0011_1111;
    }

    pub fn reset_latch(&mut self) { self.set_hi = true; }

    pub fn get(&self) -> u16 {
        ((self.hi as u16) << 8) | (self.lo as u16)
    }
}

pub struct PPU {
    pub vram: [u8; 2048],
    pub palette_table: [u8; 32],
    pub oam: [u8; 256],

    control: PpuControl,
    mask: PpuMask,
    scroll: ScrollReg,
    ppu_addr: AddrReg,
    oam_addr: u8,
}

impl Default for PPU {
    fn default() -> Self {
        PPU { 
            vram: [0; 2048],
            palette_table: [0; 32],
            oam: [0; 256],

            control: PpuControl::new(),
            mask: PpuMask::new(),
            scroll: ScrollReg::new(),
            ppu_addr: AddrReg::new(),
            oam_addr: 0,
        }
    }
}


impl PPU {
    pub fn mem_read(&mut self, addr: u16, cart: &Cartridge) -> u8 {
        // PPU Reading/Writing requires the Cartridge since CHR ROM is memory-mapped within the PPU
        match addr {
            // Status
            0x2002 => todo!(),
            // OAM Data
            0x2004 => self.oam[self.oam_addr as usize],
            // PPU Data
            0x2007 => {
                let res = self.read_internal(addr, cart);
                self.ppu_addr.increment(self.control.vram_addr_increment());
                res
            },
            _ => panic!("invalid read of {:04X} on PPU", addr),
        }
    }

    pub fn mem_write(&mut self, addr: u16, value: u8, cart: &mut Cartridge) {
        match addr {
            // PPU Control
            0x2000 => self.control.update(value),
            // Mask
            0x2001 => self.mask.update(value),
            // OAM Addr
            0x2003 => self.oam_addr = value,
            // OAM Data
            0x2004 => {
                self.oam[self.oam_addr as usize] = value;
                self.oam_addr += 1;
            }
            // Scroll
            0x2005 => self.scroll.update(value),
            // PPU Address
            0x2006 => self.ppu_addr.update(value),
            // PPU Data
            0x2007 => {
                self.write_internal(self.ppu_addr.get(), value, cart);
                self.ppu_addr.increment(self.control.vram_addr_increment());
            },
            _ => panic!("invalid write to {:04X} on PPU", addr),
        }
    }

    fn mirror_vram_addr(&self, addr: u16, cart: &Cartridge) -> u16 {
        let mirrored_vram = addr & 0b10111111111111; // mirror down 0x3000-0x3eff to 0x2000 - 0x2eff
        let vram_index = mirrored_vram - 0x2000; // to vram vector
        let name_table = vram_index / 0x400;
        match (cart.get_mirroring(), name_table) {
            (Mirroring::VERTICAL, 2) | (Mirroring::VERTICAL, 3) => vram_index - 0x800,
            (Mirroring::HORIZONTAL, 2) => vram_index - 0x400,
            (Mirroring::HORIZONTAL, 1) => vram_index - 0x400,
            (Mirroring::HORIZONTAL, 3) => vram_index - 0x800,
            _ => vram_index,
        }
    }

    fn read_internal(&self, addr: u16, cart: &Cartridge) -> u8 {
        match addr {
            0x0000..=0x1FFF => cart.chr_read(addr),
            0x2000..=0x3EFF => self.vram[self.mirror_vram_addr(addr, cart) as usize],
            0x3F00..=0x3FFF => self.palette_table[(addr & 0b0001_1111) as usize],
            _ => panic!("Invalid read at PPU address {}", addr),
        }
    }

    fn write_internal(&mut self, addr: u16, value: u8, cart: &mut Cartridge) {
        match addr {
            0x0000..=0x1FFF => cart.chr_write(addr),
            0x2000..=0x3EFF => self.vram[self.mirror_vram_addr(addr, cart) as usize] = value,
            0x3F00..=0x3FFF => self.palette_table[(addr & 0b0001_1111) as usize] = value,
            _ => panic!("Invalid write at PPU address {}", addr),
        }
    }

    pub fn oam_dma(&mut self, data: &[u8; 256]) {
        for x in data.iter() {
            self.oam[self.oam_addr as usize] = *x;
            self.oam_addr = self.oam_addr.wrapping_add(1);
        }
    }

    pub fn new() -> Self {
        Self::default()
    }

    pub fn tick(&mut self, cycles: u8, cart: &Cartridge) {

    }
}