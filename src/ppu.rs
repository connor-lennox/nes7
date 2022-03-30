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
        self.set_hi = !self.set_hi;
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

#[derive(Clone, Copy)]
struct OAMEntry {
    pub y: u8,
    pub tile: u8,
    pub attributes: u8,
    pub x: u8,
}

impl OAMEntry {
    pub fn new(y: u8, tile: u8, attributes: u8, x: u8) -> Self {
        OAMEntry { y, tile, attributes, x }
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

    vblank: bool,
    s0_hit: bool,
    sprite_overflow: bool,
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

            vblank: false,
            s0_hit: false,
            sprite_overflow: false,
        }
    }
}


impl PPU {
    pub fn oam_dma(&mut self, data: &[u8; 256]) {
        for x in data.iter() {
            self.oam[self.oam_addr as usize] = *x;
            self.oam_addr = self.oam_addr.wrapping_add(1);
        }
    }

    pub fn read_status(&mut self) -> u8 {
        let mut res = 0b0000_0000;
        if self.sprite_overflow { res |= 0b0010_0000 }
        if self.s0_hit { res |= 0b0100_0000 }
        if self.vblank { res |= 0b1000_0000 }

        // Bits 0-5 should technically contain residual data from prior
        // writes, but the data itself decays extremely rapidly and 
        // no games depend on this behavior.

        // Reading this register clears the VBLANK flag and resets both latches
        self.vblank = false;
        self.ppu_addr.reset_latch();
        self.scroll.reset_latch();

        res
    }

    fn get_oam_entries(&self) -> Vec<OAMEntry> {
        // OAM Entries are packed into the OAM RAM in groups of 4 bytes: (y, tile, attributes, x)
        self.oam.chunks(4).map(|c| OAMEntry::new(c[0], c[1], c[2], c[3])).collect()
    }

    pub fn new() -> Self {
        Self::default()
    }
}

pub fn ppu_read(ppu: &mut PPU, cart: &Cartridge, addr: u16) -> u8 {
    // PPU Reading/Writing requires the Cartridge since CHR ROM is memory-mapped within the PPU
    match addr {
        // Status
        0x2002 => ppu.read_status(),
        // OAM Data
        0x2004 => ppu.oam[ppu.oam_addr as usize],
        // PPU Data
        0x2007 => {
            let res = read_internal(ppu, cart, addr);
            ppu.ppu_addr.increment(ppu.control.vram_addr_increment());
            res
        },
        _ => panic!("invalid read of {:04X} on PPU", addr),
    }
}

pub fn ppu_write(ppu: &mut PPU, cart: &mut Cartridge, addr: u16, value: u8) {
    match addr {
        // PPU Control
        0x2000 => ppu.control.update(value),
        // Mask
        0x2001 => ppu.mask.update(value),
        // OAM Addr
        0x2003 => ppu.oam_addr = value,
        // OAM Data
        0x2004 => {
            ppu.oam[ppu.oam_addr as usize] = value;
            ppu.oam_addr += 1;
        }
        // Scroll
        0x2005 => ppu.scroll.update(value),
        // PPU Address
        0x2006 => ppu.ppu_addr.update(value),
        // PPU Data
        0x2007 => {
            write_internal(ppu, cart, ppu.ppu_addr.get(), value);
            ppu.ppu_addr.increment(ppu.control.vram_addr_increment());
        },
        _ => panic!("invalid write to {:04X} on PPU", addr),
    }
}

fn read_internal(ppu: &mut PPU, cart: &Cartridge, addr: u16) -> u8 {
    match addr {
        0x0000..=0x1FFF => cart.chr_read(addr),
        0x2000..=0x3EFF => ppu.vram[mirror_vram_address(addr, cart.get_mirroring()) as usize],
        0x3F00..=0x3FFF => ppu.palette_table[(addr & 0b0001_1111) as usize],
        _ => panic!("Invalid read at PPU address {}", addr),
    }
}

fn write_internal(ppu: &mut PPU, cart: &mut Cartridge, addr: u16, value: u8) {
    match addr {
        0x0000..=0x1FFF => cart.chr_write(addr),
        0x2000..=0x3EFF => ppu.vram[mirror_vram_address(addr, cart.get_mirroring()) as usize] = value,
        0x3F00..=0x3FFF => ppu.palette_table[(addr & 0b0001_1111) as usize] = value,
        _ => panic!("Invalid write at PPU address {}", addr),
    }
}

fn mirror_vram_address(addr: u16, mirroring: Mirroring) -> u16 {
    // Applies VRAM mirroring and converts address into an index for VRAM array
    let mirrored_vram = addr & 0b10111111111111; // mirror down 0x3000-0x3eff to 0x2000 - 0x2eff
    let vram_index = mirrored_vram - 0x2000; // to vram vector
    let name_table = vram_index / 0x400;
    match (mirroring, name_table) {
        (Mirroring::VERTICAL, 2) | (Mirroring::VERTICAL, 3) => vram_index - 0x800,
        (Mirroring::HORIZONTAL, 2) => vram_index - 0x400,
        (Mirroring::HORIZONTAL, 1) => vram_index - 0x400,
        (Mirroring::HORIZONTAL, 3) => vram_index - 0x800,
        _ => vram_index,
    }
}

fn sprite_evaluation(ppu: &PPU, scanline: u8) -> Vec<OAMEntry> {
    // The position in the OAM determines the priority, so get them from start to finish
    // Only take elements with a Y value that is at least the current scanline, and
    // are no further away than the sprite height.
    // Also, we can only take the first 8 valid entries.
    let sprite_height = if ppu.control.contains(PpuControl::SPRITE_SIZE) { 8 } else { 16 };
    ppu.get_oam_entries().into_iter()
        .filter(|o| o.y >= scanline && (o.y - scanline) < sprite_height)
        .take(8)
        .collect::<Vec<OAMEntry>>()
}


#[cfg(test)]
mod test {
    use super::*;
    use crate::cart::test;

    #[test]
    fn test_vram_mirroring() {
        // Mirroring an address in the first nametable should just drop the range
        let addr1 = 0x3011;
        assert_eq!(mirror_vram_address(addr1, Mirroring::VERTICAL), 0x0011);
        assert_eq!(mirror_vram_address(addr1, Mirroring::HORIZONTAL), 0x0011);
        assert_eq!(mirror_vram_address(addr1, Mirroring::FOUR), 0x0011);

        let addr2 = 0x2410;
        assert_eq!(mirror_vram_address(addr2, Mirroring::VERTICAL), 0x0410);
        assert_eq!(mirror_vram_address(addr2, Mirroring::HORIZONTAL), 0x0010);
        
        let addr3 = 0x2810;
        assert_eq!(mirror_vram_address(addr3, Mirroring::VERTICAL), 0x0010);
        assert_eq!(mirror_vram_address(addr3, Mirroring::HORIZONTAL), 0x0410);
    }

    #[test]
    fn test_ppu_data_read_increments_addr() {
        let mut ppu = PPU::default();
        let cart = test::test_cart();

        // Should increment by 1 to 0x0001
        ppu.control.set(PpuControl::VRAM_ADDR_INCR, false);
        ppu.ppu_addr.hi = 0x00;
        ppu.ppu_addr.lo = 0x00;

        // Read from PPU_DATA register
        ppu_read(&mut ppu, &cart, 0x2007);

        assert_eq!(ppu.ppu_addr.get(), 0x0001);

        // Should increment by 32 to 0x0020
        ppu.control.set(PpuControl::VRAM_ADDR_INCR, true);
        ppu.ppu_addr.hi = 0x00;
        ppu.ppu_addr.lo = 0x00;

        // Read from PPU_DATA register
        ppu_read(&mut ppu, &cart, 0x2007);

        assert_eq!(ppu.ppu_addr.get(), 0x0020);
    }

    #[test]
    fn test_ppu_data_addr_write() {
        let mut ppu = PPU::default();
        let mut cart = test::test_cart();

        // Write the high byte, then low byte of address
        ppu_write(&mut ppu, &mut cart, 0x2006, 0x12);
        ppu_write(&mut ppu, &mut cart, 0x2006, 0x34);

        assert_eq!(ppu.ppu_addr.get(), 0x1234);
    }

    #[test]
    fn test_ppu_oam_write_increments_addr() {
        let mut ppu = PPU::default();
        let mut cart = test::test_cart();

        ppu.oam_addr = 0x0000;
        ppu_write(&mut ppu, &mut cart, 0x2004, 0x00);
        assert_eq!(ppu.oam_addr, 0x0001);
    }

    #[test]
    fn test_ppu_read_status_resets_latches() {
        let mut ppu = PPU::default();

        // Manually fire each latch and set VBLANK
        ppu.ppu_addr.set_hi = false;
        ppu.scroll.set_x = false;
        ppu.vblank = true;

        let status = ppu.read_status();
        assert!((status & 0b1000_0000) != 0);
        assert_eq!(ppu.ppu_addr.set_hi, true);
        assert_eq!(ppu.scroll.set_x, true);
        assert_eq!(ppu.vblank, false); 
    }

    #[test]
    fn test_oam_entry_read() {
        let mut ppu = PPU::default();

        ppu.oam[0..8].copy_from_slice(&[0x32, 0x12, 0x64, 0x12, 0xFF, 0x5B, 0xA4, 0x77]);
        let oam_entries = ppu.get_oam_entries();

        let e0 = oam_entries[0];
        let e1 = oam_entries[1];

        assert_eq!(e0.y, 0x32);
        assert_eq!(e0.tile, 0x12);
        assert_eq!(e0.attributes, 0x64);
        assert_eq!(e0.x, 0x12);

        assert_eq!(e1.y, 0xFF);
        assert_eq!(e1.tile, 0x5B);
        assert_eq!(e1.attributes, 0xA4);
        assert_eq!(e1.x, 0x77);
    }
}