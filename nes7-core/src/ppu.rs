use crate::cart::{Cartridge, CartMem, Mirroring};
use bitflags::bitflags;


pub struct FrameBuffer {
    // All pixels are stored as palette values in a single dimension
    // (x, y) -> y * 256 + x
    pub pixels: [u8; 61440],
}

impl FrameBuffer {
    pub fn new() -> Self { FrameBuffer { pixels: [0; 61440] } }
    pub fn set_line(&mut self, line: u8, from: impl Iterator<Item = u8>) {
        self.pixels[pixel_index(0, line as usize)..pixel_index(0, line as usize + 1)].iter_mut()
            .zip(from)
            .for_each(|(data, from)| *data = from);
    }
}

fn pixel_index(x: usize, y: usize) -> usize { y * 256 + x }


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

    current_scanline: u16,
    scanline_cycle: u16,
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

            current_scanline: 0,
            scanline_cycle: 0,
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

fn read_internal(ppu: &PPU, cart: &Cartridge, addr: u16) -> u8 {
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

fn get_palette(ppu: &PPU, palette: u8) -> [u8; 4] {
    // Retrieve the palette based on just the palette index.
    // The background palettes (0-3) are stored in 0x3F01 - 0x3F0F,
    // and sprite palettes (4-7) are in 0x3F11 - 0x3F1F.
    // The palette table itself starts at address 0x3000.
    let palette_base = 3 * palette as usize;
    [ppu.palette_table[0], ppu.palette_table[palette_base + 1], ppu.palette_table[palette_base + 2], ppu.palette_table[palette_base + 3]]
}

fn merge_bytes(lo: u8, hi: u8, flip_x: bool) -> impl Iterator<Item = u8> {
    // If we're flipping this over the x, we need to read from bit 7 to bit 0
    // Normally we read from bit 0 to bit 7.
    // Return an iterator of palette indicies (0x00 - 0x03) given the low and high bytes to merge
    let range: Vec<u8> = if flip_x { (0u8..=7).collect() } else { (0u8..=7).rev().collect() };
    range.into_iter().map(move |b| (if lo & 1 << b != 0 { 0b1 } else { 0 }) | (if hi & 1 << b != 0 { 0b10 } else { 0 }))
}

fn get_sprite_data(ppu: &PPU, cartridge: &Cartridge, scanline: u8, sprite: &OAMEntry) -> Vec<u8> {
    let table_base = if ppu.control.contains(PpuControl::SPRITE_TBL) { 0x0000 } else { 0x1000 };

    let mut y_offset = (scanline - sprite.y) as u16;
    // Flip the sprite vertically if bit 7 of the attributes is set.
    // A y offset of 7 becomes 0, and vice versa. Also 1 <-> 6, 2 <-> 5, etc.
    if sprite.attributes & 0b1000_0000 != 0 {
        y_offset = 7 - y_offset;
    }

    let tile_addr = (sprite.tile as u16) << 4;
    // Low and high sprite bytes differ by only bit 3
    let lo = read_internal(ppu, cartridge, table_base | tile_addr | y_offset);
    let hi = read_internal(ppu, cartridge, table_base | tile_addr | 0b1000 | y_offset);

    // Merge low and high bytes of the sprite data to get palette indices
    // The sprite may be mirrored horizontally, based on bit 6 of the sprite attributes.
    let flip_x = sprite.attributes & 0b0100_0000 != 0;
    let palette_indices = merge_bytes(lo, hi, flip_x);

    // Retrieve the relevant palette from the PPU palette table
    // Sprite palettes are numbered 4 - 7.
    let palette_num = (sprite.attributes & 0b0000_0011) + 4;
    let palette = get_palette(ppu, palette_num);

    palette_indices.map(|i| palette[i as usize]).collect()
}

fn get_sprite_line(ppu: &PPU, cartridge: &Cartridge, scanline: u8) -> ([u8; 256], [bool; 256]) {
    let sprite_oam = sprite_evaluation(ppu, scanline);

    let mut pixels = [0; 256];
    let mut priority = [false; 256];

    // Process sprites in reverse order, because the earlier entries should overwrite.
    for sprite in sprite_oam.iter().rev() {
        let sprite_pixels = get_sprite_data(ppu, cartridge, scanline, sprite);
        let sprite_priority = (sprite.attributes & 0b0010_0000) != 0;

        for i in 0..7 {
            let x = sprite.x as usize + i;
            if x < 256 {
                pixels[x] = sprite_pixels[i];
                priority[x] = sprite_priority;
            }
        }
    }

    (pixels, priority)
}

fn get_tall_sprite_data(ppu: &PPU, cartridge: &Cartridge, scanline: u8, sprite: &OAMEntry) -> Vec<u8> {
    // The table base of a tall sprite is determined by bit 0 of the sprite's tile number.
    // If bit 0 is set, the table base is 0x1000. Else it's 0x0000.
    let table_base = if sprite.tile & 0b1 != 0 { 0x1000 } else { 0x0000 };

    let mut y_offset = (scanline - sprite.y) as u16;
    // Flip the sprite vertically if bit 7 of the attributes is set.
    // A y offset of 15 becomes 0, and vice versa. Also 1 <-> 14, 2 <-> 13, etc.
    if sprite.attributes & 0b1000_0000 != 0 {
        y_offset = 15 - y_offset;
    }

    // We either want the top or bottom tile for this sprite, based on the y_offset.
    let tile_addr = ((sprite.tile & 0b1111_1110) as u16 | if y_offset >= 8 { 0b1 } else { 0 }) << 4;

    // As with 8x8 sprites, the only difference here is bit 3.
    let lo = read_internal(ppu, cartridge, table_base | tile_addr | y_offset % 8);
    let hi = read_internal(ppu, cartridge, table_base | tile_addr | 0b1000 | y_offset % 8);

    // Merge low and high bytes of the sprite data to get palette indices
    // The sprite may be mirrored horizontally, based on bit 6 of the sprite attributes.
    let flip_x = sprite.attributes & 0b0100_0000 != 0;
    let palette_indices = merge_bytes(lo, hi, flip_x);

    // Retrieve the relevant palette from the PPU palette table
    // Sprite palettes are numbered 4 - 7.
    let palette_num = (sprite.attributes & 0b0000_0011) + 4;
    let palette = get_palette(ppu, palette_num);

    palette_indices.map(|i| palette[i as usize]).collect()
}

fn get_tall_sprite_line(ppu: &PPU, cartridge: &Cartridge, scanline: u8) -> ([u8; 256], [bool; 256]) {
    let sprite_oam = sprite_evaluation(ppu, scanline);

    let mut pixels = [0; 256];
    let mut priority = [false; 256];

    // Process sprites in reverse order, because the earlier entries should overwrite.
    for sprite in sprite_oam.iter().rev() {
        let sprite_pixels = get_tall_sprite_data(ppu, cartridge, scanline, sprite);
        let sprite_priority = (sprite.attributes & 0b0010_0000) != 0;

        for i in 0..7 {
            let x = sprite.x as usize + i;
            if x < 256 {
                pixels[x] = sprite_pixels[i];
                priority[x] = sprite_priority;
            }
        }
    }

    (pixels, priority)
}

fn get_background_palette_idx(ppu: &PPU, tile_x: u8, tile_y: u8) -> u8 {
    let attr_tbl_idx = (tile_x / 4) * 8 + (tile_y / 4);
    // The attribute table is normally at 0x23C0, 0x27C0... but we 
    // are addressing directly into vram, which starts at 0x2000.
    let attribute_base = match ppu.control.bits & 0b11 {
        0 => 0x03C0,
        1 => 0x07C0,
        2 => 0x0BC0,
        3 => 0x0FC0,
        _ => panic!("invalid attribute table")
    };
    let attr_byte = ppu.vram[attribute_base + attr_tbl_idx as usize];

    match (tile_x % 4 / 2, tile_y % 4 / 2) {
        (0,0) => attr_byte & 0b11,
        (1,0) => (attr_byte >> 2) & 0b11,
        (0,1) => (attr_byte >> 4) & 0b11,
        (1,1) => (attr_byte >> 6) & 0b11,
        (_,_) => panic!("Invalid background palette ID found in attribute table")
    }
}

fn get_background_line(ppu: &PPU, cartridge: &Cartridge, scanline: u8) -> [u8; 256] {
    let nametable_address = match ppu.control.bits & 0b11 {
        0 => 0x2000,
        1 => 0x2400,
        2 => 0x2800,
        3 => 0x2C00,
        _ => panic!("invalid base nametable address")
    };
    let mut result = [0; 256];

    let row_start = (scanline as u16 / 8) * 32;
    let y_offset = (scanline % 8) as u16;

    let tile_y = scanline / 8;

    let pattern_table_addr = if ppu.control.bits & 0b0001_0000 != 0 { 0x1000 } else { 0x0000 };

    for tile_x in 0u8..32 {
        let mirrored_address = mirror_vram_address(nametable_address + row_start + tile_x as u16, cartridge.get_mirroring());
        let nametable_entry = (ppu.vram[mirrored_address as usize] as u16) << 4;
        let lo = cartridge.chr_read(pattern_table_addr + nametable_entry + y_offset);
        let hi = cartridge.chr_read(pattern_table_addr + nametable_entry + y_offset + 8);

        let bg_palette = get_palette(ppu, get_background_palette_idx(ppu, tile_x, tile_y));
        let palette_indices = merge_bytes(lo, hi, false);

        let pixels = palette_indices.map(|i| bg_palette[i as usize]);

        (0..8).zip(pixels)
            .for_each(|(i, p)| result[(tile_x as usize * 8) + i] = p)
    }

    result
}

fn render_scanline(ppu: &PPU, cartridge: &Cartridge, frame: &mut FrameBuffer, scanline: u8) {
    // Get the sprite data and background priority for the line. The background priority 
    // determines if an opaque bg pixel will overwrite an opaque sprite pixel.
    let (sprite_data, bg_priority) = match ppu.control.contains(PpuControl::SPRITE_SIZE) {
        true => get_tall_sprite_line(ppu, cartridge, scanline),
        false => get_sprite_line(ppu, cartridge, scanline),
    };

    // Grab the background data
    let background_data = get_background_line(ppu, cartridge, scanline);

    // For each pixel, either it will be the sprite data or the background data (or nothing?)
    let line_data = sprite_data.into_iter()
        .zip(background_data.into_iter())
        .zip(bg_priority.into_iter())
        .map(|((s, b), p)| if p { s } else { b });

    // Write the data to the FrameBuffer
    frame.set_line(scanline, line_data)
}


pub fn step_ppu(ppu: &mut PPU, cartridge: &Cartridge, frame: &mut FrameBuffer, cycles: u16) {
    ppu.scanline_cycle += cycles;
    // The PPU takes 340 of its cycles (not CPU cycles!) to finish rendering a line
    if ppu.scanline_cycle > 340 {
        // The first 240 lines are visible, the rest are VBLANK lines
        if ppu.current_scanline <= 239 {
            render_scanline(ppu, cartridge, frame, ppu.current_scanline as u8);
        }
        ppu.scanline_cycle -= 340;
        ppu.current_scanline += 1;

        // This is the start of the VBLANK period
        if(ppu.current_scanline) == 241 {
            ppu.vblank = true;
            // TODO: Send VBLANK NMI
        }

        // After line 260 wrap back around to 0
        if ppu.current_scanline == 261 {
            ppu.vblank = false;
            ppu.current_scanline = 0;
        }
    }
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