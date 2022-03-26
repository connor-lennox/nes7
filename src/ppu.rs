use crate::{cart::Mirroring, cpu::Mem};

pub struct PPU {
    pub chr_rom: Vec<u8>,
    pub mirroring: Mirroring,
    pub palette_table: [u8; 32],
    pub vram: [u8; 2048],
    pub oam: [u8; 256],
}


impl Mem for PPU {
    fn mem_read(&self, addr: u16) -> u8 {
        todo!()
    }

    fn mem_write(&mut self, addr: u16, value: u8) {
        todo!()
    }
}

impl PPU {
    pub fn new(chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
        PPU { 
            chr_rom,
            mirroring,
            palette_table: [0; 32],
            vram: [0; 2048],
            oam: [0; 256],
        }
    }
}