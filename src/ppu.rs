use crate::{cart::{Mirroring, Cartridge}, cpu::Mem};

pub struct PPU {
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
    pub fn new() -> Self {
        PPU { 
            palette_table: [0; 32],
            vram: [0; 2048],
            oam: [0; 256],
        }
    }

    pub fn tick(&mut self, cycles: u8, cart: &Cartridge) {

    }
}