use crate::cpu::Mem;

pub struct Bus {
    cpu_vram: [u8; 2048]
}

impl Mem for Bus {
    fn mem_read(&self, addr: u16) -> u8 {
        self.cpu_vram[addr as usize]
    }

    fn mem_write(&mut self, addr: u16, value: u8) {
        self.cpu_vram[addr as usize] = value;
    }
}

impl Bus {
    pub fn new() -> Self {
        Bus {
            cpu_vram: [0; 2048]
        }
    }
}