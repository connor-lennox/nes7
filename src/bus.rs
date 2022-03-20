use crate::{cpu::Mem, cart::Cartridge};

const RAM_START: u16 =      0x0000;
const RAM_END: u16 =        0x1FFF;

const PRG_ROM_START: u16 =  0x8000;
const PRG_ROM_END: u16 =    0xFFFF;

pub struct Bus {
    cpu_vram: [u8; 2048],
    cartridge: Cartridge
}

impl Mem for Bus {
    fn mem_read(&self, addr: u16) -> u8 {
        match addr {
            RAM_START..=RAM_END => self.cpu_vram[(addr & 0x7FF) as usize],
            PRG_ROM_START..=PRG_ROM_END => self.cartridge.mem_read(addr),
            _ => panic!("Attempted to read from unknown address")
        }
    }

    fn mem_write(&mut self, addr: u16, value: u8) {
        match addr {
            RAM_START..=RAM_END => self.cpu_vram[(addr & 0x7FF) as usize] = value,
            PRG_ROM_START..=PRG_ROM_END => self.cartridge.mem_write(addr, value),
            _ => panic!("Attempted to write to unknown address")
        }
    }
}

impl Bus {
    pub fn new(cart: Cartridge) -> Self {
        Bus {
            cpu_vram: [0; 2048],
            cartridge: cart,
        }
    }
}