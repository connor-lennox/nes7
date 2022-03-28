use crate::{cpu::Mem, cart::{Cartridge, CartMem}, ppu::PPU};

const RAM_START: u16 =      0x0000;
const RAM_END: u16 =        0x1FFF;

const PPU_REGISTER_START: u16 = 0x2000;
const PPU_REGISTER_END: u16 =   0x2007;

const PRG_ROM_START: u16 =  0x8000;
const PRG_ROM_END: u16 =    0xFFFF;

pub struct Bus {
    cpu_vram: [u8; 2048],
    cartridge: Cartridge,
    ppu: PPU,
}

impl Mem for Bus {
    fn mem_read(&mut self, addr: u16) -> u8 {
        match addr {
            RAM_START..=RAM_END => self.cpu_vram[(addr & 0x7FF) as usize],
            PPU_REGISTER_START..=PPU_REGISTER_END => self.ppu.mem_read(addr),
            PRG_ROM_START..=PRG_ROM_END => self.cartridge.mem_read(addr),
            _ => panic!("Attempted to read from unknown address")
        }
    }

    fn mem_write(&mut self, addr: u16, value: u8) {
        match addr {
            RAM_START..=RAM_END => self.cpu_vram[(addr & 0x7FF) as usize] = value,
            PPU_REGISTER_START..=PPU_REGISTER_END => self.ppu.mem_write(addr, value),
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
            ppu: PPU::new(),
        }
    }

    pub fn load_cartridge(&mut self, cart: Cartridge) {
        self.cartridge = cart;
        // TODO: Push chr rom to ppu?
    }
}