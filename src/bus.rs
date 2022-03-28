use crate::{cpu::Mem, cart::{Cartridge, CartMem}, ppu::PPU};

const RAM_START: u16 =      0x0000;
const RAM_END: u16 =        0x1FFF;

const PPU_REGISTER_START: u16 = 0x2000;
const PPU_REGISTER_END: u16 =   0x2007;

const PPU_OAM: u16 = 0x4014;

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
            PPU_REGISTER_START..=PPU_REGISTER_END => self.ppu.mem_read(addr, &self.cartridge),
            PRG_ROM_START..=PRG_ROM_END => self.cartridge.mem_read(addr),
            _ => panic!("Attempted to read from unknown address")
        }
    }

    fn mem_write(&mut self, addr: u16, value: u8) {
        match addr {
            RAM_START..=RAM_END => self.cpu_vram[(addr & 0x7FF) as usize] = value,
            PPU_REGISTER_START..=PPU_REGISTER_END => self.ppu.mem_write(addr, value, &mut self.cartridge),
            PRG_ROM_START..=PRG_ROM_END => self.cartridge.mem_write(addr, value),
            PPU_OAM => self.push_oam(value),
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

    fn push_oam(&mut self, value: u8) {
        let mut data: [u8; 256] = [0; 256];
        let hi = (value as u16) << 8;
        for i in 0..256 {
            data[i as usize] = self.mem_read(hi + i);
        }
        self.ppu.oam_dma(&data);
    }

    pub fn load_cartridge(&mut self, cart: Cartridge) {
        self.cartridge = cart;
    }
}