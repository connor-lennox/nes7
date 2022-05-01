use std::{env, fs};
use nes7_core::{cpu::{CPU, step_cpu, reset_cpu_from_cart}, ppu::{PPU, FrameBuffer, step_ppu}, cart};
use resources::Resources;

fn main() {
    let mut cpu = CPU::default();
    let ppu = PPU::default();
    let frame = FrameBuffer::new();

    let args: Vec<String> = env::args().collect();
    let cart_path = &args[1];
    let cart_data = fs::read(cart_path).unwrap();
    let cartridge = cart::from_binary(&cart_data).unwrap();

    reset_cpu_from_cart(&mut cpu, &cartridge);

    let mut resources = Resources::new();
    resources.insert(ppu);
    resources.insert(cartridge);
    resources.insert(frame);
    let resources = resources;

    loop {
        let cycle_count = step_cpu(&mut cpu, &resources);
        step_ppu(&resources, (cycle_count as u16) * 3);
    }
}
