use std::{env, fs};
use nes7::{cpu::{CPU, step_cpu, reset_cpu_from_cart}, ppu::{PPU, FrameBuffer, step_ppu}, cart};

fn main() {
    let mut cpu = CPU::default();
    let mut ppu = PPU::default();
    let mut frame = FrameBuffer::new();

    let args: Vec<String> = env::args().collect();
    let cart_path = &args[1];
    let cart_data = fs::read(cart_path).unwrap();
    let mut cartridge = cart::from_binary(&cart_data).unwrap();

    reset_cpu_from_cart(&mut cpu, &cartridge);

    loop {
        let cycle_count = step_cpu(&mut cpu, &mut ppu, &mut cartridge);
        step_ppu(&mut ppu, &mut cartridge, &mut frame, (cycle_count as u16) * 3);
    }
}
