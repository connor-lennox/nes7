use std::{env, fs};

use bevy;
use bevy::prelude::*;
use bevy_pixels::prelude::*;
use nes7_core::{cpu::{CPU, step_cpu, reset_cpu_from_cart}, ppu::{PPU, FrameBuffer, step_ppu}, cart::{self, Cartridge}};


const WIDTH: u32 = 256;
const HEIGHT: u32 = 240;

fn main() {
    App::new()
        .insert_resource(WindowDescriptor {
            title: "Nes7".to_string(),
            width: (WIDTH * 3) as f32,
            height: (HEIGHT * 3) as f32,
            ..Default::default()
        })
        .insert_resource(PixelsOptions {
            width: WIDTH,
            height: HEIGHT,
        })
        .add_plugins(DefaultPlugins)
        .add_plugin(PixelsPlugin)
        .add_plugin(NESPlugin)
        .run();
}

struct FrameTimer(Timer);

pub struct NESPlugin;

impl Plugin for NESPlugin {
    fn build(&self, app: &mut App) {
        let args: Vec<String> = env::args().collect();
        let cart_path = &args[1];
        let cart_data = fs::read(cart_path).unwrap();
        let cartridge = cart::from_binary(&cart_data).unwrap();

        app.insert_resource(CPU::default())
            .insert_resource(PPU::default())
            .insert_resource(FrameBuffer::new())
            .insert_resource(cartridge)
            .insert_resource(FrameTimer(Timer::from_seconds(1f32 / 60f32, true)))
            .add_startup_system(startup_nes_system)
            .add_system(step_nes_system);
    }
}


fn startup_nes_system(mut cpu: ResMut<CPU>, cart: Res<Cartridge>) {
    reset_cpu_from_cart(&mut cpu, &cart);
}


fn step_nes_system(time: Res<Time>, mut timer: ResMut<FrameTimer>, 
                    mut cpu: ResMut<CPU>, mut ppu: ResMut<PPU>, 
                    mut cart: ResMut<Cartridge>, mut frame: ResMut<FrameBuffer>,
                    mut pixels: ResMut<PixelsResource>) {
    
    if timer.0.tick(time.delta()).just_finished() {
        // Need to calculate a new frame
        let mut budget = 29780;
        while budget > 0 {
            // Step components based on how long the CPU operation takes
            let cycles = step_cpu(&mut cpu, &mut ppu, &mut cart);
            step_ppu(&mut ppu, &cart, &mut frame, (cycles as u16) * 3);

            // Deduct from our cpu cycle budget for the frame
            budget -= cycles as i32;
        }

        // Update display
        let pixel_data: Vec<u8> = frame.pixels.iter()
                                    .map(|p| PALETTE[*p as usize])
                                    .flat_map(|tup| [tup.0, tup.1, tup.2, tup.3])
                                    .collect();

        pixels.pixels.get_frame().copy_from_slice(&pixel_data);
    }
}

static PALETTE: [(u8, u8, u8, u8); 56] = [
    (84, 84, 84, 255), (0, 30, 116, 255), (8, 16, 144, 255), (48, 0, 136, 255), (68, 0, 100, 255), (92, 0, 48, 255), (84, 4, 0, 255), (60, 24, 0, 255), (32, 42, 0, 255), (8, 58, 0, 255), (0, 64, 0, 255), (0, 60, 0, 255), (0, 50, 60, 255), (0, 0, 0, 255), (152, 150, 152, 255), (8, 76, 196, 255), 
    (48, 50, 236, 255), (92, 30, 228, 255), (136, 20, 176, 255), (160, 20, 100, 255), (152, 34, 32, 255), (120, 60, 0, 255), (84, 90, 0, 255), (40, 114, 0, 255), (8, 124, 0, 255), (0, 118, 40, 255), (0, 102, 120, 255), (0, 0, 0, 255), (236, 238, 236, 255), (76, 154, 236, 255), (120, 124, 236, 255), (176, 98, 236, 255), 
    (228, 84, 236, 255), (236, 88, 180, 255), (236, 106, 100, 255), (212, 136, 32, 255), (160, 170, 0, 255), (116, 196, 0, 255), (76, 208, 32, 255), (56, 204, 108, 255), (56, 180, 204, 255), (60, 60, 60, 255), (236, 238, 236, 255), (168, 204, 236, 255), (188, 188, 236, 255), (212, 178, 236, 255), (236, 174, 236, 255), (236, 174, 212, 255), 
    (236, 180, 176, 255), (228, 196, 144, 255), (204, 210, 120, 255), (180, 222, 120, 255), (168, 226, 144, 255), (152, 226, 180, 255), (160, 214, 228, 255), (160, 162, 160, 255)
];
