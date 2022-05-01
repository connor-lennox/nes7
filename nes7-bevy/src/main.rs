use std::{env, fs};

use bevy;
use bevy::prelude::*;
use bevy_pixels::prelude::*;
use nes7_core::{cpu::{CPU, step_cpu, reset_cpu_from_cart}, ppu::{PPU, FrameBuffer, step_ppu}, cart::{self, Cartridge}, joypad::{Joypad, JoypadButtons}};
use resources::Resources;


const WIDTH: u32 = 256;
const HEIGHT: u32 = 240;

// Keys for input, in order [A, B, Select, Start, Up, Down, Left, Right]
const KEYMAP: [KeyCode; 8] = [KeyCode::Z, KeyCode::X, KeyCode::RShift, KeyCode::Return, KeyCode::Up, KeyCode::Down, KeyCode::Left, KeyCode::Right];

fn main() {
    App::new()
        .insert_resource(WindowDescriptor {
            title: "Nes7".to_string(),
            width: (WIDTH * 3) as f32,
            height: (HEIGHT * 3) as f32,
            scale_factor_override: Some(1f64),
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

        let ppu = PPU::default();
        let joypad = Joypad::default();
        let frame = FrameBuffer::new();

        let mut resources = Resources::new();
        resources.insert(cartridge);
        resources.insert(ppu);
        resources.insert(joypad);
        resources.insert(frame);

        app.insert_resource(CPU::default())
            .insert_resource(resources)
            .insert_resource(FrameTimer(Timer::from_seconds(1f32 / 60f32, true)))
            .add_startup_system(startup_nes_system)
            .add_system(step_nes_system)
            .add_system(keyboard_input);
    }
}


fn startup_nes_system(mut cpu: ResMut<CPU>, components: Res<Resources>) {
    reset_cpu_from_cart(&mut cpu, &components.get::<Cartridge>().unwrap());
}


fn step_nes_system(time: Res<Time>, mut timer: ResMut<FrameTimer>, 
                    mut cpu: ResMut<CPU>, components: Res<Resources>,
                    mut pixels: ResMut<PixelsResource>) {
    
    if timer.0.tick(time.delta()).just_finished() {
        // Need to calculate a new frame
        let mut budget = 29780;
        while budget > 0 {
            // Step components based on how long the CPU operation takes
            let cycles = step_cpu(&mut cpu, &components);
            step_ppu(&components, (cycles as u16) * 3);

            // Deduct from our cpu cycle budget for the frame
            budget -= cycles as i32;
        }

        // Update display
        let frame = &components.get::<FrameBuffer>().unwrap();
        let pixel_data: Vec<u8> = frame.pixels.iter()
                                    .map(|p| PALETTE[*p as usize])
                                    .flat_map(|tup| [tup.0, tup.1, tup.2, 255])
                                    .collect();

        pixels.pixels.get_frame().copy_from_slice(&pixel_data);
    }
}

fn keyboard_input(components: Res<Resources>, keys: Res<Input<KeyCode>>) {
    let joypad = &mut components.get_mut::<Joypad>().unwrap();
    joypad.status = JoypadButtons::from_bits_truncate(
        KEYMAP.iter().fold(0, |acc, e| {let t = acc >> 1; t | if keys.pressed(*e) {0b1000_0000} else {0}})
    );
}

static PALETTE: [(u8, u8, u8); 64] = [
    (84, 84, 84), (0, 30, 116), (8, 16, 144), (48, 0, 136), (68, 0, 100), (92, 0, 48), (84, 4, 0), (60, 24, 0), (32, 42, 0), (8, 58, 0), (0, 64, 0), (0, 60, 0), (0, 50, 60), (0, 0, 0), (0, 0, 0), (0, 0, 0), 
    (152, 150, 152), (8, 76, 196), (48, 50, 236), (92, 30, 228), (136, 20, 176), (160, 20, 100), (152, 34, 32), (120, 60, 0), (84, 90, 0), (40, 114, 0), (8, 124, 0), (0, 118, 40), (0, 102, 120), (0, 0, 0), (0, 0, 0), (0, 0, 0),
    (236, 238, 236), (76, 154, 236), (120, 124, 236), (176, 98, 236), (228, 84, 236), (236, 88, 180), (236, 106, 100), (212, 136, 32), (160, 170, 0), (116, 196, 0), (76, 208, 32), (56, 204, 108), (56, 180, 204), (60, 60, 60), (0, 0, 0), (0, 0, 0),
    (236, 238, 236), (168, 204, 236), (188, 188, 236), (212, 178, 236), (236, 174, 236), (236, 174, 212), (236, 180, 176), (228, 196, 144), (204, 210, 120), (180, 222, 120), (168, 226, 144), (152, 226, 180), (160, 214, 228), (160, 162, 160), (0, 0, 0), (0, 0, 0)
];
