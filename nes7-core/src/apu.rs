#[derive(Default)]
pub struct Pulse {
    pub registers: [u8; 4]
}

impl Pulse {
    pub fn duty(&self) -> u8 { self.registers[0] >> 6 }
    pub fn envelope_loop(&self) -> u8 { self.registers[0] >> 5 & 0b1 }
    pub fn constant_volume(&self) -> u8 { self.registers[0] >> 4 & 0b1 }
    pub fn envelope_volume(&self) -> u8 { self.registers[0] & 0b1111 }
    pub fn sweep_enabled(&self) -> u8 { self.registers[1] >> 7 }
    pub fn period(&self) -> u8 { self.registers[1] >> 4 & 0b111 }
    pub fn negate(&self) -> u8 { self.registers[1] >> 3 & 0b1 }
    pub fn shift(&self) -> u8 { self.registers[1] & 0b111 }
    pub fn timer_low(&self) -> u8 { self.registers[2] }
    pub fn length_counter(&self) -> u8 { self.registers[3] >> 3 }
    pub fn timer_high(&self) -> u8 { self.registers[3] & 0b111 }
}


#[derive(Default)]
pub struct Triangle {
    pub registers: [u8; 4]
}

impl Triangle {
    pub fn length_counter_halt(&self) -> u8 { self.registers[0] >> 7 }
    pub fn linear_counter(&self) -> u8 { self.registers[0] & 0b111_1111 }
    pub fn timer_low(&self) -> u8 { self.registers[2] }
    pub fn length_counter(&self) -> u8 { self.registers[3] >> 3 }
    pub fn timer_high(&self) -> u8 { self.registers[3] & 0b111 }
}

#[derive(Default)]
pub struct Noise {
    pub registers: [u8; 4]
}

impl Noise {
    pub fn envelope_loop(&self) -> u8 { self.registers[0] >> 5 & 0b1 }
    pub fn constant_volume(&self) -> u8 { self.registers[0] >> 4 & 0b1 }
    pub fn envelope_volume(&self) -> u8 { self.registers[0] & 0b1111 }
    pub fn loop_noise(&self) -> u8 { self.registers[2] >> 7 }
    pub fn noise_period(&self) -> u8 { self.registers[2] & 0b1111 }
    pub fn length_counter(&self) -> u8 { self.registers[3] >> 3 }
}


#[derive(Default)]
pub struct DMC {
    pub registers: [u8; 4]
}

impl DMC {
    pub fn irq_enable(&self) -> u8 { self.registers[0] >> 7 }
    pub fn loop_flag(&self) -> u8 { self.registers[0] >> 6 & 0b1 }
    pub fn frequency(&self) -> u8 { self.registers[0] & 0b1111 }
    pub fn load_counter(&self) -> u8 { self.registers[1] & 0b111_1111 }
    pub fn sample_address(&self) -> u8 { self.registers[2] }
    pub fn sample_length(&self) -> u8 { self.registers[3] }
}


#[derive(Default)]
pub struct APU {
    pub pulse1: Pulse,
    pub pulse2: Pulse,
    pub triangle: Triangle,
    pub noise: Noise,
    pub dmc: DMC,

    pub status: u8,
    pub frame_counter: u8,
}