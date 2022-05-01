use bitflags::bitflags;

bitflags! {
    #[derive(Default)]
    pub struct JoypadButtons : u8 {
        const A =       0b0000_0001;
        const B =       0b0000_0010;
        const SELECT =  0b0000_0100;
        const START =   0b0000_1000;
        const UP =      0b0001_0000;
        const DOWN =    0b0010_0000;
        const LEFT =    0b0100_0000;
        const RIGHT =   0b1000_0000;
    }
}

#[derive(Default)]
pub struct Joypad {
    pub strobe: bool,
    pub button_idx: u8,
    pub status: JoypadButtons,
}

impl Joypad {
    pub fn write(&mut self, data: u8) {
        self.strobe = data & 1 == 1;
        if self.strobe {
            self.button_idx = 0;
        }
    }

    pub fn read(&mut self) -> u8 {
        if self.button_idx > 7 {
            return 1;
        }
        let response = (self.status.bits & (1 << self.button_idx)) >> self.button_idx;
        if !self.strobe && self.button_idx <= 7 {
            self.button_idx += 1;
        } 
        response
    }

    pub fn press(&mut self, button: JoypadButtons) {
        self.status.insert(button);
    }

    pub fn release(&mut self, button: JoypadButtons) {
        self.status.remove(button);
    }
}