use crate::opcodes::{self, Op, OpWithMode, Opcode, OPCODE_MAP};
use crate::bus::{self, Bus};

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPage_X,
    ZeroPage_Y,
    Absolute,
    Absolute_X,
    Absolute_Y,
    Indirect_X,
    Indirect_Y,
    Accumulator,
}

pub trait Mem {
    fn mem_read(&self, addr: u16) -> u8;
    fn mem_write(&mut self, addr: u16, value: u8);

    fn mem_read_u16(&self, addr: u16) -> u16 {
        // The NES packs 16-bit values in little endian
        let lo = self.mem_read(addr) as u16;
        let hi = self.mem_read(addr + 1) as u16;
        (hi << 8) | (lo)
    }

    fn mem_write_u16(&mut self, addr: u16, value: u16) {
        let lo = (value & 0xff) as u8;
        let hi = (value >> 8) as u8;
        self.mem_write(addr, lo);
        self.mem_write(addr + 1, hi);
    }
}


pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,

    pub status: u8,
    pub program_counter: u16,

    pub bus: Bus,
}

impl Mem for CPU {
    fn mem_read(&self, addr: u16) -> u8 {
        self.bus.mem_read(addr)
    }

    fn mem_write(&mut self, addr: u16, value: u8) {
        self.bus.mem_write(addr, value)
    }
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: 0,
            program_counter: 0,
            bus: Bus::new(),
        }
    }

    fn lda(&mut self, value: u8) {
        self.register_a = value;
        self.update_zero_negative_flags(value)
    }

    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_negative_flags(self.register_x);
    }

    fn inx(&mut self) {
        self.register_x = self.register_x.overflowing_add(1).0;
        self.update_zero_negative_flags(self.register_x);
    }

    fn iny(&mut self) {
        self.register_y = self.register_y.overflowing_add(1).0;
        self.update_zero_negative_flags(self.register_y);
    }

    fn update_zero_negative_flags(&mut self, result: u8) {
        // Update zero flag
        if result == 0 {
            self.status = self.status | 0b0000_0010;
        } else {
            self.status = self.status & 0b1111_1101;
        }

        // Update negative flag
        if result & 0b1000_0000 != 0 {
            self.status = self.status | 0b1000_0000;
        } else {
            self.status = self.status & 0b0111_1111;
        }
    }

    pub fn interpret_op(&mut self, opcode: &Op) {
        match opcode {
            Op::BCC => todo!(),
            Op::BCS => todo!(),
            Op::BEQ => todo!(),
            Op::BMI => todo!(),
            Op::BNE => todo!(),
            Op::BPL => todo!(),
            Op::BRK => (), //TODO: push PC+2, set I flag, push SR
            Op::BVC => todo!(),
            Op::BVS => todo!(),
            Op::CLC => todo!(),
            Op::CLD => todo!(),
            Op::CLI => todo!(),
            Op::CLV => todo!(),
            Op::DEX => todo!(),
            Op::DEY => todo!(),
            Op::INX => self.inx(),
            Op::INY => self.iny(),
            Op::JSR => todo!(),
            Op::NOP => todo!(),
            Op::PHA => todo!(),
            Op::PHP => todo!(),
            Op::PLA => todo!(),
            Op::PLP => todo!(),
            Op::RTI => todo!(),
            Op::RTS => todo!(),
            Op::SEC => todo!(),
            Op::SED => todo!(),
            Op::SEI => todo!(),
            Op::TAX => self.tax(),
            Op::TAY => todo!(),
            Op::TSX => todo!(),
            Op::TXA => todo!(),
            Op::TXS => todo!(),
            Op::TYA => todo!(),
        }
    }

    pub fn interpret_op_with_mode(&mut self, opcode: &OpWithMode, mode: &AddressingMode) {
        match opcode {
            OpWithMode::ADC => todo!(),
            OpWithMode::AND => todo!(),
            OpWithMode::ASL => todo!(),
            OpWithMode::BIT => todo!(),
            OpWithMode::CMP => todo!(),
            OpWithMode::CPX => todo!(),
            OpWithMode::CPY => todo!(),
            OpWithMode::DEC => todo!(),
            OpWithMode::EOR => todo!(),
            OpWithMode::INC => todo!(),
            OpWithMode::JMP => todo!(),
            OpWithMode::LDA => {
                let value = self.mem_read(self.program_counter);
                // TODO: other addressing modes
                self.program_counter += 1;
                self.lda(value);
            },
            OpWithMode::LDX => todo!(),
            OpWithMode::LDY => todo!(),
            OpWithMode::LSR => todo!(),
            OpWithMode::ORA => todo!(),
            OpWithMode::ROL => todo!(),
            OpWithMode::ROR => todo!(),
            OpWithMode::SBC => todo!(),
            OpWithMode::STA => todo!(),
            OpWithMode::STX => todo!(),
            OpWithMode::STY => todo!(),
        }
    }

    fn interpret(&mut self, opcode: &Opcode) -> u8 {
        match opcode {
            Opcode::Op { op, code: _, len: _, cycles } => {
                self.interpret_op(op);
                return *cycles;
            },
            Opcode::OpWithMode { op, code: _, len: _, cycles, mode } => {
                self.interpret_op_with_mode(op, mode);
                return *cycles;
            },
        }
    }

    pub fn run(&mut self, program: Vec<u8>) {
        self.program_counter = 0;

        // Load the program into memory
        for (i, x) in program.iter().enumerate() {
            self.mem_write(i as u16, *x);
        }

        // Execution loop
        loop {
            let code = self.mem_read(self.program_counter);
            self.program_counter += 1;
            let opcode = OPCODE_MAP.get(&code).expect(&format!("Unknown opcode {:x}", code));

            self.interpret(opcode);

            // Terminate if given a BRK opcode
            if code == 0x00 {
                return;
            }
        }
    }
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.run(vec![0xa9, 0x05, 0x00]);
        assert_eq!(cpu.register_a, 0x05);
        assert!(cpu.status & 0b0000_0010 == 0b00);
        assert!(cpu.status & 0b1000_0000 == 0)
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.run(vec![0xa9, 0x00, 0x00]);
        assert!(cpu.status & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = CPU::new();
        cpu.register_a = 10;
        cpu.run(vec![0xaa, 0x00]);
  
        assert_eq!(cpu.register_x, 10)
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();
        cpu.run(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);
  
        assert_eq!(cpu.register_x, 0xc1)
    }
 
     #[test]
     fn test_inx_overflow() {
         let mut cpu = CPU::new();
         cpu.register_x = 0xff;
         cpu.run(vec![0xe8, 0xe8, 0x00]);
 
         assert_eq!(cpu.register_x, 1)
     }
}