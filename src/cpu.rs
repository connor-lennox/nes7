use crate::cart::Cartridge;
use crate::opcodes::{Op, OpWithMode, Opcode, OPCODE_MAP};
use crate::bus::Bus;
use bitflags::bitflags;
use enum_dispatch::enum_dispatch;

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
    Indirect,
    Indirect_X,
    Indirect_Y,
    Accumulator,
}

#[enum_dispatch]
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

bitflags! {
    // Status register is a series of flags:
    // N V - B D I Z C
    // ^ ^ ^ ^ ^ ^ ^ ^- Carry
    // | | | | | | +--- Zero
    // | | | | | +----- Interrupt (IRQ disable)
    // | | | | +------- Decimal (swaps to BCD mode)
    // | | | +--------- Break
    // | | +----------- Break2
    // | +------------- Overflow
    // +--------------- Negative
    pub struct CpuFlags: u8 {
        const NEGATIVE  = 0b1000_0000;
        const OVERFLOW  = 0b0100_0000;
        const BREAK2    = 0b0010_0000;
        const BREAK     = 0b0001_0000;
        const DECIMAL   = 0b0000_1000;
        const INTERRUPT = 0b0000_0100;
        const ZERO      = 0b0000_0010;
        const CARRY     = 0b0000_0001;
    }
}

pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,

    pub status: CpuFlags,
    pub program_counter: u16,
    pub stack_pointer: u8,

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
    pub fn new(bus: Bus) -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: CpuFlags::empty(),
            program_counter: 0,
            stack_pointer: 0xFF,
            bus,
        }
    }

    fn get_operand_addr(&self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.program_counter,
            AddressingMode::ZeroPage => self.mem_read(self.program_counter) as u16,
            AddressingMode::ZeroPage_X => self.mem_read(self.program_counter).wrapping_add(self.register_x) as u16,
            AddressingMode::ZeroPage_Y => self.mem_read(self.program_counter).wrapping_add(self.register_y) as u16,
            AddressingMode::Absolute => self.mem_read_u16(self.program_counter),
            AddressingMode::Absolute_X => self.mem_read_u16(self.program_counter).wrapping_add(self.register_x as u16),
            AddressingMode::Absolute_Y => self.mem_read_u16(self.program_counter).wrapping_add(self.register_y as u16),
            AddressingMode::Indirect => {
                // Get the bytes at the PC, this is the address of the address we want (two levels of deref).
                let ptr = self.mem_read_u16(self.program_counter);
                self.mem_read_u16(ptr)
            },
            AddressingMode::Indirect_X => {
                let ptr = (self.mem_read(self.program_counter) as u8).wrapping_add(self.register_x);
                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read(ptr.wrapping_add(1) as u16);
                (hi as u16) << 8 | (lo as u16)
            },
            AddressingMode::Indirect_Y => {
                let ptr = self.mem_read(self.program_counter);
                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read(ptr.wrapping_add(1) as u16);
                let deref_addr = (hi as u16) << 8 | (lo as u16);
                deref_addr.wrapping_add(self.register_y as u16)
            },
            AddressingMode::Accumulator => panic!("Cannot get address of accumulator"),
        }
    }

    fn get_operand_addr_and_step(&mut self, mode: &AddressingMode) -> u16 {
        let res = self.get_operand_addr(mode);
        self.program_counter += match mode {
            AddressingMode::Immediate => 1,
            AddressingMode::ZeroPage => 1,
            AddressingMode::ZeroPage_X => 1,
            AddressingMode::ZeroPage_Y => 1,
            AddressingMode::Absolute => 2,
            AddressingMode::Absolute_X => 2,
            AddressingMode::Absolute_Y => 2,
            AddressingMode::Indirect => 2,
            AddressingMode::Indirect_X => 1,
            AddressingMode::Indirect_Y => 1,
            AddressingMode::Accumulator => panic!("Cannot get address of accumulator"),
        };
        res
    }

    fn branch(&mut self, condition: bool) {
        let jump_amt: i8 = self.mem_read(self.program_counter) as i8;
        let jump_addr = self.program_counter.wrapping_add(jump_amt as u16);
        self.program_counter += 1;
        if condition {
            self.program_counter = jump_addr;
        }
    }

    fn push_stack(&mut self, value: u8) {
        // Write the value to the stack position then DECREMENT the stack pointer.
        // The stack is bound to page 1 (0x0100 - 0x01FF)
        self.mem_write(0x0100 | self.stack_pointer as u16, value);
        self.stack_pointer -= 1;
    }

    fn pop_stack(&mut self) -> u8 {
        self.stack_pointer += 1;
        self.mem_read(0x0100 | self.stack_pointer as u16)
    }

    fn push_stack_16(&mut self, value: u16) {
        self.push_stack((value >> 8) as u8);
        self.push_stack((value & 0xff) as u8);
    }

    fn pop_stack_16(&mut self) -> u16 {
        let lo = self.pop_stack() as u16;
        let hi = self.pop_stack() as u16;
        (hi << 8) | lo
    }

    fn adc(&mut self, value: u8) {
        // Sum together the current register_a contents, the input value, and the carry out flag:
        let sum = self.register_a as u16 + value as u16 + (if self.status.contains(CpuFlags::CARRY) {1} else {0});
        // Set carry-out flag if the total sum is greater than 0xff (bit carried out from u8)
        self.status.set(CpuFlags::CARRY, sum > 0xff);
        // Truncate the result to a u8:
        let res = sum as u8;
        // Weird logic for the overflow flag...
        // If bit 7 is different between the (input value, result) AND (result, register_a), overflow occurred
        self.status.set(CpuFlags::OVERFLOW, (value ^ res) & (res ^ self.register_a) & 0x80 != 0);

        // Actually set the register
        self.register_a = res;
        self.update_zero_negative_flags(self.register_a)
    }

    fn and(&mut self, value: u8) {
        self.register_a &= value;
        self.update_zero_negative_flags(self.register_a);
    }

    fn lda(&mut self, value: u8) {
        self.register_a = value;
        self.update_zero_negative_flags(value)
    }

    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_negative_flags(self.register_x);
    }

    fn tay(&mut self) {
        self.register_y = self.register_a;
        self.update_zero_negative_flags(self.register_y);
    }

    fn txa(&mut self) {
        self.register_a = self.register_x;
        self.update_zero_negative_flags(self.register_a);
    }

    fn tya(&mut self) {
        self.register_a = self.register_y;
        self.update_zero_negative_flags(self.register_a);
    }

    fn dex(&mut self) {
        self.register_x = self.register_x.wrapping_sub(1);
        self.update_zero_negative_flags(self.register_x);
    }

    fn dey(&mut self) {
        self.register_y = self.register_y.wrapping_sub(1);
        self.update_zero_negative_flags(self.register_y);
    }

    fn dec(&mut self, addr: u16) {
        let res = self.mem_read(addr).wrapping_sub(1);
        self.mem_write(addr, res);
    }

    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_negative_flags(self.register_x);
    }

    fn iny(&mut self) {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_negative_flags(self.register_y);
    }

    fn inc(&mut self, addr: u16) {
        let res = self.mem_read(addr).wrapping_add(1);
        self.mem_write(addr, res);
    }

    fn jsr(&mut self) {
        // Push the PC + 1, to skip to the first instruction after the jsr
        self.push_stack_16(self.program_counter + 1);
        // Then jump to the address specified by the next two bytes:
        self.program_counter = self.mem_read_u16(self.program_counter);
    }

    fn php(&mut self) {
        // Pushing the status register sets both BREAK flags to 1 (but only in the pushed version)
        let mut new_flags = self.status.clone();
        new_flags.insert(CpuFlags::BREAK);
        new_flags.insert(CpuFlags::BREAK2);
        self.push_stack(new_flags.bits())
    }

    fn rti(&mut self) {
        self.status.bits = self.pop_stack();
        self.status.remove(CpuFlags::BREAK);
        self.status.insert(CpuFlags::BREAK2);

        self.program_counter = self.pop_stack_16();
    }

    fn asl_acc(&mut self) {
        // Set the carry-out flag to bit 7 of the accumulator:
        self.status.set(CpuFlags::CARRY, self.register_a & 0b1000_0000 != 0);
        // Shift the register
        self.register_a = self.register_a << 1;
        self.update_zero_negative_flags(self.register_a);
    }

    fn asl(&mut self, mode: &AddressingMode) {
        match mode {
            AddressingMode::Accumulator => self.asl_acc(),
            _ => {
                let addr = self.get_operand_addr_and_step(mode);
                let v = self.mem_read(addr);
                self.status.set(CpuFlags::CARRY, v & 0b1000_0000 != 0);
                self.mem_write(addr, v << 1);
                self.update_zero_negative_flags(v << 1);
            }
        }
    }

    fn lsr_acc(&mut self) {
        self.status.set(CpuFlags::CARRY, self.register_a & 0b0000_0001 != 0);
        self.register_a = self.register_a >> 1;
        self.update_zero_negative_flags(self.register_a);
    }

    fn lsr(&mut self, mode: &AddressingMode) {
        match mode {
            AddressingMode::Accumulator => self.lsr_acc(),
            _ => {
                let addr = self.get_operand_addr_and_step(mode);
                let v = self.mem_read(addr);
                self.status.set(CpuFlags::CARRY, v & 0b0000_0001 != 0);
                self.mem_write(addr, v >> 1);
                self.update_zero_negative_flags(v >> 1);
            }
        }
    }

    fn rol_acc(&mut self) {
        // Calculate result of rotating in carry flag
        let res = (self.register_a << 1) | (if self.status.contains(CpuFlags::CARRY) {1} else {0});
        // Update carry flag after calculation
        self.status.set(CpuFlags::CARRY, self.register_a & 0b1000_0000 != 0);
        self.register_a = res;
        self.update_zero_negative_flags(self.register_a);
    }

    fn rol(&mut self, mode: &AddressingMode) {
        match mode {
            AddressingMode::Accumulator => self.rol_acc(),
            _ => {
                let addr = self.get_operand_addr_and_step(mode);
                let v = self.mem_read(addr);
                let res = (v << 1) | (if self.status.contains(CpuFlags::CARRY) {1} else {0});
                self.status.set(CpuFlags::CARRY, v & 0b1000_0000 != 0);
                self.mem_write(addr, res);
                self.update_zero_negative_flags(res);
            }
        }
    }

    fn ror_acc(&mut self) {
        // Calculate result of rotating in carry flag
        let res = (self.register_a >> 1) | (if self.status.contains(CpuFlags::CARRY) {0b1000_0000} else {0});
        // Update carry flag after calculation
        self.status.set(CpuFlags::CARRY, self.register_a * 0b0000_0001 != 0);
        self.register_a = res;
        self.update_zero_negative_flags(self.register_a);
    }

    fn ror(&mut self, mode: &AddressingMode) {
        match mode {
            AddressingMode::Accumulator => self.ror_acc(),
            _ => {
                let addr = self.get_operand_addr_and_step(mode);
                let v = self.mem_read(addr);
                let res = (v >> 1) | (if self.status.contains(CpuFlags::CARRY) {0b1000_0000} else {0});
                self.status.set(CpuFlags::CARRY, v & 0b0000_0001 != 0);
                self.mem_write(addr, res);
                self.update_zero_negative_flags(res);
            }
        }
    }

    fn cmp(&mut self, lhs: u8, addr: u16) {
        // LHS is one of register_a, register_x, or register_y.
        // RHS comes from Memory
        let rhs = self.mem_read(addr);
        let res = lhs.wrapping_sub(rhs);
        self.update_zero_negative_flags(res);
        self.status.set(CpuFlags::CARRY, lhs >= rhs);
    }

    fn bit(&mut self, value: u8) {
        // Bit 7 and 6 of value are put into N and V flags.
        // Z flag is set to (register_a AND value)
        self.status.set(CpuFlags::NEGATIVE, value & 0b1000_0000 == 0b1000_0000);
        self.status.set(CpuFlags::OVERFLOW, value & 0b0100_0000 == 0b0100_0000);
        self.status.set(CpuFlags::ZERO, self.register_a & value != 0);
    }

    fn update_zero_negative_flags(&mut self, result: u8) {
        // Update zero flag
        self.status.set(CpuFlags::ZERO, result == 0);

        // Update negative flag
        self.status.set(CpuFlags::NEGATIVE, result & 0b1000_0000 != 0);
    }

    pub fn interpret_op(&mut self, opcode: &Op) {
        match opcode {
            Op::BCC => self.branch(!self.status.contains(CpuFlags::CARRY)),
            Op::BCS => self.branch(self.status.contains(CpuFlags::CARRY)),
            Op::BEQ => self.branch(self.status.contains(CpuFlags::ZERO)),
            Op::BMI => self.branch(self.status.contains(CpuFlags::NEGATIVE)),
            Op::BNE => self.branch(!self.status.contains(CpuFlags::ZERO)),
            Op::BPL => self.branch(!self.status.contains(CpuFlags::NEGATIVE)),
            Op::BRK => (), //TODO: push PC+2, set I flag, push SR
            Op::BVC => self.branch(!self.status.contains(CpuFlags::OVERFLOW)),
            Op::BVS => self.branch(self.status.contains(CpuFlags::OVERFLOW)),
            Op::CLC => self.status.remove(CpuFlags::CARRY),
            Op::CLD => self.status.remove(CpuFlags::DECIMAL),
            Op::CLI => self.status.remove(CpuFlags::INTERRUPT),
            Op::CLV => self.status.remove(CpuFlags::OVERFLOW),
            Op::DEX => self.dex(),
            Op::DEY => self.dey(),
            Op::INX => self.inx(),
            Op::INY => self.iny(),
            Op::JSR => self.jsr(),
            Op::NOP => (),
            Op::PHA => self.push_stack(self.register_a),
            Op::PHP => self.php(),
            Op::PLA => self.register_a = self.pop_stack(),
            Op::PLP => self.status.bits = self.pop_stack() & 0b1110_1111,
            Op::RTI => self.rti(),
            Op::RTS => self.program_counter = self.pop_stack_16() + 1,
            Op::SEC => self.status.insert(CpuFlags::CARRY),
            Op::SED => self.status.insert(CpuFlags::DECIMAL),
            Op::SEI => self.status.insert(CpuFlags::INTERRUPT),
            Op::TAX => self.tax(),
            Op::TAY => self.tay(),
            Op::TSX => self.register_x = self.stack_pointer,
            Op::TXA => self.txa(),
            Op::TXS => self.stack_pointer = self.register_x,
            Op::TYA => self.tya(),
        }
    }

    pub fn interpret_op_with_mode(&mut self, opcode: &OpWithMode, mode: &AddressingMode) {
        match opcode {
            OpWithMode::ADC => {
                let addr = self.get_operand_addr_and_step(mode);
                let value = self.mem_read(addr);
                self.adc(value);
            },
            OpWithMode::AND => {
                let addr = self.get_operand_addr_and_step(mode);
                let value = self.mem_read(addr);
                self.and(value);
            },
            OpWithMode::ASL => self.asl(mode),
            OpWithMode::BIT => {
                let addr = self.get_operand_addr_and_step(mode);
                let value = self.mem_read(addr);
                self.bit(value);
            },
            OpWithMode::CMP => {
                let addr = self.get_operand_addr_and_step(mode);
                self.cmp(self.register_a, addr);
            },
            OpWithMode::CPX => {
                let addr = self.get_operand_addr_and_step(mode);
                self.cmp(self.register_x, addr);
            },
            OpWithMode::CPY => {
                let addr = self.get_operand_addr_and_step(mode);
                self.cmp(self.register_y, addr);
            },
            OpWithMode::DEC => {
                let addr = self.get_operand_addr_and_step(mode);
                self.dec(addr);
            },
            OpWithMode::EOR => {
                let addr = self.get_operand_addr_and_step(mode);
                let value = self.mem_read(addr);
                self.register_a = self.register_a ^ value;
                self.update_zero_negative_flags(self.register_a);
            },
            OpWithMode::INC => {
                let addr = self.get_operand_addr_and_step(mode);
                self.inc(addr);
            },
            OpWithMode::JMP => {
                let addr = self.get_operand_addr_and_step(mode);
                self.program_counter = self.mem_read_u16(addr);
            },
            OpWithMode::LDA => {
                let addr = self.get_operand_addr_and_step(mode);
                let value = self.mem_read(addr);
                self.lda(value);
            },
            OpWithMode::LDX => {
                let addr = self.get_operand_addr_and_step(mode);
                self.register_x = self.mem_read(addr);
                self.update_zero_negative_flags(self.register_x);
            },
            OpWithMode::LDY => {
                let addr = self.get_operand_addr_and_step(mode);
                self.register_y = self.mem_read(addr);
                self.update_zero_negative_flags(self.register_y);
            },
            OpWithMode::LSR => self.lsr(mode),
            OpWithMode::ORA => {
                let addr = self.get_operand_addr_and_step(mode);
                let value = self.mem_read(addr);
                self.register_a = self.register_a | value;
                self.update_zero_negative_flags(self.register_a);
            },
            OpWithMode::ROL => self.rol(mode),
            OpWithMode::ROR => self.ror(mode),
            OpWithMode::SBC => {
                let addr = self.get_operand_addr_and_step(mode);
                let value = self.mem_read(addr);
                // SBC is actually ADC but with 2's complement of the value:
                self.adc(((value as i8).wrapping_neg().wrapping_sub(1)) as u8);
            },
            OpWithMode::STA => {
                let addr = self.get_operand_addr_and_step(mode);
                self.mem_write(addr, self.register_a);
            },
            OpWithMode::STX => {
                let addr = self.get_operand_addr_and_step(mode);
                self.mem_write(addr, self.register_x);
            },
            OpWithMode::STY => {
                let addr = self.get_operand_addr_and_step(mode);
                self.mem_write(addr, self.register_y);
            },
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

    pub fn step(&mut self) -> u8 {
        let code = self.mem_read(self.program_counter);
        self.program_counter += 1;
        let opcode = OPCODE_MAP.get(&code).expect(&format!("Unknown opcode {:x}", code));

        self.interpret(opcode)
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

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.stack_pointer = 0xFD;
        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    pub fn load_cartridge(&mut self, cart: Cartridge) {
        self.bus.load_cartridge(cart);
        self.reset();
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use crate::cart::test;

    fn test_cpu() -> CPU {
        CPU::new(Bus::new(test::test_cart()))
    }

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = test_cpu();
        cpu.run(vec![0xa9, 0x05, 0x00]);
        assert_eq!(cpu.register_a, 0x05);
        assert!(!cpu.status.contains(CpuFlags::ZERO));
        assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = test_cpu();
        cpu.run(vec![0xa9, 0x00, 0x00]);
        assert!(cpu.status.contains(CpuFlags::ZERO));
    }

    #[test]
    fn test_0xa5_lda_zeropage_load() {
        let mut cpu = test_cpu();
        // Do a read at address 0x00, which is 0xa5 (the LDA instruction itself)
        cpu.run(vec![0xa5, 0x00, 0x00]);
        assert_eq!(cpu.register_a, 0xa5);
    }

    #[test]
    fn test_0xb5_lda_zeropagex_load() {
        let mut cpu = test_cpu();
        // Set register_x to 3, then do a zeropage_x read at addr 0x00 (+3), grabbing the 0xfc
        cpu.register_x = 3;
        cpu.run(vec![0xb5, 0x00, 0x00, 0xfc]);
        assert_eq!(cpu.register_a, 0xfc);
    }

    #[test]
    fn test_0xad_lda_absolute_load() {
        let mut cpu = test_cpu();
        // The absolute address is 0x0004 (little endian), grabbing the 0xfc
        cpu.run(vec![0xad, 0x04, 0x00, 0x00, 0xfc]);
        assert_eq!(cpu.register_a, 0xfc);
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = test_cpu();
        cpu.register_a = 10;
        cpu.run(vec![0xaa, 0x00]);
  
        assert_eq!(cpu.register_x, 10);
    }

    #[test]
    fn test_0xa8_tay_move_a_to_y() {
       let mut cpu = test_cpu();
       cpu.register_a = 10;
       cpu.run(vec![0xa8, 0x00]);
       
       assert_eq!(cpu.register_y, 10);
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = test_cpu();
        cpu.run(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);
  
        assert_eq!(cpu.register_x, 0xc1);
    }
 
     #[test]
     fn test_0xe8_inx_overflow() {
         let mut cpu = test_cpu();
         cpu.register_x = 0xff;
         cpu.run(vec![0xe8, 0xe8, 0x00]);
 
         assert_eq!(cpu.register_x, 1);
     }

    #[test]
     fn test_0xc8_iny_overflow() {
         let mut cpu = test_cpu();
         cpu.register_y = 0xff;
         cpu.run(vec![0xc8, 0xc8, 0x00]);

         assert_eq!(cpu.register_y, 1);
     }

     #[test]
     fn test_0xca_dex_underflow() {
         let mut cpu = test_cpu();
         cpu.register_x = 0;
         cpu.run(vec![0xca, 0xca, 0x00]);

         assert_eq!(cpu.register_x, 0xfe);
     }

     #[test]
     fn test_0x88_dey_underflow() {
         let mut cpu = test_cpu();
         cpu.register_y = 0;
         cpu.run(vec![0x88, 0x88, 0x00]);

         assert_eq!(cpu.register_y, 0xfe);
     }

    #[test]
     fn test_0x8a_txa() {
         let mut cpu = test_cpu();
         cpu.register_x = 10;
         cpu.run(vec![0x8a, 0x00]);
         
         assert_eq!(cpu.register_a, 10);
     }

     #[test]
     fn test_0x98_tya() {
         let mut cpu = test_cpu();
         cpu.register_y = 10;
         cpu.run(vec![0x98, 0x00]);

         assert_eq!(cpu.register_a, 10);

         // Extra test for status flags
         let mut cpu2 = test_cpu();
         cpu2.register_y = 0;
         cpu2.run(vec![0x98, 0x00]);

         assert!(cpu2.status.contains(CpuFlags::ZERO));
         assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
     }

     #[test]
     fn test_0x38_sec() {
         let mut cpu = test_cpu();
         cpu.run(vec![0x38, 0x00]);

         assert!(cpu.status.contains(CpuFlags::CARRY));
     }

     #[test]
     fn test_0xf8_sed() {
         let mut cpu = test_cpu();
         cpu.run(vec![0xf8, 0x00]);

         assert!(cpu.status.contains(CpuFlags::DECIMAL));
     }

     #[test]
     fn test_0x78_sei() {
         let mut cpu = test_cpu();
         cpu.run(vec![0x78, 0x00]);

         assert!(cpu.status.contains(CpuFlags::INTERRUPT));
     }

     #[test]
     fn test_0x90_bcc() {
         let mut cpu = test_cpu();
         cpu.status.remove(CpuFlags::CARRY);
         // Branch reads the relative address from 0x03 of 4, causing execution
         // to jump past the BRK and execute the LDA.
         cpu.run(vec![0x90, 0x03, 0x00, 0x02, 0xa9, 0x01, 0x00]);

         assert_eq!(cpu.register_a, 0x01);
     }

     #[test]
     fn test_0xb0_bcs() {
         let mut cpu = test_cpu();
         cpu.status.insert(CpuFlags::CARRY);
         cpu.run(vec![0xb0, 0x03, 0x00, 0x02, 0xa9, 0x01, 0x00]);

         assert_eq!(cpu.register_a, 0x01);
     }

     #[test]
     fn test_0xf0_beq() {
        let mut cpu = test_cpu();
        cpu.status.insert(CpuFlags::ZERO);
        cpu.run(vec![0xf0, 0x03, 0x00, 0x02, 0xa9, 0x01, 0x00]);

        assert_eq!(cpu.register_a, 0x01);
     }

     #[test]
     fn test_0x30_bmi() {
        let mut cpu = test_cpu();
        cpu.status.insert(CpuFlags::NEGATIVE);
        cpu.run(vec![0x30, 0x03, 0x00, 0x02, 0xa9, 0x01, 0x00]);

        assert_eq!(cpu.register_a, 0x01);
     }

     #[test]
     fn test_0xd0_bne() {
        let mut cpu = test_cpu();
        cpu.status.remove(CpuFlags::ZERO);
        cpu.run(vec![0xd0, 0x03, 0x00, 0x02, 0xa9, 0x01, 0x00]);

        assert_eq!(cpu.register_a, 0x01);
     }

     #[test]
     fn test_0x10_bpl() {
        let mut cpu = test_cpu();
        cpu.status.remove(CpuFlags::NEGATIVE);
        cpu.run(vec![0x10, 0x03, 0x00, 0x02, 0xa9, 0x01, 0x00]);

        assert_eq!(cpu.register_a, 0x01);
     }

     #[test]
     fn test_0x50_bvc() {
        let mut cpu = test_cpu();
        cpu.status.remove(CpuFlags::OVERFLOW);
        cpu.run(vec![0x50, 0x03, 0x00, 0x02, 0xa9, 0x01, 0x00]);

        assert_eq!(cpu.register_a, 0x01);
     }

     #[test]
     fn test_0x70_bvs() {
        let mut cpu = test_cpu();
        cpu.status.insert(CpuFlags::OVERFLOW);
        cpu.run(vec![0x70, 0x03, 0x00, 0x02, 0xa9, 0x01, 0x00]);

        assert_eq!(cpu.register_a, 0x01);
     }

     #[test]
     fn test_0x18_clc() {
         let mut cpu = test_cpu();
         cpu.status.insert(CpuFlags::CARRY);
         cpu.run(vec![0x18, 0x00]);

         assert!(!cpu.status.contains(CpuFlags::CARRY));
     }

     #[test]
     fn test_0xd8_cld() {
         let mut cpu = test_cpu();
         cpu.status.insert(CpuFlags::DECIMAL);
         cpu.run(vec![0xd8, 0x00]);

         assert!(!cpu.status.contains(CpuFlags::DECIMAL));
     }

     #[test]
     fn test_0x58_cli() {
         let mut cpu = test_cpu();
         cpu.status.insert(CpuFlags::INTERRUPT);
         cpu.run(vec![0x58, 0x00]);

         assert!(!cpu.status.contains(CpuFlags::INTERRUPT));
     }

     #[test]
     fn test_0xb8_clv() {
         let mut cpu = test_cpu();
         cpu.status.insert(CpuFlags::OVERFLOW);
         cpu.run(vec![0xb8, 0x00]);

         assert!(!cpu.status.contains(CpuFlags::OVERFLOW));
     }

     #[test]
     fn test_0x29_and_immediate() {
        let mut cpu = test_cpu();
        cpu.register_a = 0b1111_0000;
        // Do AND operation with 0xAA (0b1010_1010)
        cpu.run(vec![0x29, 0xaa, 0x00]);

        assert_eq!(cpu.register_a, 0b1010_0000);
    }

     #[test]
     fn test_0x25_and_zeropage() {
         let mut cpu = test_cpu();
         cpu.register_a = 0b1111_0000;
         // Do AND operation with 0xAA (0b1010_1010)
         cpu.run(vec![0x25, 0x03, 0x00, 0xaa]);

         assert_eq!(cpu.register_a, 0b1010_0000);
     }

     #[test]
     fn test_0xc6_dec_zeropage() {
         let mut cpu = test_cpu();
         // Decrement the 0x01 at address 0x03 to 0x00
         cpu.run(vec![0xc6, 0x03, 0x00, 0x01]);

         assert_eq!(cpu.mem_read(0x03), 0x00);
     }

     #[test]
     fn test_0xe6_inc_zeropage() {
         let mut cpu = test_cpu();
         // Increment the 0x00 at address 0x03 to 0x01
         cpu.run(vec![0xe6, 0x03, 0x00, 0x00]);

         assert_eq!(cpu.mem_read(0x03), 0x01);
     }

     #[test]
     fn test_0xa2_ldx_immediate() {
         let mut cpu = test_cpu();
         cpu.register_x = 0;
         cpu.run(vec![0xa2, 0x01, 0x00]);

         assert_eq!(cpu.register_x, 0x01);
     }

     #[test]
     fn test_0xa6_ldx_zeropage() {
         let mut cpu = test_cpu();
         cpu.register_x = 0;
         cpu.run(vec![0xa6, 0x03, 0x00, 0x01]);

         assert_eq!(cpu.register_x, 0x01);
     }

     #[test]
     fn test_0xa0_ldy_immediate() {
         let mut cpu = test_cpu();
         cpu.register_y = 0;
         cpu.run(vec![0xa0, 0x01, 0x00]);

         assert_eq!(cpu.register_y, 0x01);
     }

     #[test]
     fn test_0xc9_cmp_immediate() {
         let mut cpu = test_cpu();
         cpu.register_a = 0x03;
         cpu.run(vec![0xc9, 0x03, 0x00]);
        
         assert!(cpu.status.contains(CpuFlags::ZERO));
         assert!(cpu.status.contains(CpuFlags::CARRY));
         assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
     }

     #[test]
     fn test_0xc9_cmp_immediate_negative() {
         let mut cpu = test_cpu();
         cpu.register_a = 0x02;
         cpu.run(vec![0xc9, 0x03, 0x00]);

         assert!(!cpu.status.contains(CpuFlags::ZERO));
         assert!(!cpu.status.contains(CpuFlags::CARRY));
         assert!(cpu.status.contains(CpuFlags::NEGATIVE));
     }

     #[test]
     fn test_0xe0_cpx_immediate() {
         let mut cpu = test_cpu();
         cpu.register_x = 0x04;
         cpu.run(vec![0xe0, 0x03, 0x00]);

         assert!(!cpu.status.contains(CpuFlags::ZERO));
         assert!(cpu.status.contains(CpuFlags::CARRY));
         assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
     }

     #[test]
     fn test_0xc0_cpy_immediate() {
        let mut cpu = test_cpu();
        cpu.register_y = 0xe3;
        cpu.run(vec![0xc0, 0xe3, 0x00]);

        assert!(cpu.status.contains(CpuFlags::ZERO));
        assert!(cpu.status.contains(CpuFlags::CARRY));
        assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
     }

     #[test]
     fn test_0x20_jsr() {
        let mut cpu = test_cpu();
        cpu.run(vec![0x20, 0x05, 0x00]);

        assert_eq!(cpu.program_counter, 0x06);
        assert_eq!(cpu.pop_stack(), 0x02);
     }

     #[test]
     fn test_0x08_php() {
         let mut cpu = test_cpu();
         cpu.status.insert(CpuFlags::NEGATIVE);
         cpu.run(vec![0x08, 0x00]);

         let recovered = CpuFlags::from_bits(cpu.pop_stack()).unwrap();
         assert!(recovered.contains(CpuFlags::NEGATIVE));
         assert!(recovered.contains(CpuFlags::BREAK));
         assert!(recovered.contains(CpuFlags::BREAK2));
         assert!(!recovered.contains(CpuFlags::OVERFLOW));
     }

     #[test]
     fn test_0x0a_asl_acc() {
         let mut cpu = test_cpu();
         cpu.register_a = 0b1001_1100;
         cpu.run(vec![0x0a, 0x00]);

         assert_eq!(cpu.register_a, 0b0011_1000);
         assert!(!cpu.status.contains(CpuFlags::ZERO));
         assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
         assert!(cpu.status.contains(CpuFlags::CARRY));
     }

     #[test]
     fn test_0x06_asl_zeropage() {
         let mut cpu = test_cpu();
         cpu.run(vec![0x06, 0x03, 0x00, 0b1001_1100]);

         assert_eq!(cpu.mem_read(0x03), 0b0011_1000);
         assert!(!cpu.status.contains(CpuFlags::ZERO));
         assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
         assert!(cpu.status.contains(CpuFlags::CARRY));
     }

     #[test]
     fn test_0x24_bit_zeropage() {
        let mut cpu = test_cpu();
        cpu.register_a = 0b0000_0001;
        cpu.run(vec![0x24, 0x03, 0x00, 0b1100_0001]);

        assert!(cpu.status.contains(CpuFlags::ZERO));
        assert!(cpu.status.contains(CpuFlags::NEGATIVE));
        assert!(cpu.status.contains(CpuFlags::OVERFLOW));
     }

     #[test]
     fn test_0x49_eor_immediate() {
         let mut cpu = test_cpu();
         cpu.register_a = 0b0011_0011;
         cpu.run(vec![0x49, 0b0000_1111]);

         assert_eq!(cpu.register_a, 0b0011_1100);
     }

     #[test]
     fn test_0x4c_jmp_absolute() {
         let mut cpu = test_cpu();
         // Absolute address of 0x0003, grabs value of 0x0006 and jumps there.
         // Executes a BRK immediately, PC is left at 0x0007.
         cpu.run(vec![0x4c, 0x03, 0x00, 0x06, 0x00, 0x00, 0x00, 0x00]);
         
         assert_eq!(cpu.program_counter, 0x07);
     }

     #[test]
     fn test_0x4a_asl_acc() {
         let mut cpu = test_cpu();
         cpu.register_a = 0b1100_1001;
         cpu.run(vec![0x4a, 0x00]);

         assert_eq!(cpu.register_a, 0b0110_0100);
         assert!(!cpu.status.contains(CpuFlags::ZERO));
         assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
         assert!(cpu.status.contains(CpuFlags::CARRY));
     }

     #[test]
     fn test_0x46_asl_zeropage() {
         let mut cpu = test_cpu();
         cpu.run(vec![0x46, 0x03, 0x00, 0b1100_1001]);

         assert_eq!(cpu.mem_read(0x03), 0b0110_0100);
         assert!(!cpu.status.contains(CpuFlags::ZERO));
         assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
         assert!(cpu.status.contains(CpuFlags::CARRY));
     }

     #[test]
     fn test_0x09_ora_immediate() {
         let mut cpu = test_cpu();
         cpu.register_a = 0b0011_0011;
         cpu.run(vec![0x09, 0b0000_1111, 0x00]);

         assert_eq!(cpu.register_a, 0b0011_1111);
         assert!(!cpu.status.contains(CpuFlags::ZERO));
         assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
     }

     #[test]
     fn test_0x05_ora_zeropage() {
         let mut cpu = test_cpu();
         cpu.register_a = 0b0011_0011;
         cpu.run(vec![0x05, 0x03, 0x00, 0b0000_1111]);

         assert_eq!(cpu.register_a, 0b0011_1111);
         assert!(!cpu.status.contains(CpuFlags::ZERO));
         assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
     }

     #[test]
     fn test_0x2a_rol_acc() {
         let mut cpu = test_cpu();
         cpu.register_a = 0b1001_1100;
         cpu.status.insert(CpuFlags::CARRY);
         cpu.run(vec![0x2a, 0x00]);

         assert_eq!(cpu.register_a, 0b0011_1001);
         assert!(cpu.status.contains(CpuFlags::CARRY));
         assert!(!cpu.status.contains(CpuFlags::ZERO));
         assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
     }

     #[test]
     fn test_0x26_rol_zeropage() {
         let mut cpu = test_cpu();
         cpu.status.insert(CpuFlags::CARRY);
         cpu.run(vec![0x26, 0x03, 0x00, 0b1001_1100]);

         assert_eq!(cpu.mem_read(0x03), 0b0011_1001);
         assert!(cpu.status.contains(CpuFlags::CARRY));
         assert!(!cpu.status.contains(CpuFlags::ZERO));
         assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
     }

     #[test]
     fn test_0x6a_ror_acc() {
         let mut cpu = test_cpu();
         cpu.register_a = 0b0011_1001;
         cpu.status.insert(CpuFlags::CARRY);
         cpu.run(vec![0x6a, 0x00]);

         assert_eq!(cpu.register_a, 0b1001_1100);
         assert!(cpu.status.contains(CpuFlags::CARRY));
         assert!(!cpu.status.contains(CpuFlags::ZERO));
         assert!(cpu.status.contains(CpuFlags::NEGATIVE));
     }
     
     #[test]
     fn test_0x6a_ror_acc_zero() {
         let mut cpu = test_cpu();
         cpu.register_a = 0b0000_0001;
         cpu.run(vec![0x6a, 0x00]);

         assert_eq!(cpu.register_a, 0);
         assert!(cpu.status.contains(CpuFlags::ZERO));
         assert!(cpu.status.contains(CpuFlags::CARRY));
         assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
     }

     #[test]
     fn test_0x66_ror_zeropage() {
        let mut cpu = test_cpu();
        cpu.status.insert(CpuFlags::CARRY);
        cpu.run(vec![0x66, 0x03, 0x00, 0b0011_1001]);

        assert_eq!(cpu.mem_read(0x03), 0b1001_1100);
        assert!(cpu.status.contains(CpuFlags::CARRY));
        assert!(!cpu.status.contains(CpuFlags::ZERO));
        assert!(cpu.status.contains(CpuFlags::NEGATIVE));
     }

     #[test]
     fn test_0x85_sta_zeropage() {
         let mut cpu = test_cpu();
         cpu.register_a = 0x77;
         cpu.run(vec![0x85, 0x03, 0x00, 0x00]);

         assert_eq!(cpu.mem_read(0x03), 0x77);
     }

     #[test]
     fn test_0x86_stx_zeropage() {
         let mut cpu = test_cpu();
         cpu.register_x = 0x77;
         cpu.run(vec![0x86, 0x03, 0x00, 0x00]);

         assert_eq!(cpu.mem_read(0x03), 0x77);
     }

     #[test]
     fn test_0x84_sty_zeropage() {
         let mut cpu = test_cpu();
         cpu.register_y = 0x77;
         cpu.run(vec![0x84, 0x03, 0x00, 0x00]);

         assert_eq!(cpu.mem_read(0x03), 0x77);
     }
}