use crate::cart::{Cartridge, CartMem};
use crate::opcodes::{Op, OpWithMode, Opcode, OPCODE_MAP};
use crate::ppu::PPU;
use bitflags::bitflags;

#[derive(Debug, Clone, Copy)]
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

pub trait Mem {
    fn mem_read(&mut self, addr: u16) -> u8;
    fn mem_write(&mut self, addr: u16, value: u8);

    fn mem_read_u16(&mut self, addr: u16) -> u16 {
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

    pub ram: [u8; 2048],
}

impl Default for CPU {
    fn default() -> Self { CPU { 
            register_a: 0, 
            register_x: 0, 
            register_y: 0, 
            status: CpuFlags::empty(), 
            program_counter: 0, 
            stack_pointer: 0xFF, 
            ram: [0; 2048] 
        } 
    }
}

impl CPU {
    pub fn push_stack(&mut self, value: u8) {
        // Write the value to the stack position then DECREMENT the stack pointer.
        // The stack is bound to page 1 (0x0100 - 0x01FF)
        self.ram[(0x0100 | self.stack_pointer as u16) as usize] = value;
        self.stack_pointer -= 1;
    }

    pub fn pop_stack(&mut self) -> u8 {
        self.stack_pointer += 1;
        self.ram[(0x0100 | self.stack_pointer as u16) as usize]
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

    pub fn update_zero_negative_flags(&mut self, result: u8) {
        // Update zero flag
        self.status.set(CpuFlags::ZERO, result == 0);
        // Update negative flag
        self.status.set(CpuFlags::NEGATIVE, result & 0b1000_0000 != 0);
    }

    pub fn adc(&mut self, value: u8) {
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
    
    pub fn and(&mut self, value: u8) {
        self.register_a &= value;
        self.update_zero_negative_flags(self.register_a);
    }
    
    pub fn lda(&mut self, value: u8) {
        self.register_a = value;
        self.update_zero_negative_flags(value)
    }
    
    pub fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_negative_flags(self.register_x);
    }
    
    pub fn tay(&mut self) {
        self.register_y = self.register_a;
        self.update_zero_negative_flags(self.register_y);
    }
    
    pub fn txa(&mut self) {
        self.register_a = self.register_x;
        self.update_zero_negative_flags(self.register_a);
    }
    
    pub fn tya(&mut self) {
        self.register_a = self.register_y;
        self.update_zero_negative_flags(self.register_a);
    }
    
    pub fn dex(&mut self) {
        self.register_x = self.register_x.wrapping_sub(1);
        self.update_zero_negative_flags(self.register_x);
    }
    
    pub fn dey(&mut self) {
        self.register_y = self.register_y.wrapping_sub(1);
        self.update_zero_negative_flags(self.register_y);
    }

    pub fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_negative_flags(self.register_x);
    }
    
    pub fn iny(&mut self) {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_negative_flags(self.register_y);
    }

    pub fn pla(&mut self) {
        self.register_a = self.pop_stack();
        self.update_zero_negative_flags(self.register_a);   
    }

    pub fn plp(&mut self) {
        self.status.bits = (self.pop_stack() & 0b1110_1111) | (self.status.bits & 0b0010_0000)
    }

    pub fn php(&mut self) {
        // Pushing the status register sets both BREAK flags to 1 (but only in the pushed version)
        let mut new_flags = self.status.clone();
        new_flags.insert(CpuFlags::BREAK);
        new_flags.insert(CpuFlags::BREAK2);
        self.push_stack(new_flags.bits())
    }
    
    pub fn rti(&mut self) {
        self.status.bits = self.pop_stack();
        self.status.remove(CpuFlags::BREAK);
        self.status.insert(CpuFlags::BREAK2);
    
        self.program_counter = self.pop_stack_16();
    }
    
    pub fn asl_acc(&mut self) {
        // Set the carry-out flag to bit 7 of the accumulator:
        self.status.set(CpuFlags::CARRY, self.register_a & 0b1000_0000 != 0);
        // Shift the register
        self.register_a = self.register_a << 1;
        self.update_zero_negative_flags(self.register_a);
    }

    pub fn lsr_acc(&mut self) {
        self.status.set(CpuFlags::CARRY, self.register_a & 0b0000_0001 != 0);
        self.register_a = self.register_a >> 1;
        self.update_zero_negative_flags(self.register_a);
    }

    pub fn rol_acc(&mut self) {
        // Calculate result of rotating in carry flag
        let res = (self.register_a << 1) | (if self.status.contains(CpuFlags::CARRY) {1} else {0});
        // Update carry flag after calculation
        self.status.set(CpuFlags::CARRY, self.register_a & 0b1000_0000 != 0);
        self.register_a = res;
        self.update_zero_negative_flags(self.register_a);
    }

    pub fn ror_acc(&mut self) {
        // Calculate result of rotating in carry flag
        let res = (self.register_a >> 1) | (if self.status.contains(CpuFlags::CARRY) {0b1000_0000} else {0});
        // Update carry flag after calculation
        self.status.set(CpuFlags::CARRY, self.register_a * 0b0000_0001 != 0);
        self.register_a = res;
        self.update_zero_negative_flags(self.register_a);
    }

    pub fn bit(&mut self, value: u8) {
        // Bit 7 and 6 of value are put into N and V flags.
        // Z flag is set to (register_a AND value)
        self.status.set(CpuFlags::NEGATIVE, value & 0b1000_0000 == 0b1000_0000);
        self.status.set(CpuFlags::OVERFLOW, value & 0b0100_0000 == 0b0100_0000);
        self.status.set(CpuFlags::ZERO, self.register_a & value == 0);
    }

    pub fn tsx(&mut self) {
        self.register_x = self.stack_pointer;
        self.update_zero_negative_flags(self.register_x);
    }
}


const RAM_START: u16 = 0x0000;
const RAM_END: u16 = 0x1FFF;

const PPU_REGISTER_START: u16 = 0x2000;
const PPU_REGISTER_END: u16 = 0x2007;

const PPU_OAM: u16 = 0x4014;

const PRG_ROM_START: u16 = 0x8000;
const PRG_ROM_END: u16 = 0xFFFF;


pub fn mem_read(cpu: &CPU, ppu: &mut PPU, cartridge: &Cartridge, addr: u16) -> u8 {
    match addr {
        RAM_START..=RAM_END => cpu.ram[(addr & 0x7FF) as usize],
        PPU_REGISTER_START..=PPU_REGISTER_END => ppu.mem_read(addr, cartridge),
        PRG_ROM_START..=PRG_ROM_END => cartridge.mem_read(addr),
        _ => panic!("Attempted to read from unknown address")
    }
}

pub fn mem_write(cpu: &mut CPU, ppu: &mut PPU, cartridge: &mut Cartridge, addr: u16, value: u8) {
    match addr {
        RAM_START..=RAM_END => cpu.ram[(addr & 0x7FF) as usize] = value,
        PPU_REGISTER_START..=PPU_REGISTER_END => ppu.mem_write(addr, value, cartridge),
        PRG_ROM_START..=PRG_ROM_END => cartridge.mem_write(addr, value),
        PPU_OAM => todo!("OAM to PPU"),
        _ => panic!("Attempted to write to unknown address")
    }
}

pub fn mem_read_u16(cpu: &CPU, ppu: &mut PPU, cartridge: &Cartridge, addr: u16) -> u16 {
    // The NES packs 16-bit values in little endian
    let lo = mem_read(cpu, ppu, cartridge, addr) as u16;
    let hi = mem_read(cpu, ppu, cartridge, addr + 1) as u16;
    (hi << 8) | (lo)
}

pub fn mem_write_u16(cpu: &mut CPU, ppu: &mut PPU, cartridge: &mut Cartridge, addr: u16, value: u16) {
    let lo = (value & 0xff) as u8;
    let hi = (value >> 8) as u8;
    mem_write(cpu, ppu, cartridge, addr, lo);
    mem_write(cpu, ppu, cartridge, addr + 1, hi);
}


pub fn get_operand_addr(cpu: &CPU, ppu: &mut PPU, cartridge: &Cartridge, mode: &AddressingMode, from: u16) -> u16 {
    match mode {
        AddressingMode::Immediate => from,
        AddressingMode::ZeroPage => mem_read(cpu, ppu, cartridge, from) as u16,
        AddressingMode::ZeroPage_X => mem_read(cpu, ppu, cartridge, from).wrapping_add(cpu.register_x) as u16,
        AddressingMode::ZeroPage_Y => mem_read(cpu, ppu, cartridge, from).wrapping_add(cpu.register_y) as u16,
        AddressingMode::Absolute => mem_read_u16(cpu, ppu, cartridge, from),
        AddressingMode::Absolute_X => mem_read_u16(cpu, ppu, cartridge, from).wrapping_add(cpu.register_x as u16),
        AddressingMode::Absolute_Y => mem_read_u16(cpu, ppu, cartridge, from).wrapping_add(cpu.register_y as u16),
        AddressingMode::Indirect => {
            // Get the bytes at the PC, this is the address of the address we want (two levels of deref).
            // However, we can't read over a page boundary!
            let ptr = mem_read_u16(cpu, ppu, cartridge, from);
            if ptr & 0x00FF == 0x00FF {
                let lo = mem_read(cpu, ppu, cartridge, ptr);
                let hi = mem_read(cpu, ppu, cartridge, ptr & 0xFF00);
                (hi as u16) << 8 | (lo as u16)
            } else {
                mem_read_u16(cpu, ppu, cartridge, ptr) 
            }
        },
        AddressingMode::Indirect_X => {
            let ptr = (mem_read(cpu, ppu, cartridge, from) as u8).wrapping_add(cpu.register_x);
            let lo = mem_read(cpu, ppu, cartridge, ptr as u16);
            let hi = mem_read(cpu, ppu, cartridge, ptr.wrapping_add(1) as u16);
            (hi as u16) << 8 | (lo as u16)
        },
        AddressingMode::Indirect_Y => {
            let ptr = mem_read(cpu, ppu, cartridge, from);
            let lo = mem_read(cpu, ppu, cartridge, ptr as u16);
            let hi = mem_read(cpu, ppu, cartridge, ptr.wrapping_add(1) as u16);
            let deref_addr = (hi as u16) << 8 | (lo as u16);
            deref_addr.wrapping_add(cpu.register_y as u16)
        },
        AddressingMode::Accumulator => panic!("Cannot get address of accumulator"),
    }
}

fn get_operand_addr_and_step(cpu: &mut CPU, ppu: &mut PPU, cartridge: &Cartridge, mode: &AddressingMode, from: u16) -> u16 {
    let res = get_operand_addr(cpu, ppu, cartridge, mode, from);
    cpu.program_counter += match mode {
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

fn branch(cpu: &mut CPU, ppu: &mut PPU, cartridge: &Cartridge, condition: bool) {
    let jump_amt: i8 = mem_read(cpu, ppu, cartridge, cpu.program_counter) as i8;
    cpu.program_counter += 1;
    if condition {
        cpu.program_counter = cpu.program_counter.wrapping_add(jump_amt as u16);
    }
}

fn dec(cpu: &mut CPU, ppu: &mut PPU, cartridge: &mut Cartridge, addr: u16) {
    let res = mem_read(cpu, ppu, cartridge, addr).wrapping_sub(1);
    mem_write(cpu, ppu, cartridge, addr, res);
    cpu.update_zero_negative_flags(res);
}

fn inc(cpu: &mut CPU, ppu: &mut PPU, cartridge: &mut Cartridge, addr: u16) {
    let res = mem_read(cpu, ppu, cartridge, addr).wrapping_add(1);
    mem_write(cpu, ppu, cartridge, addr, res);
    cpu.update_zero_negative_flags(res);
}


fn jsr(cpu: &mut CPU, ppu: &mut PPU, cartridge: &mut Cartridge) {
    // Push the PC + 1, to skip to the first instruction after the jsr
    cpu.push_stack_16(cpu.program_counter + 1);
    // Then jump to the address specified by the next two bytes:
    cpu.program_counter = mem_read_u16(cpu, ppu, cartridge, cpu.program_counter);
}


fn asl(cpu: &mut CPU, ppu: &mut PPU, cartridge: &mut Cartridge, mode: &AddressingMode) {
    match mode {
        AddressingMode::Accumulator => cpu.asl_acc(),
        _ => {
            let addr = get_operand_addr_and_step(cpu, ppu, cartridge, mode, cpu.program_counter);
            let v = mem_read(cpu, ppu, cartridge, addr);
            let res = v << 1;
            cpu.status.set(CpuFlags::CARRY, v & 0b1000_0000 != 0);
            mem_write(cpu, ppu, cartridge, addr, res);
            cpu.update_zero_negative_flags(res);
        }
    }
}

fn lsr(cpu: &mut CPU, ppu: &mut PPU, cartridge: &mut Cartridge, mode: &AddressingMode) {
    match mode {
        AddressingMode::Accumulator => cpu.lsr_acc(),
        _ => {
            let addr = get_operand_addr_and_step(cpu, ppu, cartridge, mode, cpu.program_counter);
            let v = mem_read(cpu, ppu, cartridge, addr);
            let res = v >> 1;
            cpu.status.set(CpuFlags::CARRY, v & 0b0000_0001 != 0);
            mem_write(cpu, ppu, cartridge, addr, res);
            cpu.update_zero_negative_flags(res);
        }
    }
}


fn rol(cpu: &mut CPU, ppu: &mut PPU, cartridge: &mut Cartridge, mode: &AddressingMode) {
    match mode {
        AddressingMode::Accumulator => cpu.rol_acc(),
        _ => {
            let addr = get_operand_addr_and_step(cpu, ppu, cartridge, mode, cpu.program_counter);
            let v = mem_read(cpu, ppu, cartridge, addr);
            let res = (v << 1) | (if cpu.status.contains(CpuFlags::CARRY) {1} else {0});
            cpu.status.set(CpuFlags::CARRY, v & 0b1000_0000 != 0);
            mem_write(cpu, ppu, cartridge, addr, res);
            cpu.update_zero_negative_flags(res);
        }
    }
}



fn ror(cpu: &mut CPU, ppu: &mut PPU, cartridge: &mut Cartridge, mode: &AddressingMode) {
    match mode {
        AddressingMode::Accumulator => cpu.ror_acc(),
        _ => {
            let addr = get_operand_addr_and_step(cpu, ppu, cartridge, mode, cpu.program_counter);
            let v = mem_read(cpu, ppu, cartridge, addr);
            let res = (v >> 1) | (if cpu.status.contains(CpuFlags::CARRY) {0b1000_0000} else {0});
            cpu.status.set(CpuFlags::CARRY, v & 0b0000_0001 != 0);
            mem_write(cpu, ppu, cartridge, addr, res);
            cpu.update_zero_negative_flags(res);
        }
    }
}

fn cmp(cpu: &mut CPU, ppu: &mut PPU, cartridge: &mut Cartridge, lhs: u8, addr: u16) {
    // LHS is one of register_a, register_x, or register_y.
    // RHS comes from Memory
    let rhs = mem_read(cpu, ppu, cartridge, addr);
    let res = lhs.wrapping_sub(rhs);
    cpu.update_zero_negative_flags(res);
    cpu.status.set(CpuFlags::CARRY, lhs >= rhs);
}


pub fn interpret_op(cpu: &mut CPU, ppu: &mut PPU, cartridge: &mut Cartridge, opcode: &Op) {
    match opcode {
        Op::BCC => branch(cpu, ppu, cartridge, !cpu.status.contains(CpuFlags::CARRY)),
        Op::BCS => branch(cpu, ppu, cartridge, cpu.status.contains(CpuFlags::CARRY)),
        Op::BEQ => branch(cpu, ppu, cartridge, cpu.status.contains(CpuFlags::ZERO)),
        Op::BMI => branch(cpu, ppu, cartridge, cpu.status.contains(CpuFlags::NEGATIVE)),
        Op::BNE => branch(cpu, ppu, cartridge, !cpu.status.contains(CpuFlags::ZERO)),
        Op::BPL => branch(cpu, ppu, cartridge, !cpu.status.contains(CpuFlags::NEGATIVE)),
        Op::BRK => (), //TODO: push PC+2, set I flag, push SR
        Op::BVC => branch(cpu, ppu, cartridge, !cpu.status.contains(CpuFlags::OVERFLOW)),
        Op::BVS => branch(cpu, ppu, cartridge, cpu.status.contains(CpuFlags::OVERFLOW)),
        Op::CLC => cpu.status.remove(CpuFlags::CARRY),
        Op::CLD => cpu.status.remove(CpuFlags::DECIMAL),
        Op::CLI => cpu.status.remove(CpuFlags::INTERRUPT),
        Op::CLV => cpu.status.remove(CpuFlags::OVERFLOW),
        Op::DEX => cpu.dex(),
        Op::DEY => cpu.dey(),
        Op::INX => cpu.inx(),
        Op::INY => cpu.iny(),
        Op::JSR => jsr(cpu, ppu, cartridge),
        Op::NOP => (),
        Op::PHA => cpu.push_stack(cpu.register_a),
        Op::PHP => cpu.php(),
        Op::PLA => cpu.pla(),
        Op::PLP => cpu.plp(),
        Op::RTI => cpu.rti(),
        Op::RTS => cpu.program_counter = cpu.pop_stack_16() + 1,
        Op::SEC => cpu.status.insert(CpuFlags::CARRY),
        Op::SED => cpu.status.insert(CpuFlags::DECIMAL),
        Op::SEI => cpu.status.insert(CpuFlags::INTERRUPT),
        Op::TAX => cpu.tax(),
        Op::TAY => cpu.tay(),
        Op::TSX => cpu.tsx(),
        Op::TXA => cpu.txa(),
        Op::TXS => cpu.stack_pointer = cpu.register_x,
        Op::TYA => cpu.tya(),
    }
}

pub fn interpret_op_with_mode(cpu: &mut CPU, ppu: &mut PPU, cartridge: &mut Cartridge, opcode: &OpWithMode, mode: &AddressingMode) {
    match opcode {
        OpWithMode::ADC => {
            let addr = get_operand_addr_and_step(cpu, ppu, cartridge, mode, cpu.program_counter);
            let value = mem_read(cpu, ppu, cartridge, addr);
            cpu.adc(value);
        },
        OpWithMode::AND => {
            let addr = get_operand_addr_and_step(cpu, ppu, cartridge, mode, cpu.program_counter);
            let value = mem_read(cpu, ppu, cartridge, addr);
            cpu.and(value);
        },
        OpWithMode::ASL => asl(cpu, ppu, cartridge, mode),
        OpWithMode::BIT => {
            let addr = get_operand_addr_and_step(cpu, ppu, cartridge, mode, cpu.program_counter);
            let value = mem_read(cpu, ppu, cartridge, addr);
            cpu.bit(value);
        },
        OpWithMode::CMP => {
            let addr = get_operand_addr_and_step(cpu, ppu, cartridge, mode, cpu.program_counter);
            cmp(cpu, ppu, cartridge, cpu.register_a, addr);
        },
        OpWithMode::CPX => {
            let addr = get_operand_addr_and_step(cpu, ppu, cartridge, mode, cpu.program_counter);
            cmp(cpu, ppu, cartridge, cpu.register_x, addr);
        },
        OpWithMode::CPY => {
            let addr = get_operand_addr_and_step(cpu, ppu, cartridge, mode, cpu.program_counter);
            cmp(cpu, ppu, cartridge, cpu.register_y, addr);
        },
        OpWithMode::DEC => {
            let addr = get_operand_addr_and_step(cpu, ppu, cartridge, mode, cpu.program_counter);
            dec(cpu, ppu, cartridge, addr);
        },
        OpWithMode::EOR => {
            let addr = get_operand_addr_and_step(cpu, ppu, cartridge, mode, cpu.program_counter);
            let value = mem_read(cpu, ppu, cartridge, addr);
            cpu.register_a = cpu.register_a ^ value;
            cpu.update_zero_negative_flags(cpu.register_a);
        },
        OpWithMode::INC => {
            let addr = get_operand_addr_and_step(cpu, ppu, cartridge, mode, cpu.program_counter);
            inc(cpu, ppu, cartridge, addr);
        },
        OpWithMode::JMP => {
            let addr = get_operand_addr_and_step(cpu, ppu, cartridge, mode, cpu.program_counter);
            cpu.program_counter = addr;
        },
        OpWithMode::LDA => {
            let addr = get_operand_addr_and_step(cpu, ppu, cartridge, mode, cpu.program_counter);
            let value = mem_read(cpu, ppu, cartridge, addr);
            cpu.lda(value);
        },
        OpWithMode::LDX => {
            let addr = get_operand_addr_and_step(cpu, ppu, cartridge, mode, cpu.program_counter);
            cpu.register_x = mem_read(cpu, ppu, cartridge, addr);
            cpu.update_zero_negative_flags(cpu.register_x);
        },
        OpWithMode::LDY => {
            let addr = get_operand_addr_and_step(cpu, ppu, cartridge, mode, cpu.program_counter);
            cpu.register_y = mem_read(cpu, ppu, cartridge, addr);
            cpu.update_zero_negative_flags(cpu.register_y);
        },
        OpWithMode::LSR => lsr(cpu, ppu, cartridge, mode),
        OpWithMode::ORA => {
            let addr = get_operand_addr_and_step(cpu, ppu, cartridge, mode, cpu.program_counter);
            let value = mem_read(cpu, ppu, cartridge, addr);
            cpu.register_a = cpu.register_a | value;
            cpu.update_zero_negative_flags(cpu.register_a);
        },
        OpWithMode::ROL => rol(cpu, ppu, cartridge, mode),
        OpWithMode::ROR => ror(cpu, ppu, cartridge, mode),
        OpWithMode::SBC => {
            let addr = get_operand_addr_and_step(cpu, ppu, cartridge, mode, cpu.program_counter);
            let value = mem_read(cpu, ppu, cartridge, addr);
            // SBC is actually ADC but with 2's complement of the value:
            cpu.adc(((value as i8).wrapping_neg().wrapping_sub(1)) as u8);
        },
        OpWithMode::STA => {
            let addr = get_operand_addr_and_step(cpu, ppu, cartridge, mode, cpu.program_counter);
            mem_write(cpu, ppu, cartridge, addr, cpu.register_a);
        },
        OpWithMode::STX => {
            let addr = get_operand_addr_and_step(cpu, ppu, cartridge, mode, cpu.program_counter);
            mem_write(cpu, ppu, cartridge, addr, cpu.register_x);
        },
        OpWithMode::STY => {
            let addr = get_operand_addr_and_step(cpu, ppu, cartridge, mode, cpu.program_counter);
            mem_write(cpu, ppu, cartridge, addr, cpu.register_y);
        },
    }
}

fn interpret(cpu: &mut CPU, ppu: &mut PPU, cartridge: &mut Cartridge, opcode: &Opcode) -> u8 {
    match opcode {
        Opcode::Op { op, code: _, len: _, cycles } => {
            interpret_op(cpu, ppu, cartridge, op);
            return *cycles;
        },
        Opcode::OpWithMode { op, code: _, len: _, cycles, mode } => {
            interpret_op_with_mode(cpu, ppu, cartridge, op, mode);
            return *cycles;
        },
    }
}

pub fn step_cpu(cpu: &mut CPU, ppu: &mut PPU, cartridge: &mut Cartridge) -> u8 {
    let code = mem_read(cpu, ppu, cartridge, cpu.program_counter);
    cpu.program_counter += 1;
    let opcode = OPCODE_MAP.get(&code).expect(&format!("Unknown opcode {:x}", code));

    interpret(cpu, ppu, cartridge, opcode)
}


pub fn reset_cpu(cpu: &mut CPU, pc: u16) {
    cpu.register_a = 0;
    cpu.register_x = 0;
    cpu.register_y = 0;
    cpu.stack_pointer = 0xFD;
    cpu.program_counter = pc;
}



#[cfg(test)]
mod test {
    use super::*;
    use crate::cart::test;

    fn test_cpu() -> CPU {
        CPU::default()
    }

    fn run_test_opcodes(cpu: &mut CPU, opcodes: Vec<u8>) {
        // Returns the CPU state after running the provided opcodes
        let mut ppu = PPU::default();
        let mut cartridge = test::test_cart();

        // Write the program into CPU RAM
        let mut addr = 0;
        for code in opcodes.iter() {
            cpu.ram[addr] = *code;
            addr += 1;
        };

        // Run until we hit a BRK code
        cpu.program_counter = 0;
        while cpu.ram[cpu.program_counter as usize] != 0x00 {
            step_cpu(cpu, &mut ppu, &mut cartridge);
        }
    }

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = test_cpu();
        run_test_opcodes(&mut cpu, vec![0xa9, 0x05, 0x00]);
        assert_eq!(cpu.register_a, 0x05);
        assert!(!cpu.status.contains(CpuFlags::ZERO));
        assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = test_cpu();
        run_test_opcodes(&mut cpu, vec![0xa9, 0x00, 0x00]);
        assert!(cpu.status.contains(CpuFlags::ZERO));
    }

    #[test]
    fn test_0xa5_lda_zeropage_load() {
        let mut cpu = test_cpu();
        // Do a read at address 0x00, which is 0xa5 (the LDA instruction itself)
        run_test_opcodes(&mut cpu, vec![0xa5, 0x00, 0x00]);
        assert_eq!(cpu.register_a, 0xa5);
    }

    #[test]
    fn test_0xb5_lda_zeropagex_load() {
        let mut cpu = test_cpu();
        // Set register_x to 3, then do a zeropage_x read at addr 0x00 (+3), grabbing the 0xfc
        cpu.register_x = 3;
        run_test_opcodes(&mut cpu, vec![0xb5, 0x00, 0x00, 0xfc]);
        assert_eq!(cpu.register_a, 0xfc);
    }

    #[test]
    fn test_0xad_lda_absolute_load() {
        let mut cpu = test_cpu();
        // The absolute address is 0x0004 (little endian), grabbing the 0xfc
        run_test_opcodes(&mut cpu, vec![0xad, 0x04, 0x00, 0x00, 0xfc]);
        assert_eq!(cpu.register_a, 0xfc);
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = test_cpu();
        cpu.register_a = 10;
        run_test_opcodes(&mut cpu, vec![0xaa, 0x00]);
  
        assert_eq!(cpu.register_x, 10);
    }

    #[test]
    fn test_0xa8_tay_move_a_to_y() {
       let mut cpu = test_cpu();
       cpu.register_a = 10;
       run_test_opcodes(&mut cpu, vec![0xa8, 0x00]);
       
       assert_eq!(cpu.register_y, 10);
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = test_cpu();
        run_test_opcodes(&mut cpu, vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);
  
        assert_eq!(cpu.register_x, 0xc1);
    }
 
     #[test]
     fn test_0xe8_inx_overflow() {
         let mut cpu = test_cpu();
         cpu.register_x = 0xff;
         run_test_opcodes(&mut cpu, vec![0xe8, 0xe8, 0x00]);
 
         assert_eq!(cpu.register_x, 1);
     }

    #[test]
     fn test_0xc8_iny_overflow() {
         let mut cpu = test_cpu();
         cpu.register_y = 0xff;
         run_test_opcodes(&mut cpu, vec![0xc8, 0xc8, 0x00]);

         assert_eq!(cpu.register_y, 1);
     }

     #[test]
     fn test_0xca_dex_underflow() {
         let mut cpu = test_cpu();
         cpu.register_x = 0;
         run_test_opcodes(&mut cpu, vec![0xca, 0xca, 0x00]);

         assert_eq!(cpu.register_x, 0xfe);
     }

     #[test]
     fn test_0x88_dey_underflow() {
         let mut cpu = test_cpu();
         cpu.register_y = 0;
         run_test_opcodes(&mut cpu, vec![0x88, 0x88, 0x00]);

         assert_eq!(cpu.register_y, 0xfe);
     }

    #[test]
     fn test_0x8a_txa() {
         let mut cpu = test_cpu();
         cpu.register_x = 10;
         run_test_opcodes(&mut cpu, vec![0x8a, 0x00]);
         
         assert_eq!(cpu.register_a, 10);
     }

     #[test]
     fn test_0x98_tya() {
         let mut cpu = test_cpu();
         cpu.register_y = 10;
         run_test_opcodes(&mut cpu, vec![0x98, 0x00]);

         assert_eq!(cpu.register_a, 10);

         // Extra test for status flags
         let mut cpu2 = test_cpu();
         cpu2.register_y = 0;
         run_test_opcodes(&mut cpu2, vec![0x98, 0x00]);

         assert!(cpu2.status.contains(CpuFlags::ZERO));
         assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
     }

     #[test]
     fn test_0x38_sec() {
         let mut cpu = test_cpu();
         run_test_opcodes(&mut cpu, vec![0x38, 0x00]);

         assert!(cpu.status.contains(CpuFlags::CARRY));
     }

     #[test]
     fn test_0xf8_sed() {
         let mut cpu = test_cpu();
         run_test_opcodes(&mut cpu, vec![0xf8, 0x00]);

         assert!(cpu.status.contains(CpuFlags::DECIMAL));
     }

     #[test]
     fn test_0x78_sei() {
         let mut cpu = test_cpu();
         run_test_opcodes(&mut cpu, vec![0x78, 0x00]);

         assert!(cpu.status.contains(CpuFlags::INTERRUPT));
     }

     #[test]
     fn test_0x90_bcc() {
         let mut cpu = test_cpu();
         cpu.status.remove(CpuFlags::CARRY);
         // Branch reads the relative address from 0x03 of 4, causing execution
         // to jump past the BRK and execute the LDA.
         run_test_opcodes(&mut cpu, vec![0x90, 0x02, 0x00, 0x02, 0xa9, 0x01, 0x00]);

         assert_eq!(cpu.register_a, 0x01);
     }

     #[test]
     fn test_0xb0_bcs() {
         let mut cpu = test_cpu();
         cpu.status.insert(CpuFlags::CARRY);
         run_test_opcodes(&mut cpu, vec![0xb0, 0x02, 0x00, 0x02, 0xa9, 0x01, 0x00]);

         assert_eq!(cpu.register_a, 0x01);
     }

     #[test]
     fn test_0xf0_beq() {
        let mut cpu = test_cpu();
        cpu.status.insert(CpuFlags::ZERO);
        run_test_opcodes(&mut cpu, vec![0xf0, 0x02, 0x00, 0x02, 0xa9, 0x01, 0x00]);

        assert_eq!(cpu.register_a, 0x01);
     }

     #[test]
     fn test_0x30_bmi() {
        let mut cpu = test_cpu();
        cpu.status.insert(CpuFlags::NEGATIVE);
        run_test_opcodes(&mut cpu, vec![0x30, 0x02, 0x00, 0x02, 0xa9, 0x01, 0x00]);

        assert_eq!(cpu.register_a, 0x01);
     }

     #[test]
     fn test_0xd0_bne() {
        let mut cpu = test_cpu();
        cpu.status.remove(CpuFlags::ZERO);
        run_test_opcodes(&mut cpu, vec![0xd0, 0x02, 0x00, 0x02, 0xa9, 0x01, 0x00]);

        assert_eq!(cpu.register_a, 0x01);
     }

     #[test]
     fn test_0x10_bpl() {
        let mut cpu = test_cpu();
        cpu.status.remove(CpuFlags::NEGATIVE);
        run_test_opcodes(&mut cpu, vec![0x10, 0x02, 0x00, 0x02, 0xa9, 0x01, 0x00]);

        assert_eq!(cpu.register_a, 0x01);
     }

     #[test]
     fn test_0x50_bvc() {
        let mut cpu = test_cpu();
        cpu.status.remove(CpuFlags::OVERFLOW);
        run_test_opcodes(&mut cpu, vec![0x50, 0x02, 0x00, 0x02, 0xa9, 0x01, 0x00]);

        assert_eq!(cpu.register_a, 0x01);
     }

     #[test]
     fn test_0x70_bvs() {
        let mut cpu = test_cpu();
        cpu.status.insert(CpuFlags::OVERFLOW);
        run_test_opcodes(&mut cpu, vec![0x70, 0x02, 0x00, 0x02, 0xa9, 0x01, 0x00]);

        assert_eq!(cpu.register_a, 0x01);
     }

     #[test]
     fn test_0x18_clc() {
         let mut cpu = test_cpu();
         cpu.status.insert(CpuFlags::CARRY);
         run_test_opcodes(&mut cpu, vec![0x18, 0x00]);

         assert!(!cpu.status.contains(CpuFlags::CARRY));
     }

     #[test]
     fn test_0xd8_cld() {
         let mut cpu = test_cpu();
         cpu.status.insert(CpuFlags::DECIMAL);
         run_test_opcodes(&mut cpu, vec![0xd8, 0x00]);

         assert!(!cpu.status.contains(CpuFlags::DECIMAL));
     }

     #[test]
     fn test_0x58_cli() {
         let mut cpu = test_cpu();
         cpu.status.insert(CpuFlags::INTERRUPT);
         run_test_opcodes(&mut cpu, vec![0x58, 0x00]);

         assert!(!cpu.status.contains(CpuFlags::INTERRUPT));
     }

     #[test]
     fn test_0xb8_clv() {
         let mut cpu = test_cpu();
         cpu.status.insert(CpuFlags::OVERFLOW);
         run_test_opcodes(&mut cpu, vec![0xb8, 0x00]);

         assert!(!cpu.status.contains(CpuFlags::OVERFLOW));
     }

     #[test]
     fn test_0x29_and_immediate() {
        let mut cpu = test_cpu();
        cpu.register_a = 0b1111_0000;
        // Do AND operation with 0xAA (0b1010_1010)
        run_test_opcodes(&mut cpu, vec![0x29, 0xaa, 0x00]);

        assert_eq!(cpu.register_a, 0b1010_0000);
    }

     #[test]
     fn test_0x25_and_zeropage() {
         let mut cpu = test_cpu();
         cpu.register_a = 0b1111_0000;
         // Do AND operation with 0xAA (0b1010_1010)
         run_test_opcodes(&mut cpu, vec![0x25, 0x03, 0x00, 0xaa]);

         assert_eq!(cpu.register_a, 0b1010_0000);
     }

     #[test]
     fn test_0xc6_dec_zeropage() {
         let mut cpu = test_cpu();
         // Decrement the 0x01 at address 0x03 to 0x00
         run_test_opcodes(&mut cpu, vec![0xc6, 0x03, 0x00, 0x01]);

         assert_eq!(cpu.ram[0x03], 0x00);
     }

     #[test]
     fn test_0xe6_inc_zeropage() {
         let mut cpu = test_cpu();
         // Increment the 0x00 at address 0x03 to 0x01
         run_test_opcodes(&mut cpu, vec![0xe6, 0x03, 0x00, 0x00]);

         assert_eq!(cpu.ram[0x03], 0x01);
     }

     #[test]
     fn test_0xa2_ldx_immediate() {
         let mut cpu = test_cpu();
         cpu.register_x = 0;
         run_test_opcodes(&mut cpu, vec![0xa2, 0x01, 0x00]);

         assert_eq!(cpu.register_x, 0x01);
     }

     #[test]
     fn test_0xa6_ldx_zeropage() {
         let mut cpu = test_cpu();
         cpu.register_x = 0;
         run_test_opcodes(&mut cpu, vec![0xa6, 0x03, 0x00, 0x01]);

         assert_eq!(cpu.register_x, 0x01);
     }

     #[test]
     fn test_0xa0_ldy_immediate() {
         let mut cpu = test_cpu();
         cpu.register_y = 0;
         run_test_opcodes(&mut cpu, vec![0xa0, 0x01, 0x00]);

         assert_eq!(cpu.register_y, 0x01);
     }

     #[test]
     fn test_0xc9_cmp_immediate() {
         let mut cpu = test_cpu();
         cpu.register_a = 0x03;
         run_test_opcodes(&mut cpu, vec![0xc9, 0x03, 0x00]);
        
         assert!(cpu.status.contains(CpuFlags::ZERO));
         assert!(cpu.status.contains(CpuFlags::CARRY));
         assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
     }

     #[test]
     fn test_0xc9_cmp_immediate_negative() {
         let mut cpu = test_cpu();
         cpu.register_a = 0x02;
         run_test_opcodes(&mut cpu, vec![0xc9, 0x03, 0x00]);

         assert!(!cpu.status.contains(CpuFlags::ZERO));
         assert!(!cpu.status.contains(CpuFlags::CARRY));
         assert!(cpu.status.contains(CpuFlags::NEGATIVE));
     }

     #[test]
     fn test_0xe0_cpx_immediate() {
         let mut cpu = test_cpu();
         cpu.register_x = 0x04;
         run_test_opcodes(&mut cpu, vec![0xe0, 0x03, 0x00]);

         assert!(!cpu.status.contains(CpuFlags::ZERO));
         assert!(cpu.status.contains(CpuFlags::CARRY));
         assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
     }

     #[test]
     fn test_0xc0_cpy_immediate() {
        let mut cpu = test_cpu();
        cpu.register_y = 0xe3;
        run_test_opcodes(&mut cpu, vec![0xc0, 0xe3, 0x00]);

        assert!(cpu.status.contains(CpuFlags::ZERO));
        assert!(cpu.status.contains(CpuFlags::CARRY));
        assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
     }

     #[test]
     fn test_0x20_jsr() {
        let mut cpu = test_cpu();
        run_test_opcodes(&mut cpu, vec![0x20, 0x05, 0x00]);

        assert_eq!(cpu.program_counter, 0x05);
        assert_eq!(cpu.pop_stack(), 0x02);
     }

     #[test]
     fn test_0x08_php() {
         let mut cpu = test_cpu();
         cpu.status.insert(CpuFlags::NEGATIVE);
         run_test_opcodes(&mut cpu, vec![0x08, 0x00]);

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
         run_test_opcodes(&mut cpu, vec![0x0a, 0x00]);

         assert_eq!(cpu.register_a, 0b0011_1000);
         assert!(!cpu.status.contains(CpuFlags::ZERO));
         assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
         assert!(cpu.status.contains(CpuFlags::CARRY));
     }

     #[test]
     fn test_0x06_asl_zeropage() {
         let mut cpu = test_cpu();
         run_test_opcodes(&mut cpu, vec![0x06, 0x03, 0x00, 0b1001_1100]);

         assert_eq!(cpu.ram[0x03], 0b0011_1000);
         assert!(!cpu.status.contains(CpuFlags::ZERO));
         assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
         assert!(cpu.status.contains(CpuFlags::CARRY));
     }

     #[test]
     fn test_0x24_bit_zeropage() {
        let mut cpu = test_cpu();
        cpu.register_a = 0b0000_0001;
        run_test_opcodes(&mut cpu, vec![0x24, 0x03, 0x00, 0b1100_0001]);

        assert!(!cpu.status.contains(CpuFlags::ZERO));
        assert!(cpu.status.contains(CpuFlags::NEGATIVE));
        assert!(cpu.status.contains(CpuFlags::OVERFLOW));
     }

     #[test]
     fn test_0x49_eor_immediate() {
         let mut cpu = test_cpu();
         cpu.register_a = 0b0011_0011;
         run_test_opcodes(&mut cpu, vec![0x49, 0b0000_1111]);

         assert_eq!(cpu.register_a, 0b0011_1100);
     }

     #[test]
     fn test_0x4c_jmp_indirect() {
         let mut cpu = test_cpu();
         // Read address of 0x0003, indirect grabs value of 0x0006 and jumps there.
         // Executes a BRK immediately, PC is left at 0x0007.
         run_test_opcodes(&mut cpu, vec![0x6c, 0x03, 0x00, 0x06, 0x00, 0x00, 0x00, 0x00]);
         
         assert_eq!(cpu.program_counter, 0x06);
     }

     #[test]
     fn test_0x4a_asl_acc() {
         let mut cpu = test_cpu();
         cpu.register_a = 0b1100_1001;
         run_test_opcodes(&mut cpu, vec![0x4a, 0x00]);

         assert_eq!(cpu.register_a, 0b0110_0100);
         assert!(!cpu.status.contains(CpuFlags::ZERO));
         assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
         assert!(cpu.status.contains(CpuFlags::CARRY));
     }

     #[test]
     fn test_0x46_asl_zeropage() {
         let mut cpu = test_cpu();
         run_test_opcodes(&mut cpu, vec![0x46, 0x03, 0x00, 0b1100_1001]);

         assert_eq!(cpu.ram[0x03], 0b0110_0100);
         assert!(!cpu.status.contains(CpuFlags::ZERO));
         assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
         assert!(cpu.status.contains(CpuFlags::CARRY));
     }

     #[test]
     fn test_0x09_ora_immediate() {
         let mut cpu = test_cpu();
         cpu.register_a = 0b0011_0011;
         run_test_opcodes(&mut cpu, vec![0x09, 0b0000_1111, 0x00]);

         assert_eq!(cpu.register_a, 0b0011_1111);
         assert!(!cpu.status.contains(CpuFlags::ZERO));
         assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
     }

     #[test]
     fn test_0x05_ora_zeropage() {
         let mut cpu = test_cpu();
         cpu.register_a = 0b0011_0011;
         run_test_opcodes(&mut cpu, vec![0x05, 0x03, 0x00, 0b0000_1111]);

         assert_eq!(cpu.register_a, 0b0011_1111);
         assert!(!cpu.status.contains(CpuFlags::ZERO));
         assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
     }

     #[test]
     fn test_0x2a_rol_acc() {
         let mut cpu = test_cpu();
         cpu.register_a = 0b1001_1100;
         cpu.status.insert(CpuFlags::CARRY);
         run_test_opcodes(&mut cpu, vec![0x2a, 0x00]);

         assert_eq!(cpu.register_a, 0b0011_1001);
         assert!(cpu.status.contains(CpuFlags::CARRY));
         assert!(!cpu.status.contains(CpuFlags::ZERO));
         assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
     }

     #[test]
     fn test_0x26_rol_zeropage() {
         let mut cpu = test_cpu();
         cpu.status.insert(CpuFlags::CARRY);
         run_test_opcodes(&mut cpu, vec![0x26, 0x03, 0x00, 0b1001_1100]);

         assert_eq!(cpu.ram[0x03], 0b0011_1001);
         assert!(cpu.status.contains(CpuFlags::CARRY));
         assert!(!cpu.status.contains(CpuFlags::ZERO));
         assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
     }

     #[test]
     fn test_0x6a_ror_acc() {
         let mut cpu = test_cpu();
         cpu.register_a = 0b0011_1001;
         cpu.status.insert(CpuFlags::CARRY);
         run_test_opcodes(&mut cpu, vec![0x6a, 0x00]);

         assert_eq!(cpu.register_a, 0b1001_1100);
         assert!(cpu.status.contains(CpuFlags::CARRY));
         assert!(!cpu.status.contains(CpuFlags::ZERO));
         assert!(cpu.status.contains(CpuFlags::NEGATIVE));
     }
     
     #[test]
     fn test_0x6a_ror_acc_zero() {
         let mut cpu = test_cpu();
         cpu.register_a = 0b0000_0001;
         run_test_opcodes(&mut cpu, vec![0x6a, 0x00]);

         assert_eq!(cpu.register_a, 0);
         assert!(cpu.status.contains(CpuFlags::ZERO));
         assert!(cpu.status.contains(CpuFlags::CARRY));
         assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
     }

     #[test]
     fn test_0x66_ror_zeropage() {
        let mut cpu = test_cpu();
        cpu.status.insert(CpuFlags::CARRY);
        run_test_opcodes(&mut cpu, vec![0x66, 0x03, 0x00, 0b0011_1001]);

        assert_eq!(cpu.ram[0x03], 0b1001_1100);
        assert!(cpu.status.contains(CpuFlags::CARRY));
        assert!(!cpu.status.contains(CpuFlags::ZERO));
        assert!(cpu.status.contains(CpuFlags::NEGATIVE));
     }

     #[test]
     fn test_0x85_sta_zeropage() {
         let mut cpu = test_cpu();
         cpu.register_a = 0x77;
         run_test_opcodes(&mut cpu, vec![0x85, 0x03, 0x00, 0x00]);

         assert_eq!(cpu.ram[0x03], 0x77);
     }

     #[test]
     fn test_0x86_stx_zeropage() {
         let mut cpu = test_cpu();
         cpu.register_x = 0x77;
         run_test_opcodes(&mut cpu, vec![0x86, 0x03, 0x00, 0x00]);

         assert_eq!(cpu.ram[0x03], 0x77);
     }

     #[test]
     fn test_0x84_sty_zeropage() {
         let mut cpu = test_cpu();
         cpu.register_y = 0x77;
         run_test_opcodes(&mut cpu, vec![0x84, 0x03, 0x00, 0x00]);

         assert_eq!(cpu.ram[0x03], 0x77);
     }
}