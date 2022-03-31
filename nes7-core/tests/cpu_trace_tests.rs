use std::fs::{self, File};
use std::io::{BufReader, BufRead};
use std::path::PathBuf;

use nes7_core::cpu::*;
use nes7_core::opcodes::{Opcode, Op, OpWithMode, OPCODE_MAP};
use nes7_core::cart::{self, Cartridge};
use nes7_core::ppu::PPU;


fn get_opcode_no_mode(cpu: &mut CPU, ppu: &mut PPU, cartridge: &mut Cartridge, pc: u16, op: &Op, _: &u8, len: &u8) -> String {
    let hex = match len {
        1 => format!("{:02X}", mem_read(cpu, ppu, cartridge, pc)),
        2 => format!("{:02X} {:02X}", mem_read(cpu, ppu, cartridge, pc), mem_read(cpu, ppu, cartridge, pc+1)),
        3 => format!("{:02X} {:02X} {:02X}", mem_read(cpu, ppu, cartridge, pc), mem_read(cpu, ppu, cartridge, pc+1), mem_read(cpu, ppu, cartridge, pc+2)),
        _ => panic!("too long no mode opcode"),
    };

    let ex = match len {
        1 => String::from(""),
        2 => {
            // Local jumps
            let jmp = mem_read(cpu, ppu, cartridge, pc + 1);
            let address = (pc as usize + 2).wrapping_add((jmp as i8) as usize);
            format!("${:04X}", address)
        },
        3 => {
            let address = mem_read_u16(cpu, ppu, cartridge, pc + 1);
            format!("${:04X}", address)
        },
        _ => String::from(""),
    };

    format!("{:8}  {: >4?} {}", hex, op, ex)
}

fn get_opcode_with_mode(cpu: &mut CPU, ppu: &mut PPU, cartridge: &mut Cartridge, pc: u16, op: &OpWithMode, code: &u8, len: &u8, addr_mode: &AddressingMode) -> String {
    let (mem_addr, stored_value) = match addr_mode {
        AddressingMode::Immediate | AddressingMode::Accumulator => (0, 0),
        _ => {
            let addr = get_operand_addr(cpu, ppu, cartridge, &addr_mode, pc + 1);
            (addr, mem_read(cpu, ppu, cartridge, addr))
        }
    };

    let hex = match len {
        1 => format!("{:02X}", mem_read(cpu, ppu, cartridge, pc)),
        2 => format!("{:02X} {:02X}", mem_read(cpu, ppu, cartridge, pc), mem_read(cpu, ppu, cartridge, pc+1)),
        3 => format!("{:02X} {:02X} {:02X}", mem_read(cpu, ppu, cartridge, pc), mem_read(cpu, ppu, cartridge, pc+1), mem_read(cpu, ppu, cartridge, pc+2)),
        _ => panic!("too long no mode opcode"),
    };

    let ex = match len {
        1 => match *code {
            0x0a | 0x4a | 0x2a | 0x6a => format!("A "),
            _ => String::from(""),
        },
        2 => {
            let address = mem_read(cpu, ppu, cartridge, pc + 1);

            match addr_mode {
                AddressingMode::Immediate => format!("#${:02X}", address),
                AddressingMode::ZeroPage => format!("${:02X} = {:02X}", mem_addr, stored_value),
                AddressingMode::ZeroPage_X => format!(
                    "${:02X},X @ {:02X} = {:02X}",
                    address, mem_addr, stored_value
                ),
                AddressingMode::ZeroPage_Y => format!(
                    "${:02X},Y @ {:02X} = {:02X}",
                    address, mem_addr, stored_value
                ),
                AddressingMode::Indirect_X => format!(
                    "(${:02X},X) @ {:02X} = {:04X} = {:02X}",
                    address,
                    (address.wrapping_add(cpu.register_x)),
                    mem_addr,
                    stored_value
                ),
                AddressingMode::Indirect_Y => format!(
                    "(${:02X}),Y = {:04X} @ {:04X} = {:02X}",
                    address,
                    (mem_addr.wrapping_sub(cpu.register_y as u16)),
                    mem_addr,
                    stored_value
                ),
                _ => panic!("unexpected addressing mode {:?} has op len 2 {:02X}", addr_mode, code),
            }
        },
        3 => {
            let address = mem_read_u16(cpu, ppu, cartridge, pc + 1);

            match addr_mode {
                AddressingMode::Absolute => match code {
                    0x4c => format!("${:04X}", address),
                    _ => format!("${:04X} = {:02X}", mem_addr, stored_value),
                },
                AddressingMode::Absolute_X => format!(
                    "${:04X},X @ {:04X} = {:02X}",
                    address, mem_addr, stored_value
                ),
                AddressingMode::Absolute_Y => format!(
                    "${:04X},Y @ {:04X} = {:02X}",
                    address, mem_addr, stored_value
                ),
                AddressingMode::Indirect => {
                    let jmp_addr = if address & 0x00FF == 0x00FF {
                        let lo = mem_read(cpu, ppu, cartridge, address);
                        let hi = mem_read(cpu, ppu, cartridge, address & 0xFF00);
                        (hi as u16) << 8 | (lo as u16)
                    } else {
                        mem_read_u16(cpu, ppu, cartridge, address)
                    };
                    format!("(${:04X}) = {:04X}", address, jmp_addr)
                },
                _ => panic!("unexpected addressing mode {:?} has ops-len 3. code {:02X}", addr_mode, code), 
            }
        },
        _ => String::from(""),
    };

    format!("{:8}  {: >4?} {}", hex, op, ex)
}

fn get_cpu_opcode(cpu: &mut CPU, ppu: &mut PPU, cartridge: &mut Cartridge) -> String {
    let pc = cpu.program_counter;
    let op_hex = mem_read(cpu, ppu, cartridge, pc);
    let opcode = OPCODE_MAP.get(&op_hex).unwrap_or_else(|| panic!("Unimplemented opcode 0x{:02X}", op_hex));
    let hex = match opcode {
        Opcode::Op { op, code, len, cycles: _ } => get_opcode_no_mode(cpu, ppu, cartridge,  pc, op, code, len),
        Opcode::OpWithMode { op, code, len, cycles: _, mode } => get_opcode_with_mode(cpu, ppu, cartridge,  pc, op, code, len, mode),
    };
    format!("{:04X}  {}", pc, hex)
}

fn get_cpu_registers(cpu: &mut CPU) -> String {
    format!("A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}", 
        cpu.register_a, cpu.register_x, cpu.register_y, cpu.status.bits(), cpu.stack_pointer)
}

fn get_cpu_trace(cpu: &mut CPU, ppu: &mut PPU, cartridge: &mut Cartridge) -> String {
    let opcode_str = get_cpu_opcode(cpu, ppu, cartridge);
    let register_str = get_cpu_registers(cpu);
    format!("{:47} {}", opcode_str, register_str)
}


#[test]
fn run_trace_test() {
    let mut log_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    log_path.push("resources/tests/nestest.log");

    let log_file = File::open(log_path).unwrap();
    let mut log_lines = BufReader::new(log_file).lines();

    let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    d.push("resources/tests/nestest.nes");

    let cart_data = fs::read(d).unwrap();

    let mut nestest_cart = cart::from_binary(&cart_data).unwrap();
    let mut cpu = CPU::default();
    let mut ppu = PPU::default();

    // Initialization for the NESTEST suite
    reset_cpu(&mut cpu, 0xC000);
    cpu.status = CpuFlags::from_bits(0x24).unwrap();

    // NESTEST ends at address 0xC66E
    while cpu.program_counter != 0xC66E {
        let trace = get_cpu_trace(&mut cpu, &mut ppu, &mut nestest_cart);
        // Specifically trimming the reference to remove PPU/CPU cycle counts
        let mut reference = String::from(&log_lines.next().unwrap().unwrap()[..73]);
        reference = str::replace(&reference, "*", " ");
        assert_eq!(trace, reference);
        println!("{}", trace);
        step_cpu(&mut cpu, &mut ppu, &mut nestest_cart);
    }
}