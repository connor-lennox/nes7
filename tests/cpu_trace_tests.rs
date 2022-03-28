use std::fs::{self, File};
use std::io::{BufReader, BufRead};
use std::path::PathBuf;

use nes7::bus::Bus;
use nes7::cpu::{CPU, Mem, AddressingMode, CpuFlags};
use nes7::opcodes::{Opcode, Op, OpWithMode, OPCODE_MAP};
use nes7::cart;


fn get_opcode_no_mode(cpu: &mut CPU, pc: u16, op: &Op, _: &u8, len: &u8) -> String {
    let hex = match len {
        1 => format!("{:02X}", cpu.mem_read(pc)),
        2 => format!("{:02X} {:02X}", cpu.mem_read(pc), cpu.mem_read(pc+1)),
        3 => format!("{:02X} {:02X} {:02X}", cpu.mem_read(pc), cpu.mem_read(pc+1), cpu.mem_read(pc+2)),
        _ => panic!("too long no mode opcode"),
    };

    let ex = match len {
        1 => String::from(""),
        2 => {
            // Local jumps
            let jmp = cpu.mem_read(pc + 1);
            let address = (pc as usize + 2).wrapping_add((jmp as i8) as usize);
            format!("${:04X}", address)
        },
        3 => {
            let address = cpu.mem_read_u16(pc + 1);
            format!("${:04X}", address)
        },
        _ => String::from(""),
    };

    format!("{:8}  {: >4?} {}", hex, op, ex)
}

fn get_opcode_with_mode(cpu: &mut CPU, pc: u16, op: &OpWithMode, code: &u8, len: &u8, addr_mode: &AddressingMode) -> String {
    let (mem_addr, stored_value) = match addr_mode {
        AddressingMode::Immediate | AddressingMode::Accumulator => (0, 0),
        _ => {
            let addr = cpu.get_operand_addr(&addr_mode, pc + 1);
            (addr, cpu.mem_read(addr))
        }
    };

    let hex = match len {
        1 => format!("{:02X}", cpu.mem_read(pc)),
        2 => format!("{:02X} {:02X}", cpu.mem_read(pc), cpu.mem_read(pc+1)),
        3 => format!("{:02X} {:02X} {:02X}", cpu.mem_read(pc), cpu.mem_read(pc+1), cpu.mem_read(pc+2)),
        _ => panic!("too long no mode opcode"),
    };

    let ex = match len {
        1 => match *code {
            0x0a | 0x4a | 0x2a | 0x6a => format!("A "),
            _ => String::from(""),
        },
        2 => {
            let address = cpu.mem_read(pc + 1);

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
            let address = cpu.mem_read_u16(pc + 1);

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
                        let lo = cpu.mem_read(address);
                        let hi = cpu.mem_read(address & 0xFF00);
                        (hi as u16) << 8 | (lo as u16)
                    } else {
                        cpu.mem_read_u16(address)
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

fn get_cpu_opcode(cpu: &mut CPU) -> String {
    let pc = cpu.program_counter;
    let op_hex = cpu.mem_read(pc);
    let opcode = OPCODE_MAP.get(&op_hex).unwrap_or_else(|| panic!("Unimplemented opcode 0x{:02X}", op_hex));
    let hex = match opcode {
        Opcode::Op { op, code, len, cycles: _ } => get_opcode_no_mode(cpu, pc, op, code, len),
        Opcode::OpWithMode { op, code, len, cycles: _, mode } => get_opcode_with_mode(cpu, pc, op, code, len, mode),
    };
    format!("{:04X}  {}", pc, hex)
}

fn get_cpu_registers(cpu: &mut CPU) -> String {
    format!("A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}", 
        cpu.register_a, cpu.register_x, cpu.register_y, cpu.status.bits(), cpu.stack_pointer)
}

fn get_cpu_trace(cpu: &mut CPU) -> String {
    let opcode_str = get_cpu_opcode(cpu);
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

    let nestest_cart = cart::from_binary(&cart_data).unwrap();

    let bus = Bus::new(nestest_cart);
    let mut cpu = CPU::new(bus);

    // Initialization for the NESTEST suite
    cpu.reset();
    cpu.program_counter = 0xC000;
    cpu.status = CpuFlags::from_bits(0x24).unwrap();

    // NESTEST ends at address 0xC66E
    while cpu.program_counter != 0xC6BC {
        let trace = get_cpu_trace(&mut cpu);
        // Specifically trimming the reference to remove PPU/CPU cycle counts
        let reference = String::from(&log_lines.next().unwrap().unwrap()[..73]);
        assert_eq!(trace, reference);
        println!("{}", trace);
        cpu.step();
    }
}