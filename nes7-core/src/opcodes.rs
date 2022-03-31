use std::collections::HashMap;

use crate::cpu::AddressingMode;
use lazy_static::lazy_static;

pub enum Opcode {
    Op { op: Op, code: u8, len: u8, cycles: u8 },
    OpWithMode { op: OpWithMode, code: u8, len: u8, cycles: u8, mode: AddressingMode },
}

#[derive(Debug)]
pub enum Op { BCC, BCS, BEQ, BMI, BNE, BPL, BRK, BVC, BVS, CLC, CLD, CLI, CLV, DEX, DEY, INX, INY, JSR, NOP, PHA, PHP, PLA, PLP, RTI, RTS, SEC, SED, SEI, TAX, TAY, TSX, TXA, TXS, TYA }
#[derive(Debug)]
pub enum OpWithMode { ADC, AND, ASL, BIT, CMP, CPX, CPY, DEC, EOR, INC, JMP, LDA, LDX, LDY, LSR, ORA, ROL, ROR, SBC, STA, STX, STY, ALR, ANC, ANE, ARR, DCP, ISB, LAS, LAX, LXA, RLA, RRA, SAX, SBX, SHA, SHX, SHY, SLO, SRE, TAS, NOP }


lazy_static! {
    pub static ref OPCODES: Vec<Opcode> = vec![
        Opcode::OpWithMode { op: OpWithMode::ADC, code: 0x69, len: 2, cycles: 2, mode: AddressingMode::Immediate },
        Opcode::OpWithMode { op: OpWithMode::ADC, code: 0x65, len: 2, cycles: 3, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::ADC, code: 0x75, len: 2, cycles: 4, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::ADC, code: 0x6D, len: 3, cycles: 4, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::ADC, code: 0x7D, len: 3, cycles: 4, mode: AddressingMode::Absolute_X },
        Opcode::OpWithMode { op: OpWithMode::ADC, code: 0x79, len: 3, cycles: 4, mode: AddressingMode::Absolute_Y },
        Opcode::OpWithMode { op: OpWithMode::ADC, code: 0x61, len: 2, cycles: 6, mode: AddressingMode::Indirect_X },
        Opcode::OpWithMode { op: OpWithMode::ADC, code: 0x71, len: 2, cycles: 5, mode: AddressingMode::Indirect_Y },

        Opcode::OpWithMode { op: OpWithMode::AND, code: 0x29, len: 2, cycles: 2, mode: AddressingMode::Immediate },
        Opcode::OpWithMode { op: OpWithMode::AND, code: 0x25, len: 2, cycles: 3, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::AND, code: 0x35, len: 2, cycles: 4, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::AND, code: 0x2D, len: 3, cycles: 4, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::AND, code: 0x3D, len: 3, cycles: 4, mode: AddressingMode::Absolute_X },
        Opcode::OpWithMode { op: OpWithMode::AND, code: 0x39, len: 3, cycles: 4, mode: AddressingMode::Absolute_Y },
        Opcode::OpWithMode { op: OpWithMode::AND, code: 0x21, len: 2, cycles: 6, mode: AddressingMode::Indirect_X },
        Opcode::OpWithMode { op: OpWithMode::AND, code: 0x31, len: 2, cycles: 5, mode: AddressingMode::Indirect_Y },

        Opcode::OpWithMode { op: OpWithMode::ASL, code: 0x0A, len: 1, cycles: 2, mode: AddressingMode::Accumulator },
        Opcode::OpWithMode { op: OpWithMode::ASL, code: 0x06, len: 2, cycles: 5, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::ASL, code: 0x16, len: 2, cycles: 6, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::ASL, code: 0x0E, len: 3, cycles: 6, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::ASL, code: 0x1E, len: 3, cycles: 7, mode: AddressingMode::Absolute_X },

        Opcode::Op { op: Op::BCC, code: 0x90, len: 2, cycles: 2 },

        Opcode::Op { op: Op::BCS, code: 0xB0, len: 2, cycles: 2 },

        Opcode::Op { op: Op::BEQ, code: 0xF0, len: 2, cycles: 2 },

        Opcode::OpWithMode { op: OpWithMode::BIT, code: 0x24, len: 2, cycles: 3, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::BIT, code: 0x2C, len: 3, cycles: 4, mode: AddressingMode::Absolute },

        Opcode::Op { op: Op::BMI, code: 0x30, len: 2, cycles: 2 },

        Opcode::Op { op: Op::BNE, code: 0xD0, len: 2, cycles: 2 },

        Opcode::Op { op: Op::BPL, code: 0x10, len: 2, cycles: 2 },

        Opcode::Op { op: Op::BRK, code: 0x00, len: 1, cycles: 7 },

        Opcode::Op { op: Op::BVC, code: 0x50, len: 2, cycles: 2 },

        Opcode::Op { op: Op::BVS, code: 0x70, len: 2, cycles: 2 },

        Opcode::Op { op: Op::CLC, code: 0x18, len: 1, cycles: 2 },

        Opcode::Op { op: Op::CLD, code: 0xD8, len: 1, cycles: 2 },

        Opcode::Op { op: Op::CLI, code: 0x58, len: 1, cycles: 2 },

        Opcode::Op { op: Op::CLV, code: 0xB8, len: 1, cycles: 2 },

        Opcode::OpWithMode { op: OpWithMode::CMP, code: 0xC9, len: 2, cycles: 2, mode: AddressingMode::Immediate },
        Opcode::OpWithMode { op: OpWithMode::CMP, code: 0xC5, len: 2, cycles: 3, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::CMP, code: 0xD5, len: 2, cycles: 4, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::CMP, code: 0xCD, len: 3, cycles: 4, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::CMP, code: 0xDD, len: 3, cycles: 4, mode: AddressingMode::Absolute_X },
        Opcode::OpWithMode { op: OpWithMode::CMP, code: 0xD9, len: 3, cycles: 4, mode: AddressingMode::Absolute_Y },
        Opcode::OpWithMode { op: OpWithMode::CMP, code: 0xC1, len: 2, cycles: 6, mode: AddressingMode::Indirect_X },
        Opcode::OpWithMode { op: OpWithMode::CMP, code: 0xD1, len: 2, cycles: 5, mode: AddressingMode::Indirect_Y },

        Opcode::OpWithMode { op: OpWithMode::CPX, code: 0xE0, len: 2, cycles: 2, mode: AddressingMode::Immediate },
        Opcode::OpWithMode { op: OpWithMode::CPX, code: 0xE4, len: 2, cycles: 3, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::CPX, code: 0xEC, len: 3, cycles: 4, mode: AddressingMode::Absolute },

        Opcode::OpWithMode { op: OpWithMode::CPY, code: 0xC0, len: 2, cycles: 2, mode: AddressingMode::Immediate },
        Opcode::OpWithMode { op: OpWithMode::CPY, code: 0xC4, len: 2, cycles: 3, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::CPY, code: 0xCC, len: 3, cycles: 4, mode: AddressingMode::Absolute },

        Opcode::OpWithMode { op: OpWithMode::DEC, code: 0xC6, len: 2, cycles: 5, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::DEC, code: 0xD6, len: 2, cycles: 6, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::DEC, code: 0xCE, len: 3, cycles: 6, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::DEC, code: 0xDE, len: 3, cycles: 7, mode: AddressingMode::Absolute_X },

        Opcode::Op { op: Op::DEX, code: 0xCA, len: 1, cycles: 2 },

        Opcode::Op { op: Op::DEY, code: 0x88, len: 1, cycles: 2 },

        Opcode::OpWithMode { op: OpWithMode::EOR, code: 0x49, len: 2, cycles: 2, mode: AddressingMode::Immediate },
        Opcode::OpWithMode { op: OpWithMode::EOR, code: 0x45, len: 2, cycles: 3, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::EOR, code: 0x55, len: 2, cycles: 4, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::EOR, code: 0x4D, len: 3, cycles: 4, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::EOR, code: 0x5D, len: 3, cycles: 4, mode: AddressingMode::Absolute_X },
        Opcode::OpWithMode { op: OpWithMode::EOR, code: 0x59, len: 3, cycles: 4, mode: AddressingMode::Absolute_Y },
        Opcode::OpWithMode { op: OpWithMode::EOR, code: 0x41, len: 2, cycles: 6, mode: AddressingMode::Indirect_X },
        Opcode::OpWithMode { op: OpWithMode::EOR, code: 0x51, len: 2, cycles: 5, mode: AddressingMode::Indirect_Y },

        Opcode::OpWithMode { op: OpWithMode::INC, code: 0xE6, len: 2, cycles: 5, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::INC, code: 0xF6, len: 2, cycles: 6, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::INC, code: 0xEE, len: 3, cycles: 6, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::INC, code: 0xFE, len: 3, cycles: 7, mode: AddressingMode::Absolute_X },

        Opcode::Op { op: Op::INX, code: 0xE8, len: 1, cycles: 2 },

        Opcode::Op { op: Op::INY, code: 0xC8, len: 1, cycles: 2 },

        Opcode::OpWithMode { op: OpWithMode::JMP, code: 0x4C, len: 3, cycles: 3, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::JMP, code: 0x6C, len: 3, cycles: 5, mode: AddressingMode::Indirect },

        Opcode::Op { op: Op::JSR, code: 0x20, len: 3, cycles: 6 },

        Opcode::OpWithMode { op: OpWithMode::LDA, code: 0xA9, len: 2, cycles: 2, mode: AddressingMode::Immediate },
        Opcode::OpWithMode { op: OpWithMode::LDA, code: 0xA5, len: 2, cycles: 3, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::LDA, code: 0xB5, len: 2, cycles: 4, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::LDA, code: 0xAD, len: 3, cycles: 4, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::LDA, code: 0xBD, len: 3, cycles: 4, mode: AddressingMode::Absolute_X },
        Opcode::OpWithMode { op: OpWithMode::LDA, code: 0xB9, len: 3, cycles: 4, mode: AddressingMode::Absolute_Y },
        Opcode::OpWithMode { op: OpWithMode::LDA, code: 0xA1, len: 2, cycles: 6, mode: AddressingMode::Indirect_X },
        Opcode::OpWithMode { op: OpWithMode::LDA, code: 0xB1, len: 2, cycles: 5, mode: AddressingMode::Indirect_Y },

        Opcode::OpWithMode { op: OpWithMode::LDX, code: 0xA2, len: 2, cycles: 2, mode: AddressingMode::Immediate },
        Opcode::OpWithMode { op: OpWithMode::LDX, code: 0xA6, len: 2, cycles: 3, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::LDX, code: 0xB6, len: 2, cycles: 4, mode: AddressingMode::ZeroPage_Y },
        Opcode::OpWithMode { op: OpWithMode::LDX, code: 0xAE, len: 3, cycles: 4, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::LDX, code: 0xBE, len: 3, cycles: 4, mode: AddressingMode::Absolute_Y },

        Opcode::OpWithMode { op: OpWithMode::LDY, code: 0xA0, len: 2, cycles: 2, mode: AddressingMode::Immediate },
        Opcode::OpWithMode { op: OpWithMode::LDY, code: 0xA4, len: 2, cycles: 3, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::LDY, code: 0xB4, len: 2, cycles: 4, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::LDY, code: 0xAC, len: 3, cycles: 4, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::LDY, code: 0xBC, len: 3, cycles: 4, mode: AddressingMode::Absolute_X },

        Opcode::OpWithMode { op: OpWithMode::LSR, code: 0x4A, len: 1, cycles: 2, mode: AddressingMode::Accumulator },
        Opcode::OpWithMode { op: OpWithMode::LSR, code: 0x46, len: 2, cycles: 5, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::LSR, code: 0x56, len: 2, cycles: 6, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::LSR, code: 0x4E, len: 3, cycles: 6, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::LSR, code: 0x5E, len: 3, cycles: 7, mode: AddressingMode::Absolute_X },

        Opcode::Op { op: Op::NOP, code: 0xEA, len: 1, cycles: 2 },

        Opcode::OpWithMode { op: OpWithMode::ORA, code: 0x09, len: 2, cycles: 2, mode: AddressingMode::Immediate },
        Opcode::OpWithMode { op: OpWithMode::ORA, code: 0x05, len: 2, cycles: 3, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::ORA, code: 0x15, len: 2, cycles: 4, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::ORA, code: 0x0D, len: 3, cycles: 4, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::ORA, code: 0x1D, len: 3, cycles: 4, mode: AddressingMode::Absolute_X },
        Opcode::OpWithMode { op: OpWithMode::ORA, code: 0x19, len: 3, cycles: 4, mode: AddressingMode::Absolute_Y },
        Opcode::OpWithMode { op: OpWithMode::ORA, code: 0x01, len: 2, cycles: 6, mode: AddressingMode::Indirect_X },
        Opcode::OpWithMode { op: OpWithMode::ORA, code: 0x11, len: 2, cycles: 5, mode: AddressingMode::Indirect_Y },

        Opcode::Op { op: Op::PHA, code: 0x48, len: 1, cycles: 3 },

        Opcode::Op { op: Op::PHP, code: 0x08, len: 1, cycles: 3 },

        Opcode::Op { op: Op::PLA, code: 0x68, len: 1, cycles: 4 },

        Opcode::Op { op: Op::PLP, code: 0x28, len: 1, cycles: 4 },

        Opcode::OpWithMode { op: OpWithMode::ROL, code: 0x2A, len: 1, cycles: 2, mode: AddressingMode::Accumulator },
        Opcode::OpWithMode { op: OpWithMode::ROL, code: 0x26, len: 2, cycles: 5, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::ROL, code: 0x36, len: 2, cycles: 6, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::ROL, code: 0x2E, len: 3, cycles: 6, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::ROL, code: 0x3E, len: 3, cycles: 7, mode: AddressingMode::Absolute_X },

        Opcode::OpWithMode { op: OpWithMode::ROR, code: 0x6A, len: 1, cycles: 2, mode: AddressingMode::Accumulator },
        Opcode::OpWithMode { op: OpWithMode::ROR, code: 0x66, len: 2, cycles: 5, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::ROR, code: 0x76, len: 2, cycles: 6, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::ROR, code: 0x6E, len: 3, cycles: 6, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::ROR, code: 0x7E, len: 3, cycles: 7, mode: AddressingMode::Absolute_X },

        Opcode::Op { op: Op::RTI, code: 0x40, len: 1, cycles: 6 },

        Opcode::Op { op: Op::RTS, code: 0x60, len: 1, cycles: 6 },

        Opcode::OpWithMode { op: OpWithMode::SBC, code: 0xE9, len: 2, cycles: 2, mode: AddressingMode::Immediate },
        Opcode::OpWithMode { op: OpWithMode::SBC, code: 0xE5, len: 2, cycles: 3, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::SBC, code: 0xF5, len: 2, cycles: 4, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::SBC, code: 0xED, len: 3, cycles: 4, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::SBC, code: 0xFD, len: 3, cycles: 4, mode: AddressingMode::Absolute_X },
        Opcode::OpWithMode { op: OpWithMode::SBC, code: 0xF9, len: 3, cycles: 4, mode: AddressingMode::Absolute_Y },
        Opcode::OpWithMode { op: OpWithMode::SBC, code: 0xE1, len: 2, cycles: 6, mode: AddressingMode::Indirect_X },
        Opcode::OpWithMode { op: OpWithMode::SBC, code: 0xF1, len: 2, cycles: 5, mode: AddressingMode::Indirect_Y },

        Opcode::Op { op: Op::SEC, code: 0x38, len: 1, cycles: 2 },

        Opcode::Op { op: Op::SED, code: 0xF8, len: 1, cycles: 2 },

        Opcode::Op { op: Op::SEI, code: 0x78, len: 1, cycles: 2 },

        Opcode::OpWithMode { op: OpWithMode::STA, code: 0x85, len: 2, cycles: 3, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::STA, code: 0x95, len: 2, cycles: 4, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::STA, code: 0x8D, len: 3, cycles: 4, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::STA, code: 0x9D, len: 3, cycles: 5, mode: AddressingMode::Absolute_X },
        Opcode::OpWithMode { op: OpWithMode::STA, code: 0x99, len: 3, cycles: 5, mode: AddressingMode::Absolute_Y },
        Opcode::OpWithMode { op: OpWithMode::STA, code: 0x81, len: 2, cycles: 6, mode: AddressingMode::Indirect_X },
        Opcode::OpWithMode { op: OpWithMode::STA, code: 0x91, len: 2, cycles: 6, mode: AddressingMode::Indirect_Y },

        Opcode::OpWithMode { op: OpWithMode::STX, code: 0x86, len: 2, cycles: 3, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::STX, code: 0x96, len: 2, cycles: 4, mode: AddressingMode::ZeroPage_Y },
        Opcode::OpWithMode { op: OpWithMode::STX, code: 0x8E, len: 3, cycles: 4, mode: AddressingMode::Absolute },

        Opcode::OpWithMode { op: OpWithMode::STY, code: 0x84, len: 2, cycles: 3, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::STY, code: 0x94, len: 2, cycles: 4, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::STY, code: 0x8C, len: 3, cycles: 4, mode: AddressingMode::Absolute },

        Opcode::Op { op: Op::TAX, code: 0xAA, len: 1, cycles: 2 },

        Opcode::Op { op: Op::TAY, code: 0xA8, len: 1, cycles: 2 },

        Opcode::Op { op: Op::TSX, code: 0xBA, len: 1, cycles: 2 },

        Opcode::Op { op: Op::TXA, code: 0x8A, len: 1, cycles: 2 },

        Opcode::Op { op: Op::TXS, code: 0x9A, len: 1, cycles: 2 },

        Opcode::Op { op: Op::TYA, code: 0x98, len: 1, cycles: 2 },
    ];

    pub static ref ILLEGAL_OPCODES: Vec<Opcode> = vec![
        Opcode::OpWithMode { op: OpWithMode::ALR, code: 0x4B, len: 2, cycles: 2, mode: AddressingMode::Immediate },

        Opcode::OpWithMode { op: OpWithMode::ANC, code: 0x0B, len: 2, cycles: 2, mode: AddressingMode::Immediate },
        Opcode::OpWithMode { op: OpWithMode::ANC, code: 0x2B, len: 2, cycles: 2, mode: AddressingMode::Immediate },

        Opcode::OpWithMode { op: OpWithMode::ANE, code: 0x8B, len: 2, cycles: 2, mode: AddressingMode::Immediate },

        Opcode::OpWithMode { op: OpWithMode::ARR, code: 0x6B, len: 2, cycles: 2, mode: AddressingMode::Immediate },

        Opcode::OpWithMode { op: OpWithMode::DCP, code: 0xC7, len: 2, cycles: 5, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::DCP, code: 0xD7, len: 2, cycles: 6, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::DCP, code: 0xCF, len: 3, cycles: 6, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::DCP, code: 0xDF, len: 3, cycles: 7, mode: AddressingMode::Absolute_X },
        Opcode::OpWithMode { op: OpWithMode::DCP, code: 0xDB, len: 3, cycles: 7, mode: AddressingMode::Absolute_Y },
        Opcode::OpWithMode { op: OpWithMode::DCP, code: 0xC3, len: 2, cycles: 8, mode: AddressingMode::Indirect_X },
        Opcode::OpWithMode { op: OpWithMode::DCP, code: 0xD3, len: 2, cycles: 8, mode: AddressingMode::Indirect_Y },

        Opcode::OpWithMode { op: OpWithMode::ISB, code: 0xE7, len: 2, cycles: 5, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::ISB, code: 0xF7, len: 2, cycles: 6, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::ISB, code: 0xEF, len: 3, cycles: 6, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::ISB, code: 0xFF, len: 3, cycles: 7, mode: AddressingMode::Absolute_X },
        Opcode::OpWithMode { op: OpWithMode::ISB, code: 0xFB, len: 3, cycles: 7, mode: AddressingMode::Absolute_Y },
        Opcode::OpWithMode { op: OpWithMode::ISB, code: 0xE3, len: 2, cycles: 8, mode: AddressingMode::Indirect_X },
        Opcode::OpWithMode { op: OpWithMode::ISB, code: 0xF3, len: 2, cycles: 4, mode: AddressingMode::Indirect_Y },

        Opcode::OpWithMode { op: OpWithMode::LAS, code: 0xBB, len: 3, cycles: 4, mode: AddressingMode::Absolute_Y },

        Opcode::OpWithMode { op: OpWithMode::LAX, code: 0xA7, len: 2, cycles: 3, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::LAX, code: 0xB7, len: 2, cycles: 4, mode: AddressingMode::ZeroPage_Y },
        Opcode::OpWithMode { op: OpWithMode::LAX, code: 0xAF, len: 3, cycles: 4, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::LAX, code: 0xBF, len: 3, cycles: 4, mode: AddressingMode::Absolute_Y },
        Opcode::OpWithMode { op: OpWithMode::LAX, code: 0xA3, len: 2, cycles: 6, mode: AddressingMode::Indirect_X },
        Opcode::OpWithMode { op: OpWithMode::LAX, code: 0xB3, len: 2, cycles: 5, mode: AddressingMode::Indirect_Y },

        Opcode::OpWithMode { op: OpWithMode::LXA, code: 0xAB, len: 2, cycles: 2, mode: AddressingMode::Immediate },

        Opcode::OpWithMode { op: OpWithMode::RLA, code: 0x27, len: 2, cycles: 5, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::RLA, code: 0x37, len: 2, cycles: 6, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::RLA, code: 0x2F, len: 3, cycles: 6, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::RLA, code: 0x3F, len: 3, cycles: 7, mode: AddressingMode::Absolute_X },
        Opcode::OpWithMode { op: OpWithMode::RLA, code: 0x3B, len: 3, cycles: 7, mode: AddressingMode::Absolute_Y },
        Opcode::OpWithMode { op: OpWithMode::RLA, code: 0x23, len: 2, cycles: 8, mode: AddressingMode::Indirect_X },
        Opcode::OpWithMode { op: OpWithMode::RLA, code: 0x33, len: 2, cycles: 8, mode: AddressingMode::Indirect_Y },

        Opcode::OpWithMode { op: OpWithMode::RRA, code: 0x67, len: 2, cycles: 5, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::RRA, code: 0x77, len: 2, cycles: 6, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::RRA, code: 0x6F, len: 3, cycles: 6, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::RRA, code: 0x7F, len: 3, cycles: 7, mode: AddressingMode::Absolute_X },
        Opcode::OpWithMode { op: OpWithMode::RRA, code: 0x7B, len: 3, cycles: 7, mode: AddressingMode::Absolute_Y },
        Opcode::OpWithMode { op: OpWithMode::RRA, code: 0x63, len: 2, cycles: 8, mode: AddressingMode::Indirect_X },
        Opcode::OpWithMode { op: OpWithMode::RRA, code: 0x73, len: 2, cycles: 8, mode: AddressingMode::Indirect_Y },
        
        Opcode::OpWithMode { op: OpWithMode::SAX, code: 0x87, len: 2, cycles: 3, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::SAX, code: 0x97, len: 2, cycles: 4, mode: AddressingMode::ZeroPage_Y },
        Opcode::OpWithMode { op: OpWithMode::SAX, code: 0x8F, len: 3, cycles: 4, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::SAX, code: 0x83, len: 2, cycles: 6, mode: AddressingMode::Indirect_X },

        Opcode::OpWithMode { op: OpWithMode::SBX, code: 0xCB, len: 2, cycles: 2, mode: AddressingMode::Immediate },

        Opcode::OpWithMode { op: OpWithMode::SHA, code: 0x9F, len: 3, cycles: 5, mode: AddressingMode::Absolute_Y },
        Opcode::OpWithMode { op: OpWithMode::SHA, code: 0x93, len: 2, cycles: 6, mode: AddressingMode::Indirect_Y },

        Opcode::OpWithMode { op: OpWithMode::SHX, code: 0x9E, len: 3, cycles: 5, mode: AddressingMode::Absolute_Y },

        Opcode::OpWithMode { op: OpWithMode::SHY, code: 0x9C, len: 3, cycles: 5, mode: AddressingMode::Absolute_X },

        Opcode::OpWithMode { op: OpWithMode::SLO, code: 0x07, len: 2, cycles: 5, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::SLO, code: 0x17, len: 2, cycles: 6, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::SLO, code: 0x0F, len: 3, cycles: 6, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::SLO, code: 0x1F, len: 3, cycles: 7, mode: AddressingMode::Absolute_X },
        Opcode::OpWithMode { op: OpWithMode::SLO, code: 0x1B, len: 3, cycles: 7, mode: AddressingMode::Absolute_Y },
        Opcode::OpWithMode { op: OpWithMode::SLO, code: 0x03, len: 2, cycles: 8, mode: AddressingMode::Indirect_X },
        Opcode::OpWithMode { op: OpWithMode::SLO, code: 0x13, len: 2, cycles: 8, mode: AddressingMode::Indirect_Y },

        Opcode::OpWithMode { op: OpWithMode::SRE, code: 0x47, len: 2, cycles: 5, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::SRE, code: 0x57, len: 2, cycles: 6, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::SRE, code: 0x4F, len: 3, cycles: 6, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::SRE, code: 0x5F, len: 3, cycles: 7, mode: AddressingMode::Absolute_X },
        Opcode::OpWithMode { op: OpWithMode::SRE, code: 0x5B, len: 3, cycles: 7, mode: AddressingMode::Absolute_Y },
        Opcode::OpWithMode { op: OpWithMode::SRE, code: 0x43, len: 2, cycles: 8, mode: AddressingMode::Indirect_X },
        Opcode::OpWithMode { op: OpWithMode::SRE, code: 0x53, len: 2, cycles: 8, mode: AddressingMode::Indirect_Y },

        Opcode::OpWithMode { op: OpWithMode::TAS, code: 0x9B, len: 3, cycles: 5, mode: AddressingMode::Absolute_Y },

        Opcode::OpWithMode { op: OpWithMode::SBC, code: 0xEB, len: 2, cycles: 2, mode: AddressingMode::Immediate },

        Opcode::Op { op: Op::NOP, code: 0x1A, len: 1, cycles: 2 },
        Opcode::Op { op: Op::NOP, code: 0x3A, len: 1, cycles: 2 },
        Opcode::Op { op: Op::NOP, code: 0x5A, len: 1, cycles: 2 },
        Opcode::Op { op: Op::NOP, code: 0x7A, len: 1, cycles: 2 },
        Opcode::Op { op: Op::NOP, code: 0xDA, len: 1, cycles: 2 },
        Opcode::Op { op: Op::NOP, code: 0xFA, len: 1, cycles: 2 },
        Opcode::OpWithMode { op: OpWithMode::NOP, code: 0x80, len: 2, cycles: 2, mode: AddressingMode::Immediate },
        Opcode::OpWithMode { op: OpWithMode::NOP, code: 0x82, len: 2, cycles: 2, mode: AddressingMode::Immediate },
        Opcode::OpWithMode { op: OpWithMode::NOP, code: 0x89, len: 2, cycles: 2, mode: AddressingMode::Immediate },
        Opcode::OpWithMode { op: OpWithMode::NOP, code: 0xC2, len: 2, cycles: 2, mode: AddressingMode::Immediate },
        Opcode::OpWithMode { op: OpWithMode::NOP, code: 0xE2, len: 2, cycles: 2, mode: AddressingMode::Immediate },
        Opcode::OpWithMode { op: OpWithMode::NOP, code: 0x04, len: 2, cycles: 3, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::NOP, code: 0x44, len: 2, cycles: 3, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::NOP, code: 0x64, len: 2, cycles: 3, mode: AddressingMode::ZeroPage },
        Opcode::OpWithMode { op: OpWithMode::NOP, code: 0x14, len: 2, cycles: 4, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::NOP, code: 0x34, len: 2, cycles: 4, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::NOP, code: 0x54, len: 2, cycles: 4, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::NOP, code: 0x74, len: 2, cycles: 4, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::NOP, code: 0xD4, len: 2, cycles: 4, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::NOP, code: 0xF4, len: 2, cycles: 4, mode: AddressingMode::ZeroPage_X },
        Opcode::OpWithMode { op: OpWithMode::NOP, code: 0x0C, len: 3, cycles: 4, mode: AddressingMode::Absolute },
        Opcode::OpWithMode { op: OpWithMode::NOP, code: 0x1C, len: 3, cycles: 4, mode: AddressingMode::Absolute_X },
        Opcode::OpWithMode { op: OpWithMode::NOP, code: 0x3C, len: 3, cycles: 4, mode: AddressingMode::Absolute_X },
        Opcode::OpWithMode { op: OpWithMode::NOP, code: 0x5C, len: 3, cycles: 4, mode: AddressingMode::Absolute_X },
        Opcode::OpWithMode { op: OpWithMode::NOP, code: 0x7C, len: 3, cycles: 4, mode: AddressingMode::Absolute_X },
        Opcode::OpWithMode { op: OpWithMode::NOP, code: 0xDC, len: 3, cycles: 4, mode: AddressingMode::Absolute_X },
        Opcode::OpWithMode { op: OpWithMode::NOP, code: 0xFC, len: 3, cycles: 4, mode: AddressingMode::Absolute_X },
    ];

    pub static ref OPCODE_MAP: HashMap<u8, &'static Opcode> = {
        let mut map = HashMap::new();
        for opcode in &*OPCODES {
            match opcode {
                 Opcode::Op { op: _,  code, len: _, cycles: _ } => map.insert(*code, opcode),
                 Opcode::OpWithMode { op: _, code, len: _, cycles: _, mode: _ } => map.insert(*code, opcode),
            };
        }
        for opcode in &*ILLEGAL_OPCODES {
            match opcode {
                Opcode::Op { op: _,  code, len: _, cycles: _ } => map.insert(*code, opcode),
                Opcode::OpWithMode { op: _, code, len: _, cycles: _, mode: _ } => map.insert(*code, opcode),
            };
        }
        map
    };
}