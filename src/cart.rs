use crate::cpu::Mem;

#[derive(Debug, PartialEq, Clone)]
pub enum Mirroring {
    VERTICAL,
    HORIZONTAL,
    FOUR,
}

// Magic string used to identify valid NES files
const NES_TAG: [u8; 4] = [0x4e, 0x45, 0x53, 0x1a];

// Page sizes for both ROM types
const PRG_PAGE_SIZE: usize = 16384;
const CHR_PAGE_SIZE: usize = 8192;

pub struct Cartridge {
    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
    pub mapper: u8,
    pub mirroring: Mirroring,
}

impl Cartridge {
    // Function to convert the hex data of a .nes file to a Cartridge
    pub fn new(data: &Vec<u8>) -> Result<Cartridge, String> {
        // Check magic string
        if &data[0..4] != NES_TAG {
            return Err("File is not in valid iNES format".to_string());
        }

        // Get the mapper this cartridge is using
        let mapper = (&data[7] & 0b1111_0000) | (&data[6] >> 4); 

        // The iNES rom type is specified here. Currently only supporting iNES 1.0
        let version = data[7] >> 2 & 0b0011;
        if version != 0 {
            return Err("iNES 2.0 is not supported".to_string());
        }

        // Figure out the miroring mode from bits 3 and 0 of byte 6
        let four_screen = data[6] & 0b1000 != 0;
        let vertical_mirror = data[6] & 0b0001 != 0;
        let mirroring = match (four_screen, vertical_mirror) {
            (true, _) => Mirroring::FOUR,
            (false, true) => Mirroring::VERTICAL,
            (false, false) => Mirroring::HORIZONTAL,
        };

        // Bytes 4 and 5 are the prg and chr rom sizes, in pages
        let prg_size = data[4] as usize * PRG_PAGE_SIZE;
        let chr_size = data[5] as usize * CHR_PAGE_SIZE;

        // Optional trainer rom section for Famicom support. Skip if present.
        let trainer_present = data[6] & 0b0100 != 0;

        let prg_start = 16 + if trainer_present { 512 } else { 0 };
        let chr_start = prg_start + prg_start;

        Ok(Cartridge {
            prg_rom: data[prg_start..(prg_start + prg_size)].to_vec(),
            chr_rom: data[chr_start..(chr_start + chr_size)].to_vec(),
            mapper,
            mirroring,
        })
    }
}


impl Mem for Cartridge {
    fn mem_read(&self, addr: u16) -> u8 {
        // The PRG rom is located from 0x8000 to 0xFFFF,
        // but potentially takes less space.
        // In that case, the PRG rom is mirrored over.
        let mut shifted_addr = addr - 0x8000;
        if shifted_addr >= 0x4000 && self.prg_rom.len() == 0x4000 {
            shifted_addr %= 0x4000;
        }
        self.prg_rom[shifted_addr as usize]
    }

    fn mem_write(&mut self, _: u16, _: u8) {
        panic!("Attempted to write to Cartridge ROM")
    }
}


#[cfg(test)]
pub mod test {
    use super::*;

    fn create_cart(header: Vec<u8>, trainer: bool, pgp: Vec<u8>, chr: Vec<u8>) -> Cartridge {
        let mut data = Vec::with_capacity(
            header.len()
            + if trainer { 512 } else { 0 }
            + pgp.len()
            + chr.len()
        );

        data.extend(header);
        if trainer {
            data.extend(vec![0; 512]);
        }
        data.extend(pgp);
        data.extend(chr);

        Cartridge::new(&data).unwrap()
    }

    pub fn test_cart() -> Cartridge {
        create_cart(vec![
            0x4E, 0x45, 0x53, 0x1A, 0x02, 0x01, 0x31, 00, 00, 00, 00, 00, 00, 00, 00, 00,
        ], false, vec![1; 2 * PRG_PAGE_SIZE], vec![2; 1 * CHR_PAGE_SIZE])
    }

    #[test]
    fn default_cart_correct_values() {
        let cart = test_cart();

        assert_eq!(cart.prg_rom.len(), 2 * PRG_PAGE_SIZE);
        assert_eq!(cart.chr_rom.len(), 1 * CHR_PAGE_SIZE);
        assert_eq!(cart.mirroring, Mirroring::VERTICAL);
        assert_eq!(cart.mapper, 3);
    }

    #[test]
    fn test_with_trainer() {
        let cart = create_cart(vec![
            0x4E, 0x45, 0x53, 0x1A, 0x02, 0x01, 0x31 | 0b100, 00, 00, 00, 00, 00, 00, 00, 00, 00,
        ], true, vec![1; 2 * PRG_PAGE_SIZE], vec![2; 1 * CHR_PAGE_SIZE]);

        assert_eq!(cart.prg_rom.len(), 2 * PRG_PAGE_SIZE);
        assert_eq!(cart.chr_rom.len(), 1 * CHR_PAGE_SIZE);
        assert_eq!(cart.mirroring, Mirroring::VERTICAL);
        assert_eq!(cart.mapper, 3);
    }
}