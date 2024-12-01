#[derive(Debug)]
struct BitsIter<'a, 'b> {
    bits: &'a mut Bits<'b>,
}

#[derive(Debug)]
pub struct Bits<'a> {
    entry: &'a Vec<u8>,
    byte: usize,
    offset: u8,
}

pub fn from_entry<'a>(entry: &'a Vec<u8>) -> Bits<'a> {
    Bits {
        entry: entry,
        byte: 0,
        offset: 0,
    }
}

impl<'a> Bits<'a> {
    // One iter at a time because of mut borrowing
    fn iter<'b>(&'b mut self) -> BitsIter<'b, 'a> {
        BitsIter { bits: self }
    }

    pub fn pos(&self) -> usize {
        self.byte * 8usize + self.offset as usize
    }

    #[allow(dead_code)]
    pub fn reset(&mut self) {
        self.byte = 0;
        self.offset = 0;
    }

    pub fn take(&mut self, n: usize) -> usize {
        self.iter()
            .take(n)
            .fold(0usize, |acc, b| (acc << 1) + b as usize)
    }
}

impl<'a, 'b> Iterator for BitsIter<'a, 'b> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self.bits.byte < self.bits.entry.len() {
            let by = self.bits.byte;
            let b = self.bits.offset;
            self.bits.offset += 1;
            if self.bits.offset == 8 {
                self.bits.offset = 0;
                self.bits.byte += 1;
            }
            Some((self.bits.entry[by] >> (7 - b)) & 1)
        } else {
            None
        }
    }
}
