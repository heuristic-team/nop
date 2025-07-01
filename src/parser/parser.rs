use crate::lexer::Lexeme;

pub struct It {
    lexems: Vec<Lexeme>,
    offset: usize,
}

impl It {
    pub fn new(lexems: Vec<Lexeme>) -> Self {
        It { lexems, offset: 0 }
    }

    fn check_len(&self, length: usize) -> bool {
        if length >= self.lexems.len() {
            false
        } else {
            true
        }
    }

    pub fn next(&mut self) -> Option<&Lexeme> {
        if !self.check_len(self.offset) {
            return None;
        }
        self.offset += 1;
        Some(&self.lexems[self.offset - 1])
    }

    pub fn peek(&mut self) -> Option<&Lexeme> {
        if !self.check_len(self.offset + 1) {
            return None;
        }
        Some(&self.lexems[self.offset])
    }
}
