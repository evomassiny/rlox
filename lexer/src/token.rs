use super::source::Span;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

/// All Token variants accepted by the scanner
#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // one or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // litterals
    Identifier(String),
    Str(String),
    Number(f64),
    // keywords,
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Eof,
}
impl TokenKind {
    /// handcrafted DFA
    pub fn parse_string(identifier: String) -> Self {
        let mut id = identifier.chars();
        match id.next() {
            // 'and'
            Some('a') => {
                if id.next() == Some('n') && id.next() == Some('d') {
                    return Self::And;
                }
            }
            // 'class'
            Some('c') => {
                if id.next() == Some('l')
                    && id.next() == Some('a')
                    && id.next() == Some('s')
                    && id.next() == Some('s')
                {
                    return Self::Class;
                }
            }
            // 'else'
            Some('e') => {
                if id.next() == Some('l') && id.next() == Some('s') && id.next() == Some('e') {
                    return Self::Else;
                }
            }
            // 'for' | 'fun' | 'false'
            Some('f') => match id.next() {
                Some('o') => {
                    if id.next() == Some('r') {
                        return Self::For;
                    }
                }
                Some('u') => {
                    if id.next() == Some('n') {
                        return Self::Fun;
                    }
                }
                Some('a') => {
                    if id.next() == Some('l') && id.next() == Some('s') && id.next() == Some('e') {
                        return Self::False;
                    }
                }
                _ => return Self::Identifier(identifier),
            },
            // 'if'
            Some('i') => {
                if id.next() == Some('f') {
                    return Self::If;
                }
            }
            // 'nil'
            Some('n') => {
                if id.next() == Some('i') && id.next() == Some('l') {
                    return Self::Nil;
                }
            }
            // 'or'
            Some('o') => {
                if id.next() == Some('r') {
                    return Self::Or;
                }
            }
            // 'print'
            Some('p') => {
                if id.next() == Some('r')
                    && id.next() == Some('i')
                    && id.next() == Some('n')
                    && id.next() == Some('t')
                {
                    return Self::Print;
                }
            }
            // 'return'
            Some('r') => {
                if id.next() == Some('e')
                    && id.next() == Some('t')
                    && id.next() == Some('u')
                    && id.next() == Some('r')
                    && id.next() == Some('n')
                {
                    return Self::Return;
                }
            }
            // 'super'
            Some('s') => {
                if id.next() == Some('u')
                    && id.next() == Some('p')
                    && id.next() == Some('e')
                    && id.next() == Some('r')
                {
                    return Self::Super;
                }
            }
            // 'this' | 'true'
            Some('t') => match id.next() {
                Some('h') => {
                    if id.next() == Some('i') && id.next() == Some('s') {
                        return Self::This;
                    }
                }
                Some('r') => {
                    if id.next() == Some('u') && id.next() == Some('e') {
                        return Self::True;
                    }
                }
                _ => return Self::Identifier(identifier),
            },
            // 'var'
            Some('v') => {
                if id.next() == Some('a') && id.next() == Some('r') {
                    return Self::Var;
                }
            }
            // 'while'
            Some('w') => {
                if id.next() == Some('h')
                    && id.next() == Some('i')
                    && id.next() == Some('l')
                    && id.next() == Some('e')
                {
                    return Self::While;
                }
            }
            _ => return Self::Identifier(identifier),
        }
        Self::Identifier(identifier)
    }
}
