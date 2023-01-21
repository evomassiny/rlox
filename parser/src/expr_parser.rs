use super::ast::{Expr, ExprKind, LiteralKind};
use super::cursor::{Cursor, ParseError};
use lexer::{Span, Token, TokenKind, Tokenize};

/// precedence order
/// NOTE: higher precedence means less expressions.
/// eg: in `A*B+C`, * concerns 2 expressions, + concerns 4
#[repr(u8)]
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub enum Precedence {
    /// =
    Assignement = 0,
    /// or
    Or = 1,
    /// and
    And = 2,
    /// ==, !=
    Equality = 3,
    /// <, >, <=, >=
    Comparison = 4,
    /// +, -
    Term = 5,
    /// *, /
    Factor = 6,
    /// !, -
    Unary = 7,
    /// . ()
    Call = 8,
    Primary = 9,
}

/// Expression parser
/// Implements a Pratt parser,
/// using 2 tables of parsing rules
/// * one for infix expression
/// * one for prefix expression
pub struct ExprParser<'a>(&'a mut Cursor);

/// a function that parses an expression from a cursor, given a prefix TokenKind
type PrefixParserFn = fn(&mut Cursor, bool) -> Result<Expr, ParseError>;

/// a function that parses an expression from a cursor and the previously parsed
/// expression, given an infix TokenKind
type InfixParserFn = fn(&mut Cursor, Expr, bool) -> Result<Expr, ParseError>;

impl<'a> ExprParser<'a> {
    pub fn new(cursor: &'a mut Cursor) -> Self {
        Self(cursor)
    }

    pub fn parse(&mut self) -> Result<Expr, ParseError> {
        Self::parse_precedence(self.0, Precedence::Assignement)
    }

    fn parse_precedence(cursor: &mut Cursor, precedence: Precedence) -> Result<Expr, ParseError> {
        let _ = cursor.advance()?;
        let prefix_fn = Self::get_prefix_handler(&cursor.previous()?.kind)
            .ok_or(ParseError::ExpectedExpression)?;
        let can_assign = precedence <= Precedence::Assignement;

        let mut expr = prefix_fn(cursor, can_assign)?;
        dbg!(&expr);

        // Combine parsed expression with the next right expression,
        // if the next token is defines an infix expression.
        while let Some(next_expr_precedence) = Self::get_token_precedence(&cursor.current()?.kind) {
            if next_expr_precedence <= precedence {
                break;
            }
            dbg!("HERE");
            let _ = cursor.advance();
            // call the rule associated with handling
            // expressing **CONTAINING** this token
            let next_expr_parser_fn = Self::get_infix_handler(&cursor.previous()?.kind)
                .ok_or(ParseError::ExpectedExpression)?;
            expr = next_expr_parser_fn(cursor, expr, can_assign)?;
        }
        if can_assign && cursor.matches(TokenKind::Equal)? {
            return Err(ParseError::ExpectedToken(
                "Invalid assignement target.".into(),
            ));
        }
        Ok(expr)
    }

    /// Return, depending of the variant of `kind`
    /// which parsing routine we should use to parse the
    /// expression
    fn get_prefix_handler(kind: &TokenKind) -> Option<PrefixParserFn> {
        match kind {
            &TokenKind::LeftParen => todo!(),
            &TokenKind::Minus => todo!(),
            &TokenKind::Bang => todo!(),
            &TokenKind::Identifier(_) => todo!(),
            &TokenKind::Str(_) => todo!(),
            &TokenKind::Number(_) => Some(Self::parse_number),
            &TokenKind::False => todo!(),
            &TokenKind::Nil => todo!(),
            &TokenKind::Super => todo!(),
            &TokenKind::This => todo!(),
            &TokenKind::True => todo!(),
            _ => None,
        }
    }

    /// If `kind` could define an infix expression, retruns its
    /// "binding power", otherwise return None.
    fn get_token_precedence(kind: &TokenKind) -> Option<Precedence> {
        match kind {
            &TokenKind::Plus | &TokenKind::Minus => Some(Precedence::Term),
            &TokenKind::Star | &TokenKind::Slash => Some(Precedence::Factor),
            _ => None,
        }
    }

    /// Return, depending of the variant of `kind`
    /// which parsing routine we should use to parse the
    /// expression if we found the token while parsing a bigger expression
    fn get_infix_handler(kind: &TokenKind) -> Option<InfixParserFn> {
        match kind {
            &TokenKind::LeftParen => todo!(),
            &TokenKind::Dot => todo!(),
            &TokenKind::Minus => todo!(),
            &TokenKind::Plus => Some(Self::parse_sum),
            &TokenKind::Slash => todo!(),
            &TokenKind::Star => Some(Self::parse_product),
            &TokenKind::BangEqual => todo!(),
            &TokenKind::EqualEqual => todo!(),
            &TokenKind::Greater => todo!(),
            &TokenKind::GreaterEqual => todo!(),
            &TokenKind::Less => todo!(),
            &TokenKind::LessEqual => todo!(),
            &TokenKind::And => todo!(),
            &TokenKind::Or => todo!(),
            _ => None,
        }
    }

    fn parse_grouping(cursor: &mut Cursor, can_assign: bool) -> Result<Expr, ParseError> {
        todo!();
    }

    /// Build a Literal expression from a Number Token.
    fn parse_number(cursor: &mut Cursor, _can_assign: bool) -> Result<Expr, ParseError> {
        let Token { kind: TokenKind::Number(value), span } = cursor.take_previous()? else {
           return Err(ParseError::ExpectedToken("Expected a number".to_string()));
        };
        Ok(Expr {
            kind: ExprKind::Literal(LiteralKind::Num(value)),
            span,
        })
    }

    /// Build a sum,
    /// (assumes a '+' or '-' token has just been parse)
    fn parse_sum(cursor: &mut Cursor, lhs: Expr, _can_assign: bool) -> Result<Expr, ParseError> {
        // rhs => right hand side
        // lhs => left hand side
        let Token { kind, span } = cursor.take_previous()?;
        let rhs = Self::parse_precedence(cursor, Precedence::Factor)?;
        Ok(Expr {
            kind: ExprKind::Binary(Box::new(lhs), kind, Box::new(rhs)),
            span,
        })
    }

    /// Build a product,
    /// (assumes a '*' or '/' token has just been parse)
    fn parse_product(
        cursor: &mut Cursor,
        lhs: Expr,
        _can_assign: bool,
    ) -> Result<Expr, ParseError> {
        let Token { kind, span } = cursor.take_previous()?;
        let rhs = Self::parse_precedence(cursor, Precedence::Unary)?;
        Ok(Expr {
            kind: ExprKind::Binary(Box::new(lhs), kind, Box::new(rhs)),
            span,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::ExprParser;
    use crate::ast::{Expr, ExprKind, LiteralKind};
    use crate::cursor::{Cursor, ParseError};
    use lexer::{Lexer, StrPeeker, TokenKind, Tokenize};

    #[test]
    fn parse_number() {
        let src = "1";
        let lexer: Lexer<StrPeeker<'_, 64>> = Lexer::from_str(src);
        let mut cursor = Cursor::new(Box::new(lexer));
        let _ = cursor.advance();
        let mut parser = ExprParser::new(&mut cursor);

        let expr = parser.parse().unwrap();
        assert_eq!(expr.kind, ExprKind::Literal(LiteralKind::Num(1.)));
    }

    #[test]
    fn parse_sum() {
        let src = "1 + 2";
        let lexer: Lexer<StrPeeker<'_, 64>> = Lexer::from_str(src);
        let mut cursor = Cursor::new(Box::new(lexer));
        let _ = cursor.advance();
        let mut parser = ExprParser::new(&mut cursor);

        let expr = parser.parse().unwrap();
        let ExprKind::Binary(lhs, token_kind, rhs) = expr.kind else { panic!("failed to parse sum") };
        assert_eq!(lhs.kind, ExprKind::Literal(LiteralKind::Num(1.)));
        assert_eq!(rhs.kind, ExprKind::Literal(LiteralKind::Num(2.)));
        assert_eq!(token_kind, TokenKind::Plus);
    }

    #[test]
    fn parse_product() {
        let src = "1 * 2";
        let lexer: Lexer<StrPeeker<'_, 64>> = Lexer::from_str(src);
        let mut cursor = Cursor::new(Box::new(lexer));
        let _ = cursor.advance();
        let mut parser = ExprParser::new(&mut cursor);

        let expr = parser.parse().unwrap();
        let ExprKind::Binary(lhs, token_kind, rhs) = expr.kind else { panic!("failed to parse product") };
        assert_eq!(lhs.kind, ExprKind::Literal(LiteralKind::Num(1.)));
        assert_eq!(rhs.kind, ExprKind::Literal(LiteralKind::Num(2.)));
        assert_eq!(token_kind, TokenKind::Star);
    }

    #[test]
    fn parse_sum_of_product() {
        let src = "1 * 2 + 3";
        let lexer: Lexer<StrPeeker<'_, 64>> = Lexer::from_str(src);
        let mut cursor = Cursor::new(Box::new(lexer));
        let _ = cursor.advance();
        let mut parser = ExprParser::new(&mut cursor);

        let expr = parser.parse().unwrap();
        let ExprKind::Binary(lhs, token_kind, rhs) = expr.kind else { panic!("failed to parse sum") };
        assert_eq!(token_kind, TokenKind::Plus);
        assert_eq!(rhs.kind, ExprKind::Literal(LiteralKind::Num(3.)));
        assert_eq!(token_kind, TokenKind::Plus);

        let ExprKind::Binary(lhs, token_kind, rhs) = lhs.kind else { panic!("failed to parse product") };
        assert_eq!(lhs.kind, ExprKind::Literal(LiteralKind::Num(1.)));
        assert_eq!(rhs.kind, ExprKind::Literal(LiteralKind::Num(2.)));
        assert_eq!(token_kind, TokenKind::Star);
    }
}
