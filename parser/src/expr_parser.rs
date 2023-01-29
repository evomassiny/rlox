use super::ast::{BinaryExprKind, Expr, ExprKind, LiteralKind, UnaryExprKind};
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
pub struct ExprParser<'cursor, 'input>(&'cursor mut Cursor<'input>);

/// a function that parses an expression from a cursor, given a prefix TokenKind
type PrefixParserFn = fn(&mut Cursor, bool) -> Result<Expr, ParseError>;

/// a function that parses an expression from a cursor and the previously parsed
/// expression, given an infix TokenKind
type InfixParserFn = fn(&mut Cursor, Expr, bool) -> Result<Expr, ParseError>;

impl<'cursor, 'input> ExprParser<'cursor, 'input>
where
    'input: 'cursor,
{
    pub fn new(cursor: &'cursor mut Cursor<'input>) -> Self {
        Self(cursor)
    }

    pub fn parse(&mut self) -> Result<Expr, ParseError> {
        Self::parse_precedence(self.0, Precedence::Assignement)
    }

    /// Parse all encountered expressions until we reach
    /// a token associated with a binding power lower than `precedence`
    fn parse_precedence(cursor: &mut Cursor, precedence: Precedence) -> Result<Expr, ParseError> {
        let _ = cursor.advance()?;
        let prefix_fn = Self::get_prefix_handler(&cursor.previous()?.kind)
            .ok_or(ParseError::ExpectedExpression)?;
        let can_assign = precedence <= Precedence::Assignement;

        let mut expr = prefix_fn(cursor, can_assign)?;

        // Combine parsed expression with the next right expression,
        // if the next token is defines an infix expression.
        while let Some(next_expr_precedence) = Self::get_token_precedence(&cursor.current()?.kind) {
            if next_expr_precedence <= precedence {
                break;
            }
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
            &TokenKind::LeftParen => Some(Self::parse_grouping),
            &TokenKind::Minus => Some(Self::parse_minus),
            &TokenKind::Bang => Some(Self::parse_not),
            &TokenKind::Identifier(_) => todo!(),
            &TokenKind::Str(_) => Some(Self::parse_string),
            &TokenKind::Number(_) => Some(Self::parse_number),
            &TokenKind::False => Some(Self::parse_false),
            &TokenKind::Nil => Some(Self::parse_nil),
            &TokenKind::Super => todo!(),
            &TokenKind::This => todo!(),
            &TokenKind::True => Some(Self::parse_true),
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
            &TokenKind::Minus => Some(Self::parse_substraction),
            &TokenKind::Plus => Some(Self::parse_sum),
            &TokenKind::Slash => Some(Self::parse_division),
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
        let Token { kind: TokenKind::LeftParen, span } = cursor.take_previous()? else {
           return Err(ParseError::ExpectedToken("Expected '('.".to_string()));
        };
        let inner_expression = Self::parse_precedence(cursor, Precedence::Assignement)?;
        let _ = cursor.advance()?;
        let Token { kind: TokenKind::RightParen, .. } = cursor.take_previous()? else {
           return Err(ParseError::ExpectedToken("Expected ')'.".to_string()));
        };

        Ok(Expr {
            kind: ExprKind::Grouping(Box::new(inner_expression)),
            span,
        })
    }

    /// Build an Unary expression representing a "Minus" expression.
    fn parse_minus(cursor: &mut Cursor, can_assign: bool) -> Result<Expr, ParseError> {
        let Token { kind: TokenKind::Minus, span } = cursor.take_previous()? else {
           return Err(ParseError::ExpectedToken("Expected '-'.".to_string()));
        };
        let child_expression = Self::parse_precedence(cursor, Precedence::Unary)?;

        Ok(Expr {
            kind: ExprKind::Unary(UnaryExprKind::Minus, Box::new(child_expression)),
            span,
        })
    }

    /// Build an Unary expression representing a "Not" expression.
    fn parse_not(cursor: &mut Cursor, can_assign: bool) -> Result<Expr, ParseError> {
        let Token { kind: TokenKind::Bang, span } = cursor.take_previous()? else {
           return Err(ParseError::ExpectedToken("Expected '!'.".to_string()));
        };
        let child_expression = Self::parse_precedence(cursor, Precedence::Unary)?;

        Ok(Expr {
            kind: ExprKind::Unary(UnaryExprKind::Not, Box::new(child_expression)),
            span,
        })
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

    /// Build a Literal expression from a String Token.
    fn parse_string(cursor: &mut Cursor, _can_assign: bool) -> Result<Expr, ParseError> {
        let Token { kind: TokenKind::Str(value), span } = cursor.take_previous()? else {
           return Err(ParseError::ExpectedToken("Expected a string".to_string()));
        };
        Ok(Expr {
            kind: ExprKind::Literal(LiteralKind::Str(value)),
            span,
        })
    }

    /// Build a Literal expression from a Nil Token.
    fn parse_nil(cursor: &mut Cursor, _can_assign: bool) -> Result<Expr, ParseError> {
        let Token { kind: TokenKind::Nil, span } = cursor.take_previous()? else {
           return Err(ParseError::ExpectedToken("Expected a 'nil'".to_string()));
        };
        Ok(Expr {
            kind: ExprKind::Literal(LiteralKind::Nil),
            span,
        })
    }

    /// Build a Literal expression from a `true` Token.
    fn parse_true(cursor: &mut Cursor, _can_assign: bool) -> Result<Expr, ParseError> {
        let Token { kind: TokenKind::True, span } = cursor.take_previous()? else {
           return Err(ParseError::ExpectedToken("Expected a 'true'".to_string()));
        };
        Ok(Expr {
            kind: ExprKind::Literal(LiteralKind::Bool(true)),
            span,
        })
    }

    /// Build a Literal expression from a `false` Token.
    fn parse_false(cursor: &mut Cursor, _can_assign: bool) -> Result<Expr, ParseError> {
        let Token { kind: TokenKind::False, span } = cursor.take_previous()? else {
           return Err(ParseError::ExpectedToken("Expected a 'false'".to_string()));
        };
        Ok(Expr {
            kind: ExprKind::Literal(LiteralKind::Bool(false)),
            span,
        })
    }

    fn parse_binary_expression(
        cursor: &mut Cursor,
        lhs: Expr,
        rhs_precendence: Precedence,
        binary_kind: BinaryExprKind,
    ) -> Result<Expr, ParseError> {
        let Token { span, .. } = cursor.take_previous()?;
        let rhs = Self::parse_precedence(cursor, rhs_precendence)?;
        Ok(Expr {
            kind: ExprKind::Binary(Box::new(lhs), binary_kind, Box::new(rhs)),
            span,
        })
    }

    /// Build a sum,
    /// (assumes a '+' token has just been parsed)
    fn parse_sum(cursor: &mut Cursor, lhs: Expr, _can_assign: bool) -> Result<Expr, ParseError> {
        Self::parse_binary_expression(cursor, lhs, Precedence::Factor, BinaryExprKind::Add)
    }

    /// Build a substraction,
    /// (assumes a '-' token has just been parsed)
    fn parse_substraction(
        cursor: &mut Cursor,
        lhs: Expr,
        _can_assign: bool,
    ) -> Result<Expr, ParseError> {
        Self::parse_binary_expression(cursor, lhs, Precedence::Factor, BinaryExprKind::Sub)
    }

    /// Build a product,
    /// (assumes a '*' token has just been parsed)
    fn parse_product(
        cursor: &mut Cursor,
        lhs: Expr,
        _can_assign: bool,
    ) -> Result<Expr, ParseError> {
        Self::parse_binary_expression(cursor, lhs, Precedence::Unary, BinaryExprKind::Mul)
    }

    /// Build a division,
    /// (assumes a '/' token has just been parsed)
    fn parse_division(
        cursor: &mut Cursor,
        lhs: Expr,
        _can_assign: bool,
    ) -> Result<Expr, ParseError> {
        Self::parse_binary_expression(cursor, lhs, Precedence::Unary, BinaryExprKind::Div)
    }
}

#[cfg(test)]
mod tests {
    use super::ExprParser;
    use crate::ast::{BinaryExprKind, Expr, ExprKind, LiteralKind, UnaryExprKind};
    use crate::cursor::{Cursor, ParseError};
    use lexer::{Lexer, StrPeeker, TokenKind, Tokenize};

    fn parse_expression(src: &str) -> Result<Expr, ParseError> {
        let lexer: Lexer<StrPeeker<'_, 64>> = Lexer::from_str(src);
        let mut cursor = Cursor::new(Box::new(lexer));
        let _ = cursor.advance();
        let mut parser = ExprParser::new(&mut cursor);
        parser.parse()
    }

    #[test]
    fn parse_number() {
        let src = "1";
        let expr = parse_expression(&src).unwrap();
        assert_eq!(expr.kind, ExprKind::Literal(LiteralKind::Num(1.)));
    }

    #[test]
    fn parse_string() {
        let src = r#" "str" "#;
        let expr = parse_expression(&src).unwrap();
        assert_eq!(
            expr.kind,
            ExprKind::Literal(LiteralKind::Str("str".to_string()))
        );
    }

    #[test]
    fn parse_nil() {
        let src = "nil";
        let expr = parse_expression(&src).unwrap();
        assert_eq!(expr.kind, ExprKind::Literal(LiteralKind::Nil));
    }

    #[test]
    fn parse_true() {
        let src = "true";
        let expr = parse_expression(&src).unwrap();
        assert_eq!(expr.kind, ExprKind::Literal(LiteralKind::Bool(true)));
    }

    #[test]
    fn parse_false() {
        let src = "false";
        let expr = parse_expression(&src).unwrap();
        assert_eq!(expr.kind, ExprKind::Literal(LiteralKind::Bool(false)));
    }

    #[test]
    fn parse_group() {
        let src = "(1)";
        let expr = parse_expression(&src).unwrap();
        let ExprKind::Grouping(inner_expr) = expr.kind else { panic!("failed to parse group") };
        assert_eq!(inner_expr.kind, ExprKind::Literal(LiteralKind::Num(1.)));
    }

    #[test]
    fn parse_unary_minus() {
        let src = "-1";
        let expr = parse_expression(&src).unwrap();
        let ExprKind::Unary(UnaryExprKind::Minus, inner_expr) = expr.kind else {
            panic!("failed to parse minus") 
        };
        assert_eq!(inner_expr.kind, ExprKind::Literal(LiteralKind::Num(1.)));
    }

    #[test]
    fn parse_unary_not() {
        let src = "!true";
        let expr = parse_expression(&src).unwrap();
        let ExprKind::Unary(UnaryExprKind::Not, inner_expr) = expr.kind else {
            panic!("failed to parse 'not'") 
        };
        assert_eq!(inner_expr.kind, ExprKind::Literal(LiteralKind::Bool(true)));
    }

    #[test]
    fn parse_sum() {
        let src = "1 + 2";
        let expr = parse_expression(&src).unwrap();

        let ExprKind::Binary(lhs, token_kind, rhs) = expr.kind else { panic!("failed to parse sum") };
        assert_eq!(lhs.kind, ExprKind::Literal(LiteralKind::Num(1.)));
        assert_eq!(rhs.kind, ExprKind::Literal(LiteralKind::Num(2.)));
        assert_eq!(token_kind, BinaryExprKind::Add);
    }

    #[test]
    fn parse_substraction() {
        let src = "1 - 2";
        let expr = parse_expression(&src).unwrap();

        let ExprKind::Binary(lhs, token_kind, rhs) = expr.kind else { panic!("failed to parse sum") };
        assert_eq!(lhs.kind, ExprKind::Literal(LiteralKind::Num(1.)));
        assert_eq!(rhs.kind, ExprKind::Literal(LiteralKind::Num(2.)));
        assert_eq!(token_kind, BinaryExprKind::Sub);
    }

    #[test]
    fn parse_product() {
        let src = "1 * 2";
        let expr = parse_expression(&src).unwrap();

        let ExprKind::Binary(lhs, token_kind, rhs) = expr.kind else { panic!("failed to parse product") };
        assert_eq!(lhs.kind, ExprKind::Literal(LiteralKind::Num(1.)));
        assert_eq!(rhs.kind, ExprKind::Literal(LiteralKind::Num(2.)));
        assert_eq!(token_kind, BinaryExprKind::Mul);
    }

    #[test]
    fn parse_division() {
        let src = "1 / 2";
        let expr = parse_expression(&src).unwrap();

        let ExprKind::Binary(lhs, token_kind, rhs) = expr.kind else { panic!("failed to parse division") };
        assert_eq!(lhs.kind, ExprKind::Literal(LiteralKind::Num(1.)));
        assert_eq!(rhs.kind, ExprKind::Literal(LiteralKind::Num(2.)));
        assert_eq!(token_kind, BinaryExprKind::Div);
    }

    #[test]
    fn parse_sum_of_product() {
        let src = "1 * 2 + 3";
        let expr = parse_expression(&src).unwrap();

        let ExprKind::Binary(lhs, token_kind, rhs) = expr.kind else { panic!("failed to parse sum") };
        assert_eq!(token_kind, BinaryExprKind::Add);
        assert_eq!(rhs.kind, ExprKind::Literal(LiteralKind::Num(3.)));
        assert_eq!(token_kind, BinaryExprKind::Add);

        let ExprKind::Binary(lhs, token_kind, rhs) = lhs.kind else { panic!("failed to parse product") };
        assert_eq!(lhs.kind, ExprKind::Literal(LiteralKind::Num(1.)));
        assert_eq!(rhs.kind, ExprKind::Literal(LiteralKind::Num(2.)));
        assert_eq!(token_kind, BinaryExprKind::Mul);
    }
}
