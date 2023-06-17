use super::ast::{
    BinaryExprKind, Expr as GenericExpr, ExprKind as GenericExprKind, LiteralKind, LogicalExprKind,
    UnaryExprKind,
};
use super::cursor::{Cursor, ParseError};
use lexer::{Token, TokenKind};

// in this file, use simply use
// strings to represent symbols.
pub type Expr = GenericExpr<String>;
pub type ExprKind = GenericExprKind<String>;

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
        cursor.advance()?;
        let prefix_fn = Self::get_prefix_handler(&cursor.previous()?.kind)
            .ok_or(ParseError::ExpectedExpression("Expected an expression"))?;
        let can_assign = precedence <= Precedence::Assignement;

        let mut expr = prefix_fn(cursor, can_assign)?;

        // Combine parsed expression with the next right expression,
        // if the next token is defines an infix expression.
        while let Some(next_expr_precedence) = Self::get_token_precedence(&cursor.current()?.kind) {
            if next_expr_precedence < precedence {
                // NOTE: not clear whether we should use < or <=
                break;
            }
            cursor.advance()?;
            // call the rule associated with handling
            // expressing **CONTAINING** this token
            let next_expr_parser_fn = Self::get_infix_handler(&cursor.previous()?.kind).ok_or(
                ParseError::ExpectedExpression("Expected expression after infix operator."),
            )?;
            expr = next_expr_parser_fn(cursor, expr, can_assign)?;
        }
        if can_assign && cursor.matches(TokenKind::Equal)? {
            return Err(ParseError::ExpectedToken("Invalid assignement target."));
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
            &TokenKind::Identifier(_) => Some(Self::parse_variable),
            &TokenKind::Str(_) => Some(Self::parse_string),
            &TokenKind::Number(_) => Some(Self::parse_number),
            &TokenKind::False => Some(Self::parse_false),
            &TokenKind::Nil => Some(Self::parse_nil),
            &TokenKind::Super => Some(Self::parse_super),
            &TokenKind::This => Some(Self::parse_this),
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
            &TokenKind::EqualEqual
            | &TokenKind::BangEqual
            | &TokenKind::GreaterEqual
            | &TokenKind::Greater
            | &TokenKind::LessEqual
            | &TokenKind::Less => Some(Precedence::Comparison),
            &TokenKind::Or => Some(Precedence::Or),
            &TokenKind::And => Some(Precedence::And),
            &TokenKind::LeftParen | &TokenKind::Dot => Some(Precedence::Call),
            &TokenKind::Equal => Some(Precedence::Assignement),
            _ => None,
        }
    }

    /// Return, depending of the variant of `kind`
    /// which parsing routine we should use to parse the
    /// expression if we found the token while parsing a bigger expression
    fn get_infix_handler(kind: &TokenKind) -> Option<InfixParserFn> {
        match kind {
            &TokenKind::Equal => Some(Self::parse_assign),
            &TokenKind::Dot => Some(Self::parse_get_or_set),
            &TokenKind::LeftParen => Some(Self::parse_call),
            &TokenKind::Minus => Some(Self::parse_substraction),
            &TokenKind::Plus => Some(Self::parse_sum),
            &TokenKind::Slash => Some(Self::parse_division),
            &TokenKind::Star => Some(Self::parse_product),
            &TokenKind::BangEqual => Some(Self::parse_not_equal),
            &TokenKind::EqualEqual => Some(Self::parse_equal),
            &TokenKind::Greater => Some(Self::parse_greater),
            &TokenKind::GreaterEqual => Some(Self::parse_greater_equal),
            &TokenKind::Less => Some(Self::parse_less),
            &TokenKind::LessEqual => Some(Self::parse_less_equal),
            &TokenKind::And => Some(Self::parse_and),
            &TokenKind::Or => Some(Self::parse_or),
            _ => None,
        }
    }

    fn parse_grouping(cursor: &mut Cursor, _can_assign: bool) -> Result<Expr, ParseError> {
        let Token { kind: TokenKind::LeftParen, span } = cursor.take_previous()? else {
           return Err(ParseError::ExpectedToken("Expected '('."));
        };
        let inner_expression = Self::parse_precedence(cursor, Precedence::Assignement)?;
        cursor.advance()?;
        let Token { kind: TokenKind::RightParen, .. } = cursor.take_previous()? else {
           return Err(ParseError::ExpectedToken("Expected ')'."));
        };

        Ok(Expr {
            kind: ExprKind::Grouping(Box::new(inner_expression)),
            span,
        })
    }

    /// Build an Unary expression representing a "Minus" expression.
    fn parse_minus(cursor: &mut Cursor, _can_assign: bool) -> Result<Expr, ParseError> {
        let Token { kind: TokenKind::Minus, span } = cursor.take_previous()? else {
           return Err(ParseError::ExpectedToken("Expected '-'."));
        };
        let child_expression = Self::parse_precedence(cursor, Precedence::Unary)?;

        Ok(Expr {
            kind: ExprKind::Unary(UnaryExprKind::Minus, Box::new(child_expression)),
            span,
        })
    }

    /// Build an Unary expression representing a "Not" expression.
    fn parse_not(cursor: &mut Cursor, _can_assign: bool) -> Result<Expr, ParseError> {
        let Token { kind: TokenKind::Bang, span } = cursor.take_previous()? else {
           return Err(ParseError::ExpectedToken("Expected '!'."));
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
           return Err(ParseError::ExpectedToken("Expected a number"));
        };
        Ok(Expr {
            kind: ExprKind::Literal(LiteralKind::Num(value)),
            span,
        })
    }

    /// Build a Literal expression from a String Token.
    fn parse_string(cursor: &mut Cursor, _can_assign: bool) -> Result<Expr, ParseError> {
        let Token { kind: TokenKind::Str(value), span } = cursor.take_previous()? else {
           return Err(ParseError::ExpectedToken("Expected a string"));
        };
        Ok(Expr {
            kind: ExprKind::Literal(LiteralKind::Str(value)),
            span,
        })
    }

    /// Build a Literal expression from a Nil Token.
    fn parse_nil(cursor: &mut Cursor, _can_assign: bool) -> Result<Expr, ParseError> {
        let Token { kind: TokenKind::Nil, span } = cursor.take_previous()? else {
           return Err(ParseError::ExpectedToken("Expected a 'nil'"));
        };
        Ok(Expr {
            kind: ExprKind::Literal(LiteralKind::Nil),
            span,
        })
    }

    /// Build a Literal expression from a `true` Token.
    fn parse_true(cursor: &mut Cursor, _can_assign: bool) -> Result<Expr, ParseError> {
        let Token { kind: TokenKind::True, span } = cursor.take_previous()? else {
           return Err(ParseError::ExpectedToken("Expected a 'true'"));
        };
        Ok(Expr {
            kind: ExprKind::Literal(LiteralKind::Bool(true)),
            span,
        })
    }

    /// Build a Literal expression from a `false` Token.
    fn parse_false(cursor: &mut Cursor, _can_assign: bool) -> Result<Expr, ParseError> {
        let Token { kind: TokenKind::False, span } = cursor.take_previous()? else {
           return Err(ParseError::ExpectedToken("Expected a 'false'"));
        };
        Ok(Expr {
            kind: ExprKind::Literal(LiteralKind::Bool(false)),
            span,
        })
    }

    /// Build a `Variable` expression from an `Identifier` Token.
    fn parse_variable(cursor: &mut Cursor, _can_assign: bool) -> Result<Expr, ParseError> {
        let Token { kind: TokenKind::Identifier(name), span } = cursor.take_previous()? else {
           return Err(ParseError::ExpectedToken("Expected an identifier"));
        };
        Ok(Expr {
            kind: ExprKind::Variable(name),
            span,
        })
    }

    /// Build a `Super` expression from an [`Super`, `Dot`, `Identifier`] Token sequences.
    fn parse_super(cursor: &mut Cursor, _can_assign: bool) -> Result<Expr, ParseError> {
        let Token { kind: TokenKind::Super, .. } = cursor.take_previous()? else {
           return Err(ParseError::ExpectedToken("Expected 'super'"));
        };
        cursor.advance()?;
        let Token { kind: TokenKind::Dot, .. } = cursor.take_previous()? else {
           return Err(ParseError::ExpectedToken("Expected '.'"));
        };
        cursor.advance()?;
        let Token { kind: TokenKind::Identifier(method_name), span } = cursor.take_previous()? else {
           return Err(ParseError::ExpectedToken("Expected a method identifier"));
        };
        Ok(Expr {
            kind: ExprKind::Super(method_name),
            span,
        })
    }

    /// Build a `This` expression from a `This` token.
    fn parse_this(cursor: &mut Cursor, _can_assign: bool) -> Result<Expr, ParseError> {
        let Token { kind: TokenKind::This, span: this_span } = cursor.take_previous()? else {
           return Err(ParseError::ExpectedToken("Expected 'this'"));
        };
        Ok(Expr {
            kind: ExprKind::This,
            span: this_span,
        })
    }

    /// Build a 'call' expression,
    /// (assumes an '(' token has just been parsed)
    /// parse all sub expression (the call arguments)
    fn parse_call(
        cursor: &mut Cursor,
        callee: Expr,
        _can_assign: bool,
    ) -> Result<Expr, ParseError> {
        let Token { span, .. } = cursor.take_previous()?;
        let mut arguments: Vec<Expr> = Vec::new();
        while !matches!(cursor.current()?.kind, TokenKind::RightParen) {
            arguments.push(Self::parse_precedence(cursor, Precedence::Assignement)?);
            if matches!(cursor.current()?.kind, TokenKind::Comma) {
                cursor.advance()?;
            }
        }
        // consume right parenthesis
        cursor.advance()?;
        Ok(Expr {
            kind: ExprKind::Call(Box::new(callee), arguments),
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

    /// Build an 'and' logical expression,
    /// (assumes an 'and' token has just been parsed)
    fn parse_and(cursor: &mut Cursor, lhs: Expr, _can_assign: bool) -> Result<Expr, ParseError> {
        let Token { span, .. } = cursor.take_previous()?;
        let rhs = Self::parse_precedence(cursor, Precedence::Equality)?;
        Ok(Expr {
            kind: ExprKind::Logical(Box::new(lhs), LogicalExprKind::And, Box::new(rhs)),
            span,
        })
    }

    /// Build an 'or' logical expression,
    /// (assumes a 'or' token has just been parsed)
    fn parse_or(cursor: &mut Cursor, lhs: Expr, _can_assign: bool) -> Result<Expr, ParseError> {
        let Token { span, .. } = cursor.take_previous()?;
        let rhs = Self::parse_precedence(cursor, Precedence::And)?;
        Ok(Expr {
            kind: ExprKind::Logical(Box::new(lhs), LogicalExprKind::Or, Box::new(rhs)),
            span,
        })
    }

    /// Build a 'not equal' comparison expression,
    /// (assumes a '!=' token has just been parsed)
    fn parse_not_equal(
        cursor: &mut Cursor,
        lhs: Expr,
        _can_assign: bool,
    ) -> Result<Expr, ParseError> {
        Self::parse_binary_expression(cursor, lhs, Precedence::Term, BinaryExprKind::NotEqual)
    }

    /// Build an 'equal' comparison expression,
    /// (assumes a '== token has just been parsed)
    fn parse_equal(cursor: &mut Cursor, lhs: Expr, _can_assign: bool) -> Result<Expr, ParseError> {
        Self::parse_binary_expression(cursor, lhs, Precedence::Term, BinaryExprKind::Equal)
    }

    /// Build an 'less or equal' comparison expression,
    /// (assumes a '<= token has just been parsed)
    fn parse_less_equal(
        cursor: &mut Cursor,
        lhs: Expr,
        _can_assign: bool,
    ) -> Result<Expr, ParseError> {
        Self::parse_binary_expression(cursor, lhs, Precedence::Term, BinaryExprKind::LessEqual)
    }

    /// Build an 'less' comparison expression,
    /// (assumes a '<' token has just been parsed)
    fn parse_less(cursor: &mut Cursor, lhs: Expr, _can_assign: bool) -> Result<Expr, ParseError> {
        Self::parse_binary_expression(cursor, lhs, Precedence::Term, BinaryExprKind::Less)
    }

    /// Build an 'greater or equal' comparison expression,
    /// (assumes a '>=' token has just been parsed)
    fn parse_greater_equal(
        cursor: &mut Cursor,
        lhs: Expr,
        _can_assign: bool,
    ) -> Result<Expr, ParseError> {
        Self::parse_binary_expression(cursor, lhs, Precedence::Term, BinaryExprKind::GreaterEqual)
    }

    /// Build an 'greater' expression,
    /// (assumes a '> token has just been parsed)
    fn parse_greater(
        cursor: &mut Cursor,
        lhs: Expr,
        _can_assign: bool,
    ) -> Result<Expr, ParseError> {
        Self::parse_binary_expression(cursor, lhs, Precedence::Term, BinaryExprKind::Greater)
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

    /// Build a Get or a Set Expr,
    /// (assumes the '.' token has just been parsed)
    fn parse_get_or_set(
        cursor: &mut Cursor,
        lhs: Expr,
        can_assign: bool,
    ) -> Result<Expr, ParseError> {
        // store the span of the `dot` token
        let Token { span: dot_span, .. } = cursor.take_previous()?;

        // get identifier (eg: attribute name)
        cursor.advance()?;
        let Token { kind: TokenKind::Identifier(id), .. } = cursor.take_previous()? else {
             return Err(ParseError::ExpectedToken("Expected an identifier after '.'"));
        };

        if can_assign && cursor.matches(TokenKind::Equal)? {
            let Token {
                span: equal_span, ..
            } = cursor.take_previous()?;
            let rvalue = Self::parse_precedence(cursor, Precedence::Assignement)?;
            return Ok(Expr {
                kind: ExprKind::Set(Box::new(lhs), id, Box::new(rvalue)),
                span: equal_span,
            });
        }
        Ok(Expr {
            kind: ExprKind::Get(Box::new(lhs), id),
            span: dot_span,
        })
    }

    // parse an assignment,
    // eg: `IDENTIFER = EXPRESSION`
    // (assumes that '=' token has just been parsed).
    fn parse_assign(
        cursor: &mut Cursor,
        lvalue: Expr,
        can_assign: bool,
    ) -> Result<Expr, ParseError> {
        if !can_assign {
            return Err(ParseError::ExpectedToken("unexpected assignment"));
        }
        // store the span of the `=` token
        let Token { span, .. } = cursor.take_previous()?;
        let Expr { kind: ExprKind::Variable(id), .. } = lvalue else {
            return Err(ParseError::ExpectedToken("Can only assign to variables"));
        };

        let rvalue = Self::parse_precedence(cursor, Precedence::Or)?;
        Ok(Expr {
            kind: ExprKind::Assign(id, Box::new(rvalue)),
            span,
        })
    }
}

#[cfg(test)]
mod parsing {
    use super::{Expr, ExprKind, ExprParser};
    use crate::ast::{BinaryExprKind, LiteralKind, LogicalExprKind, UnaryExprKind};
    use crate::cursor::{Cursor, ParseError};
    use lexer::{Lexer, StrPeeker};

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
        let expr = parse_expression(src).unwrap();
        assert_eq!(expr.kind, ExprKind::Literal(LiteralKind::Num(1.)));
    }

    #[test]
    fn parse_string() {
        let src = r#" "str" "#;
        let expr = parse_expression(src).unwrap();
        assert_eq!(
            expr.kind,
            ExprKind::Literal(LiteralKind::Str("str".to_string()))
        );
    }

    #[test]
    fn parse_nil() {
        let src = "nil";
        let expr = parse_expression(src).unwrap();
        assert_eq!(expr.kind, ExprKind::Literal(LiteralKind::Nil));
    }

    #[test]
    fn parse_true() {
        let src = "true";
        let expr = parse_expression(src).unwrap();
        assert_eq!(expr.kind, ExprKind::Literal(LiteralKind::Bool(true)));
    }

    #[test]
    fn parse_false() {
        let src = "false";
        let expr = parse_expression(src).unwrap();
        assert_eq!(expr.kind, ExprKind::Literal(LiteralKind::Bool(false)));
    }

    #[test]
    fn parse_variable() {
        let src = "a";
        let expr = parse_expression(src).unwrap();
        assert_eq!(expr.kind, ExprKind::Variable("a".to_string()));
    }

    #[test]
    fn parse_super() {
        let src = "super.method_name";
        let expr = parse_expression(src).unwrap();
        assert_eq!(expr.kind, ExprKind::Super("method_name".to_string()));
    }

    #[test]
    fn parse_this() {
        let src = "this";
        let expr = parse_expression(src).unwrap();
        assert_eq!(expr.kind, ExprKind::This);
    }

    #[test]
    fn parse_group() {
        let src = "(1)";
        let expr = parse_expression(src).unwrap();
        let ExprKind::Grouping(inner_expr) = expr.kind else { panic!("failed to parse group") };
        assert_eq!(inner_expr.kind, ExprKind::Literal(LiteralKind::Num(1.)));
    }

    #[test]
    fn parse_unary_minus() {
        let src = "-1";
        let expr = parse_expression(src).unwrap();
        let ExprKind::Unary(UnaryExprKind::Minus, inner_expr) = expr.kind else {
            panic!("failed to parse minus") 
        };
        assert_eq!(inner_expr.kind, ExprKind::Literal(LiteralKind::Num(1.)));
    }

    #[test]
    fn parse_unary_not() {
        let src = "!true";
        let expr = parse_expression(src).unwrap();
        let ExprKind::Unary(UnaryExprKind::Not, inner_expr) = expr.kind else {
            panic!("failed to parse 'not'") 
        };
        assert_eq!(inner_expr.kind, ExprKind::Literal(LiteralKind::Bool(true)));
    }

    #[test]
    fn parse_sum() {
        let src = "1 + 2";
        let expr = parse_expression(src).unwrap();

        let ExprKind::Binary(lhs, token_kind, rhs) = expr.kind else { panic!("failed to parse sum") };
        assert_eq!(lhs.kind, ExprKind::Literal(LiteralKind::Num(1.)));
        assert_eq!(rhs.kind, ExprKind::Literal(LiteralKind::Num(2.)));
        assert_eq!(token_kind, BinaryExprKind::Add);
    }

    #[test]
    fn parse_substraction() {
        let src = "1 - 2";
        let expr = parse_expression(src).unwrap();

        let ExprKind::Binary(lhs, token_kind, rhs) = expr.kind else { panic!("failed to parse sum") };
        assert_eq!(lhs.kind, ExprKind::Literal(LiteralKind::Num(1.)));
        assert_eq!(rhs.kind, ExprKind::Literal(LiteralKind::Num(2.)));
        assert_eq!(token_kind, BinaryExprKind::Sub);
    }

    #[test]
    fn parse_product() {
        let src = "1 * 2";
        let expr = parse_expression(src).unwrap();

        let ExprKind::Binary(lhs, token_kind, rhs) = expr.kind else { panic!("failed to parse product") };
        assert_eq!(lhs.kind, ExprKind::Literal(LiteralKind::Num(1.)));
        assert_eq!(rhs.kind, ExprKind::Literal(LiteralKind::Num(2.)));
        assert_eq!(token_kind, BinaryExprKind::Mul);
    }

    #[test]
    fn parse_division() {
        let src = "1 / 2";
        let expr = parse_expression(src).unwrap();

        let ExprKind::Binary(lhs, token_kind, rhs) = expr.kind else { panic!("failed to parse division") };
        assert_eq!(lhs.kind, ExprKind::Literal(LiteralKind::Num(1.)));
        assert_eq!(rhs.kind, ExprKind::Literal(LiteralKind::Num(2.)));
        assert_eq!(token_kind, BinaryExprKind::Div);
    }

    #[test]
    fn parse_not_equal() {
        let src = "1 != 2";
        let expr = parse_expression(src).unwrap();

        let ExprKind::Binary(lhs, token_kind, rhs) = expr.kind else { panic!("failed to parse equality") };
        assert_eq!(lhs.kind, ExprKind::Literal(LiteralKind::Num(1.)));
        assert_eq!(rhs.kind, ExprKind::Literal(LiteralKind::Num(2.)));
        assert_eq!(token_kind, BinaryExprKind::NotEqual);
    }

    #[test]
    fn parse_equal() {
        let src = "1 == 2";
        let expr = parse_expression(src).unwrap();

        let ExprKind::Binary(lhs, token_kind, rhs) = expr.kind else { panic!("failed to parse equality") };
        assert_eq!(lhs.kind, ExprKind::Literal(LiteralKind::Num(1.)));
        assert_eq!(rhs.kind, ExprKind::Literal(LiteralKind::Num(2.)));
        assert_eq!(token_kind, BinaryExprKind::Equal);
    }

    #[test]
    fn parse_less() {
        let src = "1 < 2";
        let expr = parse_expression(src).unwrap();

        let ExprKind::Binary(lhs, token_kind, rhs) = expr.kind else { panic!("failed to parse equality") };
        assert_eq!(lhs.kind, ExprKind::Literal(LiteralKind::Num(1.)));
        assert_eq!(rhs.kind, ExprKind::Literal(LiteralKind::Num(2.)));
        assert_eq!(token_kind, BinaryExprKind::Less);
    }

    #[test]
    fn parse_less_equal() {
        let src = "1 <= 2";
        let expr = parse_expression(src).unwrap();

        let ExprKind::Binary(lhs, token_kind, rhs) = expr.kind else { panic!("failed to parse equality") };
        assert_eq!(lhs.kind, ExprKind::Literal(LiteralKind::Num(1.)));
        assert_eq!(rhs.kind, ExprKind::Literal(LiteralKind::Num(2.)));
        assert_eq!(token_kind, BinaryExprKind::LessEqual);
    }

    #[test]
    fn parse_greater() {
        let src = "1 > 2";
        let expr = parse_expression(src).unwrap();

        let ExprKind::Binary(lhs, token_kind, rhs) = expr.kind else { panic!("failed to parse equality") };
        assert_eq!(lhs.kind, ExprKind::Literal(LiteralKind::Num(1.)));
        assert_eq!(rhs.kind, ExprKind::Literal(LiteralKind::Num(2.)));
        assert_eq!(token_kind, BinaryExprKind::Greater);
    }

    #[test]
    fn parse_greater_equal() {
        let src = "1 >= 2";
        let expr = parse_expression(src).unwrap();

        let ExprKind::Binary(lhs, token_kind, rhs) = expr.kind else { panic!("failed to parse equality") };
        assert_eq!(lhs.kind, ExprKind::Literal(LiteralKind::Num(1.)));
        assert_eq!(rhs.kind, ExprKind::Literal(LiteralKind::Num(2.)));
        assert_eq!(token_kind, BinaryExprKind::GreaterEqual);
    }

    #[test]
    fn parse_and() {
        let src = "true and true";
        let expr = parse_expression(src).unwrap();

        let ExprKind::Logical(lhs, token_kind, rhs) = expr.kind else { panic!("failed to parse logical expression") };
        assert_eq!(lhs.kind, ExprKind::Literal(LiteralKind::Bool(true)));
        assert_eq!(rhs.kind, ExprKind::Literal(LiteralKind::Bool(true)));
        assert_eq!(token_kind, LogicalExprKind::And);
    }

    #[test]
    fn parse_or() {
        let src = "true or true";
        let expr = parse_expression(src).unwrap();

        let ExprKind::Logical(lhs, token_kind, rhs) = expr.kind else { panic!("failed to parse logical expression") };
        assert_eq!(lhs.kind, ExprKind::Literal(LiteralKind::Bool(true)));
        assert_eq!(rhs.kind, ExprKind::Literal(LiteralKind::Bool(true)));
        assert_eq!(token_kind, LogicalExprKind::Or);
    }

    #[test]
    fn parse_call_no_arg() {
        // test with no args
        let src = "fn()";
        let expr = parse_expression(src).unwrap();

        let ExprKind::Call(callee, arguments) = expr.kind else { panic!("failed to parse call expression") };
        assert_eq!(callee.kind, ExprKind::Variable("fn".to_string()));
        assert_eq!(arguments, vec![]);
    }

    #[test]
    fn parse_call_one_arg() {
        // test with one arg
        let src = "fn(1)";
        let expr = parse_expression(src).unwrap();

        let ExprKind::Call(callee, arguments) = expr.kind else { panic!("failed to parse call expression with one arg") };
        assert_eq!(callee.kind, ExprKind::Variable("fn".to_string()));
        assert_eq!(arguments.len(), 1);
    }

    #[test]
    fn parse_call_several_args() {
        // test with two arg
        let src = "fn(1, 2)";
        let expr = parse_expression(src).unwrap();

        let ExprKind::Call(callee, arguments) = expr.kind else { panic!("failed to parse call expression with several args") };
        assert_eq!(callee.kind, ExprKind::Variable("fn".to_string()));
        assert_eq!(arguments.len(), 2);
    }

    #[test]
    fn parse_sum_of_product() {
        let src = "1 * 2 + 3";
        let expr = parse_expression(src).unwrap();

        let ExprKind::Binary(lhs, token_kind, rhs) = expr.kind else { panic!("failed to parse sum") };
        assert_eq!(token_kind, BinaryExprKind::Add);
        assert_eq!(rhs.kind, ExprKind::Literal(LiteralKind::Num(3.)));
        assert_eq!(token_kind, BinaryExprKind::Add);

        let ExprKind::Binary(lhs, token_kind, rhs) = lhs.kind else { panic!("failed to parse product") };
        assert_eq!(lhs.kind, ExprKind::Literal(LiteralKind::Num(1.)));
        assert_eq!(rhs.kind, ExprKind::Literal(LiteralKind::Num(2.)));
        assert_eq!(token_kind, BinaryExprKind::Mul);
    }

    #[test]
    fn parse_getter() {
        let src = "a.b";
        let expr = parse_expression(src).unwrap();

        let ExprKind::Get(instance, attribute) = expr.kind else { panic!("failed to parse Get expression") };
        assert_eq!(instance.kind, ExprKind::Variable("a".to_string()));
        assert_eq!(attribute, "b".to_string());
    }

    #[test]
    fn parse_setter() {
        let src = "a.b = 1";
        let expr = parse_expression(src).unwrap();

        let ExprKind::Set(instance, attribute, rvalue) = expr.kind else { panic!("failed to parse Set expression") };
        assert_eq!(instance.kind, ExprKind::Variable("a".to_string()));
        assert_eq!(attribute, "b".to_string());
        assert_eq!(rvalue.kind, ExprKind::Literal(LiteralKind::Num(1.)));
    }

    #[test]
    fn parse_assign() {
        let src = "a = 1";
        let expr = parse_expression(src).unwrap();

        let ExprKind::Assign(lvalue, rvalue) = expr.kind else { panic!("failed to parse Assign expression") };
        assert_eq!(&lvalue, "a");
        assert_eq!(rvalue.kind, ExprKind::Literal(LiteralKind::Num(1.)));
    }
}
