use super::ast::{Expr, ExprKind, LiteralKind, Stmt, StmtKind};
use super::cursor::{Cursor, ParseError};
use super::expr_parser::ExprParser;
use lexer::{Span, Token, TokenKind, Tokenize};

/// Parse statements using an explicit recursive
/// descent parser, and expressions using a Pratt Parser.
pub struct StmtParser<'input> {
    cursor: Cursor<'input>,
}

impl<'input> StmtParser<'input> {
    pub fn new(lexer: Box<dyn Tokenize + 'input>) -> Self {
        let cursor = Cursor::new(lexer);
        Self { cursor }
    }

    /**
     * declaration -> varDeclaration
     *                | classDeclaration
     *                | funDeclaration
     *                | statement;
     */
    fn declaration<'parser>(&'parser mut self) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        let current: &TokenKind = &self.cursor.current()?.kind;
        match *current {
            TokenKind::Var => {
                // position cursor rigt after `var` token;
                self.cursor.advance()?;
                self.var_declaration()
            }
            /*
            TokenKind::Class => {
                self.cursor.advance()?;
                self.class_declaration()
            }
            TokenKind::Fun => {
                self.cursor.advance()?;
                self.fun_declaration()
            }
            */
            _ => self.statement(),
        }
    }

    /// parse a var declaration
    /// assume `var` has been parsed
    fn var_declaration<'parser>(&'parser mut self) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        // store span of `var` token
        let Token { span: var_span, .. } = self.cursor.take_previous()?;

        // parse next expression
        let mut expression_parser = ExprParser::new(&mut self.cursor);
        let expr = expression_parser.parse()?;
        let _ = self
            .cursor
            .consume(TokenKind::Semicolon, "Expected ';' after print statement.")?;

        match expr {
            // case with no initializer
            Expr {
                kind: ExprKind::Variable(id),
                span,
            } => {
                let nil_expr = Expr {
                    kind: ExprKind::Literal(LiteralKind::Nil),
                    span,
                };
                Ok(Stmt {
                    kind: StmtKind::Var(id, Box::new(nil_expr)),
                    span: var_span,
                })
            }
            // case with initializer
            Expr {
                kind: ExprKind::Assign(id, initializer),
                ..
            } => Ok(Stmt {
                kind: StmtKind::Var(id, initializer),
                span: var_span,
            }),
            _ => Err(ParseError::ExpectedExpression("Expected variable name.")),
        }
    }

    fn class_declaration<'parser>(&'parser mut self) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        todo!()
    }

    fn fun_declaration<'parser>(&'parser mut self) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        todo!()
    }

    /// parse block statement
    /// assumes `{` has been parsed.
    fn block_statement<'parser>(&'parser mut self) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        let Token { span, .. } = self.cursor.take_previous()?;

        // parse sub-statements until we encounter a '}'
        let mut statements: Vec<Stmt> = Vec::new();
        while !self.cursor.check(TokenKind::RightBrace)? {
            let stmt = self.declaration()?;
            statements.push(stmt);
        }
        self.cursor
            .consume(TokenKind::RightParen, "Expect '}' after block.")?;
        Ok(Stmt {
            kind: StmtKind::Block(statements),
            span,
        })
    }

    fn return_statement<'parser>(&'parser mut self) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        todo!()
    }
    fn while_statement<'parser>(&'parser mut self) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        todo!()
    }
    fn for_statement<'parser>(&'parser mut self) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        todo!()
    }
    /**
     * statement -> printStatement
     *              | blockStatement
     *              | ifStatement
     *              | returnStatement
     *              | whileStatement
     *              | forStatement
     *              | expressionStatement
     *              ;
     */
    fn statement<'parser>(&'parser mut self) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        let current: &TokenKind = &self.cursor.current()?.kind;
        match *current {
            TokenKind::Print => {
                // position cursor right after `print
                let _ = self.cursor.advance();
                self.print_statement()
            }
            TokenKind::LeftBrace => {
                // position cursor right after `{`
                let _ = self.cursor.advance();
                self.block_statement()
            }
            TokenKind::If => {
                // position cursor right after `if
                let _ = self.cursor.advance();
                self.if_statement()
            }
            /*
            TokenKind::Return => {
                self.cursor.advance()?;
                self.return_statement()
            }
            TokenKind::While => {
                self.cursor.advance()?;
                self.while_statement()
            }
            TokenKind::For => {
                self.cursor.advance()?;
                self.for_statement()
            }
            */
            _ => self.expression_statement(),
        }
    }

    /// parse a print statement,
    /// assume `print` has been parsed
    fn print_statement<'parser>(&'parser mut self) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        // store 'print' span
        let Token { span, .. } = self.cursor.take_previous()?;

        // parse following expression
        let mut expression_parser = ExprParser::new(&mut self.cursor);
        let expr = expression_parser.parse()?;

        let _ = self
            .cursor
            .consume(TokenKind::Semicolon, "Expected ';' after print statement.")?;

        dbg!(&expr);
        Ok(Stmt {
            kind: StmtKind::Print(Box::new(expr)),
            span,
        })
    }

    fn if_statement<'parser>(&'parser mut self) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        // store 'if' span
        let Token { span, .. } = self.cursor.take_previous()?;

        // consume '('
        let _ = self.cursor.consume(
            TokenKind::LeftParen,
            "Expected a parenthesised condition after an if statement.",
        )?;
        // parse condition
        let mut expression_parser = ExprParser::new(&mut self.cursor);
        let cond_expr = expression_parser.parse()?;
        // consume ')'
        let _ = self.cursor.consume(
            TokenKind::LeftParen,
            "Expected a closing after an 'if' condition.",
        )?;
        // parse then branch
        if !self.cursor.matches(TokenKind::LeftBrace)? {
            return Err(ParseError::ExpectedExpression(
                "Expect a 'then' branch after an if condition (eg: '{')",
            ));
        }
        let then_block = self.block_statement()?;

        // case with no `else` branch
        if !self.cursor.matches(TokenKind::Else)? {
            return Ok(Stmt {
                kind: StmtKind::If(Box::new(cond_expr), Box::new(then_block), None),
                span,
            });
        }

        // consume '{'
        let _ = self.cursor.consume(
            TokenKind::LeftParen,
            "Expected a '{' after an else statement.",
        )?;
        let else_block = self.block_statement()?;
        Ok(Stmt {
            kind: StmtKind::If(
                Box::new(cond_expr),
                Box::new(then_block),
                Some(Box::new(else_block)),
            ),
            span,
        })
    }

    /// parse an expression followed by a semicolon.
    fn expression_statement<'parser>(&'parser mut self) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        let mut expression_parser = ExprParser::new(&mut self.cursor);
        let expr = expression_parser.parse()?;
        let _ = self.cursor.consume(
            TokenKind::Semicolon,
            "Expected ';' add the end of statement.",
        )?;
        Ok(Stmt {
            span: expr.span.clone(),
            kind: StmtKind::Expr(Box::new(expr)),
        })
    }

    /// Parse source into statements
    pub fn parse<'parser>(&'parser mut self) -> Result<Vec<Stmt>, ParseError>
    where
        'input: 'parser,
    {
        let _ = self.cursor.advance()?;

        let mut statements = Vec::new();
        loop {
            if self.cursor.matches(TokenKind::Eof)? {
                break;
            }
            // parse declaration, forward
            // errors (if any) and move the token cursor to the next
            // statement. (allow recovery in REPL environment)
            match self.declaration() {
                Err(e) => {
                    let span = self.cursor.current_position();
                    dbg!("error at {span}");
                    self.cursor.move_to_next_stmt();
                    return Err(e);
                }
                Ok(stmt) => statements.push(stmt),
            }
        }

        Ok(statements)
    }
}

#[cfg(test)]
mod stmt_parsing {
    use super::*;
    use lexer::{Lexer, StrPeeker, TokenKind, Tokenize};

    fn parse_statement(src: &str) -> Result<Vec<Stmt>, ParseError> {
        let lexer: Lexer<StrPeeker<'_, 64>> = Lexer::from_str(src);
        let mut parser = StmtParser::new(Box::new(lexer));
        parser.parse()
    }

    #[test]
    /// test parsing a single expression statement
    fn parse_expression_statement() {
        let src = "1;";
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt { kind: StmtKind::Expr(expr), .. }) = ast.pop() else {
            panic!("failed to parse expression statement.") };
        assert_eq!(expr.kind, ExprKind::Literal(LiteralKind::Num(1.)));
    }

    #[test]
    /// test parsing a print statement
    fn parse_print_statement() {
        let src = r#"print "hello";"#;
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt { kind: StmtKind::Print(expr), .. }) = ast.pop() else {
            panic!("failed to parse print statement.") };
        assert_eq!(
            expr.kind,
            ExprKind::Literal(LiteralKind::Str("hello".to_string()))
        );
    }

    #[test]
    /// test parsing a var statement
    fn parse_var_statement() {
        let src = "var a;";
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt { kind: StmtKind::Var(id, expr), .. }) = ast.pop() else {
            panic!("failed to parse Var statement.") };
        assert_eq!(id, "a".to_string());
        assert_eq!(expr.kind, ExprKind::Literal(LiteralKind::Nil));
    }

    #[test]
    fn parse_var_statement_with_initializer() {
        let src = "var a = 1;";
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt { kind: StmtKind::Var(id, expr), .. }) = ast.pop() else {
            panic!("failed to parse Var statement.") };
        assert_eq!(id, "a".to_string());
        assert_eq!(expr.kind, ExprKind::Literal(LiteralKind::Num(1.)));
    }

    #[test]
    fn parse_block_statement() {
        let src = "{ 1; }";
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt { kind: StmtKind::Block(mut statements), .. }) = ast.pop() else {
            panic!("failed to parse Block statement.") };

        let Some(Stmt { kind: StmtKind::Expr(expr), .. }) = statements.pop() else {
            panic!("failed to parse inner expression in Block statement.") };
        assert_eq!(expr.kind, ExprKind::Literal(LiteralKind::Num(1.)));
    }

    #[test]
    fn parse_if_statement() {
        let src = "if (true) { 1; }";
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt { kind: StmtKind::If(cond_expr, then_block, None), .. }) = ast.pop() else {
            panic!("failed to parse If statement.") };
        assert_eq!(cond_expr.kind, ExprKind::Literal(LiteralKind::Bool(true)));
        let Stmt { kind: StmtKind::Block(_), .. } = *then_block else {
            panic!("failed to parse then branch as Block statement.") };
    }

    #[test]
    fn parse_if_statement_with_else_branch() {
        let src = "if (true) { 1; } else { 2; }";
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt { kind: StmtKind::If(cond_expr, then_block, Some(else_block)), .. }) = ast.pop() else {
            panic!("failed to parse If statement.") };
        assert_eq!(cond_expr.kind, ExprKind::Literal(LiteralKind::Bool(true)));
        let Stmt { kind: StmtKind::Block(_), .. } = *then_block else {
            panic!("failed to parse 'then' branch as Block statement.") };
        let Stmt { kind: StmtKind::Block(_), .. } = *else_block else {
            panic!("failed to parse 'else' branch as Block statement.") };
    }
}
