use super::ast::{
    Expr as GenericExpr, ExprKind as GenericExprKind, LiteralKind,
    Stmt as GenericStmt, StmtKind as GenericStmtKind,
};
use super::expr_parser::ExprParser;
use super::parser_state::{ParseError, ParserState};
use lexer::{Token, TokenKind, Tokenize};

// in this file, use simply use
// strings to represent symbols.
pub type Expr = GenericExpr<String>;
pub type ExprKind = GenericExprKind<String>;
pub type Stmt = GenericStmt<String>;
pub type StmtKind = GenericStmtKind<String>;

/// Parse statements using an explicit recursive
/// descent parser, and expressions using a Pratt Parser.
pub struct StmtParser<'input> {
    state: ParserState<'input>,
}

impl<'input> StmtParser<'input> {
    pub fn new(lexer: Box<dyn Tokenize + 'input>) -> Self {
        let state = ParserState::new(lexer);
        Self { state }
    }

    /**
     * This is the topmost step of a recursive descent parser,
     * it parses the following grammar:
     *
     * declaration -> varDeclaration
     *                | classDeclaration
     *                | funDeclaration
     *                | statement;
     */
    fn declaration<'parser>(&'parser mut self) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        let current: &TokenKind = &self.state.current()?.kind;
        match *current {
            TokenKind::Var => {
                // position state rigt after `var` token;
                self.state.advance()?;
                self.var_declaration()
            }
            TokenKind::Class => {
                self.state.advance()?;
                self.class_declaration()
            }
            TokenKind::Fun => {
                self.state.advance()?;
                self.fun_declaration()
            }
            _ => self.statement(),
        }
    }

    /**
     * Recursively parse the following statements:
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
        let current: &TokenKind = &self.state.current()?.kind;
        match *current {
            TokenKind::Print => {
                // position state right after `print
                let _ = self.state.advance();
                self.print_statement()
            }
            TokenKind::LeftBrace => {
                // position state right after `{`
                let _ = self.state.advance();
                self.block_statement()
            }
            TokenKind::If => {
                // position state right after `if
                let _ = self.state.advance();
                self.if_statement()
            }
            TokenKind::Return => {
                self.state.advance()?;
                self.return_statement()
            }
            TokenKind::While => {
                self.state.advance()?;
                self.while_statement()
            }
            TokenKind::For => {
                self.state.advance()?;
                self.for_statement()
            }
            TokenKind::For => {
                self.state.advance()?;
                self.for_statement()
            }
            _ => self.expression_statement(),
        }
    }

    /// parse a var declaration
    /// assume `var` has been parsed
    fn var_declaration<'parser>(&'parser mut self) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        // store span of `var` token
        let Token { span: var_span, .. } = self.state.take_previous()?;

        // parse next expression
        let expr = self.parse_one_expression()?;
        self.state.consume(
            TokenKind::Semicolon,
            "Expected ';' after print statement.",
        )?;

        match expr {
            // case with no initializer
            Expr {
                kind: ExprKind::Variable(identifier),
                id: var_id,
                span,
            } => {
                let nil_expr = Expr {
                    kind: ExprKind::Literal(LiteralKind::Nil),
                    id: self.state.new_node_id(),
                    span,
                };
                Ok(Stmt {
                    id: var_id,
                    kind: StmtKind::Var(identifier, Box::new(nil_expr)),
                    span: var_span,
                })
            }
            // case with initializer
            Expr {
                kind: ExprKind::Assign(identifier, initializer),
                ..
            } => Ok(Stmt {
                id: self.state.new_node_id(),
                kind: StmtKind::Var(identifier, initializer),
                span: var_span,
            }),
            _ => Err(ParseError::ExpectedExpression("Expected variable name.")),
        }
    }

    /// parse a class declaration (including its methods)
    /// assume `class` has been parsed.
    fn class_declaration<'parser>(&'parser mut self) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        // store span of `class` token
        let Token {
            span: class_span, ..
        } = self.state.take_previous()?;

        // parse class name
        self.state.advance()?;
        let Token {
            kind: TokenKind::Identifier(class_name),
            ..
        } = self.state.take_previous()?
        else {
            return Err(ParseError::ExpectedToken(
                "expected class name in class declaration.",
            ));
        };

        // parse super class name if any
        let mut super_class: Option<String> = None;
        if self.state.matches(TokenKind::Less)? {
            self.state.advance()?;
            let Token {
                kind: TokenKind::Identifier(name),
                ..
            } = self.state.take_previous()?
            else {
                return Err(ParseError::ExpectedToken(
                    "expected identifier in class inheritance declaration.",
                ));
            };
            super_class = Some(name);
        }

        self.state.consume(
            TokenKind::LeftBrace,
            "Expected '{' after class declaration.",
        )?;

        let mut methods: Vec<Stmt> = Vec::new();
        while !self.state.check(TokenKind::RightBrace)? {
            self.state.advance()?;
            let Token {
                kind: TokenKind::Identifier(name),
                span,
            } = self.state.take_previous()?
            else {
                return Err(ParseError::ExpectedToken(
                    "expected method name in class declaration.",
                ));
            };
            let arguments = self.parse_function_args()?;
            let body = self.parse_function_body()?;
            let method = Stmt {
                id: self.state.new_node_id(),
                kind: StmtKind::Function(name, arguments, body),
                span,
            };
            methods.push(method);
        }

        self.state.consume(
            TokenKind::RightBrace,
            "Expected '{' after class declaration.",
        )?;
        Ok(Stmt {
            id: self.state.new_node_id(),
            kind: StmtKind::Class(class_name, super_class, methods),
            span: class_span,
        })
    }

    fn fun_declaration<'parser>(&'parser mut self) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        let Token { span, .. } = self.state.take_previous()?;
        let _ = self.state.advance();
        let Token {
            kind: TokenKind::Identifier(fn_name),
            ..
        } = self.state.take_previous()?
        else {
            return Err(ParseError::ExpectedToken(
                "expected identifier in function declaration.",
            ));
        };
        // parse arguments
        let arguments: Vec<String> = self.parse_function_args()?;
        // parse body
        let body: Vec<Stmt> = self.parse_function_body()?;
        Ok(Stmt {
            id: self.state.new_node_id(),
            kind: StmtKind::Function(fn_name, arguments, body),
            span,
        })
    }

    /// parse block statement
    /// assumes `{` has been parsed.
    fn block_statement<'parser>(&'parser mut self) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        let Token { span, .. } = self.state.take_previous()?;

        // parse sub-statements until we encounter a '}'
        let mut statements: Vec<Stmt> = Vec::new();
        while !self.state.check(TokenKind::RightBrace)? {
            let stmt = self.declaration()?;
            statements.push(stmt);
        }
        self.state
            .consume(TokenKind::RightParen, "Expect '}' after block.")?;

        Ok(Stmt {
            id: self.state.new_node_id(),
            kind: StmtKind::Block(statements),
            span,
        })
    }

    /// parse return statements,
    /// assumes that `return` has just been parsed.
    fn return_statement<'parser>(&'parser mut self) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        // span of the `return` token
        let Token { span, .. } = self.state.take_previous()?;
        let mut maybe_expr: Option<Box<Expr>> = None;
        if !self.state.check(TokenKind::Semicolon)? {
            let expr = self.parse_one_expression()?;
            maybe_expr = Some(Box::new(expr));
        }
        self.state.consume(
            TokenKind::Semicolon,
            "Expected ';' add the end of return statement.",
        )?;
        Ok(Stmt {
            id: self.state.new_node_id(),
            kind: StmtKind::Return(maybe_expr),
            span,
        })
    }

    /// parse a while statement,
    /// assumes that `while` as just been parsed.
    fn while_statement<'parser>(&'parser mut self) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        // span of the `while` token
        let Token { span, .. } = self.state.take_previous()?;
        self.state
            .consume(TokenKind::LeftParen, "Expected '(' after 'while'.")?;
        let condition_expression = self.parse_one_expression()?;
        self.state
            .consume(TokenKind::RightBrace, "Expected ')' after 'while'.")?;
        self.state.consume(
            TokenKind::LeftBrace,
            "Expected '{' after while condition.",
        )?;
        let stmt = self.block_statement()?;
        Ok(Stmt {
            id: self.state.new_node_id(),
            kind: StmtKind::While(
                Box::new(condition_expression),
                Box::new(stmt),
            ),
            span,
        })
    }

    /// parse a for statement,
    /// assumes that the `for` yoken has just been parsed
    fn for_statement<'parser>(&'parser mut self) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        // span of the `for` token
        let Token { span, .. } = self.state.take_previous()?;
        let mut initializer: Option<Box<Expr>> = None;
        let mut condition: Option<Box<Expr>> = None;
        let mut increment: Option<Box<Expr>> = None;

        // parse initializer
        self.state
            .consume(TokenKind::LeftParen, "Expected '(' after 'for'.")?;
        if !self.state.matches(TokenKind::Semicolon)? {
            initializer = Some(Box::new(self.parse_one_expression()?));
            self.state.consume(
                TokenKind::Semicolon,
                "Expected ';' after initializer in 'for' statement.",
            )?;
        }
        // parse condition
        if !self.state.matches(TokenKind::Semicolon)? {
            condition = Some(Box::new(self.parse_one_expression()?));
            self.state.consume(
                TokenKind::Semicolon,
                "Expected ';' after condition in 'for' statement.",
            )?;
        }
        // parse increment
        if !self.state.matches(TokenKind::RightParen)? {
            increment = Some(Box::new(self.parse_one_expression()?));
            self.state.consume(
                TokenKind::RightParen,
                "Expected ')' after increment in 'for' statement.",
            )?;
        }

        // parse block
        self.state.consume(
            TokenKind::LeftBrace,
            "Expected '{' after in 'for' statement.",
        )?;
        let block = self.block_statement()?;
        Ok(Stmt {
            id: self.state.new_node_id(),
            kind: StmtKind::For(
                initializer,
                condition,
                increment,
                Box::new(block),
            ),
            span,
        })
    }
    /// parse a print statement,
    /// assume `print` has been parsed
    fn print_statement<'parser>(&'parser mut self) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        // store 'print' span
        let Token { span, .. } = self.state.take_previous()?;

        // parse following expression
        let expr = self.parse_one_expression()?;
        self.state.consume(
            TokenKind::Semicolon,
            "Expected ';' after print statement.",
        )?;

        Ok(Stmt {
            id: self.state.new_node_id(),
            kind: StmtKind::Print(Box::new(expr)),
            span,
        })
    }

    fn if_statement<'parser>(&'parser mut self) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        // store 'if' span
        let Token { span, .. } = self.state.take_previous()?;

        // consume '('
        self.state.consume(
            TokenKind::LeftParen,
            "Expected a parenthesised condition after an if statement.",
        )?;
        // parse condition
        let cond_expr = self.parse_one_expression()?;
        // consume ')'
        self.state.consume(
            TokenKind::LeftParen,
            "Expected a closing after an 'if' condition.",
        )?;
        // parse then branch
        if !self.state.matches(TokenKind::LeftBrace)? {
            return Err(ParseError::ExpectedExpression(
                "Expect a 'then' branch after an if condition (eg: '{')",
            ));
        }
        let then_block = self.block_statement()?;

        // case with no `else` branch
        if !self.state.matches(TokenKind::Else)? {
            return Ok(Stmt {
                id: self.state.new_node_id(),
                kind: StmtKind::If(
                    Box::new(cond_expr),
                    Box::new(then_block),
                    None,
                ),
                span,
            });
        }

        // consume '{'
        self.state.consume(
            TokenKind::LeftParen,
            "Expected a '{' after an else statement.",
        )?;
        let else_block = self.block_statement()?;
        Ok(Stmt {
            id: self.state.new_node_id(),
            kind: StmtKind::If(
                Box::new(cond_expr),
                Box::new(then_block),
                Some(Box::new(else_block)),
            ),
            span,
        })
    }

    fn parse_one_expression(&mut self) -> Result<Expr, ParseError> {
        let mut expression_parser = ExprParser::new(&mut self.state);
        expression_parser.parse()
    }

    fn parse_function_args(&mut self) -> Result<Vec<String>, ParseError> {
        // parse arguments
        let mut arguments: Vec<String> = Vec::new();
        self.state.consume(
            TokenKind::LeftParen,
            "Expected '(' after function declaration.",
        )?;
        while !self.state.matches(TokenKind::RightParen)? {
            self.state.advance()?;
            let Token {
                kind: TokenKind::Identifier(arg_name),
                ..
            } = self.state.take_previous()?
            else {
                return Err(ParseError::ExpectedToken(
                    "expected identifier in function arguments.",
                ));
            };
            arguments.push(arg_name);

            if self.state.check(TokenKind::Comma)? {
                self.state.advance()?;
            }
        }
        Ok(arguments)
    }

    fn parse_function_body(&mut self) -> Result<Vec<Stmt>, ParseError> {
        self.state.consume(
            TokenKind::LeftBrace,
            "Expected function body after arguments in function declaration.",
        )?;
        let mut statements: Vec<Stmt> = Vec::new();
        while !self.state.check(TokenKind::RightBrace)? {
            let stmt = self.declaration()?;
            statements.push(stmt);
        }
        self.state.consume(
            TokenKind::RightBrace,
            "Expected '}' after function body.",
        )?;
        Ok(statements)
    }

    /// parse an expression followed by a semicolon.
    fn expression_statement<'parser>(
        &'parser mut self,
    ) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        let expr = self.parse_one_expression()?;
        self.state.consume(
            TokenKind::Semicolon,
            "Expected ';' add the end of statement.",
        )?;
        Ok(Stmt {
            id: self.state.new_node_id(),
            span: expr.span.clone(),
            kind: StmtKind::Expr(Box::new(expr)),
        })
    }

    /// Parse source into statements
    pub fn parse<'parser>(&'parser mut self) -> Result<Vec<Stmt>, ParseError>
    where
        'input: 'parser,
    {
        self.state.advance()?;

        let mut statements = Vec::new();
        loop {
            if self.state.matches(TokenKind::Eof)? {
                break;
            }
            // parse declaration, forward
            // errors (if any) and move the token state to the next
            // statement. (allow recovery in REPL environment)
            match self.declaration() {
                Err(e) => {
                    let span = self.state.current_position();
                    dbg!("error at {:?}", span);
                    self.state.move_to_next_stmt();
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
    use lexer::{Lexer, StrPeeker};

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
        let Some(Stmt {
            kind: StmtKind::Expr(expr),
            ..
        }) = ast.pop()
        else {
            panic!("failed to parse expression statement.")
        };
        assert_eq!(expr.kind, ExprKind::Literal(LiteralKind::Num(1.)));
    }

    #[test]
    /// test parsing a print statement
    fn parse_print_statement() {
        let src = r#"print "hello";"#;
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt {
            kind: StmtKind::Print(expr),
            ..
        }) = ast.pop()
        else {
            panic!("failed to parse print statement.")
        };
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
        let Some(Stmt {
            kind: StmtKind::Var(id, expr),
            ..
        }) = ast.pop()
        else {
            panic!("failed to parse Var statement.")
        };
        assert_eq!(id, "a".to_string());
        assert_eq!(expr.kind, ExprKind::Literal(LiteralKind::Nil));
    }

    #[test]
    fn parse_var_statement_with_initializer() {
        let src = "var a = 1;";
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt {
            kind: StmtKind::Var(id, expr),
            ..
        }) = ast.pop()
        else {
            panic!("failed to parse Var statement.")
        };
        assert_eq!(id, "a".to_string());
        assert_eq!(expr.kind, ExprKind::Literal(LiteralKind::Num(1.)));
    }

    #[test]
    fn parse_block_statement() {
        let src = "{ 1; }";
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt {
            kind: StmtKind::Block(mut statements),
            ..
        }) = ast.pop()
        else {
            panic!("failed to parse Block statement.")
        };

        let Some(Stmt {
            kind: StmtKind::Expr(expr),
            ..
        }) = statements.pop()
        else {
            panic!("failed to parse inner expression in Block statement.")
        };
        assert_eq!(expr.kind, ExprKind::Literal(LiteralKind::Num(1.)));
    }

    #[test]
    fn parse_if_statement() {
        let src = "if (true) { 1; }";
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt {
            kind: StmtKind::If(cond_expr, then_block, None),
            ..
        }) = ast.pop()
        else {
            panic!("failed to parse If statement.")
        };
        assert_eq!(cond_expr.kind, ExprKind::Literal(LiteralKind::Bool(true)));
        let Stmt {
            kind: StmtKind::Block(_),
            ..
        } = *then_block
        else {
            panic!("failed to parse then branch as Block statement.")
        };
    }

    #[test]
    fn parse_if_statement_with_else_branch() {
        let src = "if (true) { 1; } else { 2; }";
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt {
            kind: StmtKind::If(cond_expr, then_block, Some(else_block)),
            ..
        }) = ast.pop()
        else {
            panic!("failed to parse If statement.")
        };
        assert_eq!(cond_expr.kind, ExprKind::Literal(LiteralKind::Bool(true)));
        let Stmt {
            kind: StmtKind::Block(_),
            ..
        } = *then_block
        else {
            panic!("failed to parse 'then' branch as Block statement.")
        };
        let Stmt {
            kind: StmtKind::Block(_),
            ..
        } = *else_block
        else {
            panic!("failed to parse 'else' branch as Block statement.")
        };
    }

    #[test]
    fn parse_naked_return_statement() {
        let src = "return;";
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt {
            kind: StmtKind::Return(None),
            ..
        }) = ast.pop()
        else {
            panic!("failed to parse Return statement.")
        };
    }

    #[test]
    fn parse_return_statement_with_value() {
        let src = "return true;";
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt {
            kind: StmtKind::Return(Some(expr)),
            ..
        }) = ast.pop()
        else {
            panic!("failed to parse Return statement.")
        };
        assert_eq!(expr.kind, ExprKind::Literal(LiteralKind::Bool(true)));
    }

    #[test]
    fn parse_while_statement() {
        let src = "while (true) {}";
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt {
            kind: StmtKind::While(expr, _block_stmt),
            ..
        }) = ast.pop()
        else {
            panic!("failed to parse while statement.")
        };
        assert_eq!(expr.kind, ExprKind::Literal(LiteralKind::Bool(true)));
    }

    #[test]
    fn parse_for_statement() {
        let src = "for (1; 2; 3) {}";
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt {
            kind:
                StmtKind::For(
                    Some(initializer),
                    Some(condition),
                    Some(increment),
                    _block,
                ),
            ..
        }) = ast.pop()
        else {
            panic!("failed to parse For statement.")
        };
        assert_eq!(initializer.kind, ExprKind::Literal(LiteralKind::Num(1.)));
        assert_eq!(condition.kind, ExprKind::Literal(LiteralKind::Num(2.)));
        assert_eq!(increment.kind, ExprKind::Literal(LiteralKind::Num(3.)));
    }

    #[test]
    fn parse_for_statement_without_initializer() {
        let src = "for (; 2; 3) {}";
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt {
            kind: StmtKind::For(None, Some(condition), Some(increment), _block),
            ..
        }) = ast.pop()
        else {
            panic!("failed to parse For statement.")
        };
        assert_eq!(condition.kind, ExprKind::Literal(LiteralKind::Num(2.)));
        assert_eq!(increment.kind, ExprKind::Literal(LiteralKind::Num(3.)));
    }

    #[test]
    fn parse_for_statement_without_condition() {
        let src = "for (1; ; 3) {}";
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt {
            kind:
                StmtKind::For(Some(initializer), None, Some(increment), _block),
            ..
        }) = ast.pop()
        else {
            panic!("failed to parse For statement.")
        };
        assert_eq!(initializer.kind, ExprKind::Literal(LiteralKind::Num(1.)));
        assert_eq!(increment.kind, ExprKind::Literal(LiteralKind::Num(3.)));
    }

    #[test]
    fn parse_for_statement_without_increment() {
        let src = "for (1; 2; ) {}";
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt {
            kind:
                StmtKind::For(Some(initializer), Some(condition), None, _block),
            ..
        }) = ast.pop()
        else {
            panic!("failed to parse For statement.")
        };
        assert_eq!(initializer.kind, ExprKind::Literal(LiteralKind::Num(1.)));
        assert_eq!(condition.kind, ExprKind::Literal(LiteralKind::Num(2.)));
    }

    #[test]
    fn parse_naked_for_statement() {
        let src = "for (;;) {}";
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt {
            kind: StmtKind::For(None, None, None, _block),
            ..
        }) = ast.pop()
        else {
            panic!("failed to parse For statement.")
        };
    }

    #[test]
    fn parse_function_declaration() {
        let src = "fun foo() {}";
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt {
            kind: StmtKind::Function(name, args, body),
            ..
        }) = ast.pop()
        else {
            panic!("failed to parse Function declaration statement.")
        };
        assert_eq!(&name, "foo");
        assert!(args.is_empty());
        assert!(body.is_empty());
    }

    #[test]
    fn parse_function_declaration_with_args() {
        let src = "fun foo(a, b) {}";
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt {
            kind: StmtKind::Function(name, args, body),
            ..
        }) = ast.pop()
        else {
            panic!("failed to parse Function declaration statement.")
        };
        assert_eq!(&name, "foo");
        assert_eq!(args, vec!["a".to_string(), "b".to_string()]);
        assert!(body.is_empty());
    }

    #[test]
    fn parse_function_declaration_with_args_and_body() {
        let src = "fun foo(a, b) { return 1; }";
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt {
            kind: StmtKind::Function(name, args, mut body),
            ..
        }) = ast.pop()
        else {
            panic!("failed to parse Function declaration statement.")
        };
        assert_eq!(&name, "foo");
        assert_eq!(args, vec!["a".to_string(), "b".to_string()]);
        let Some(_stmt) = body.pop() else {
            panic!("Failed to parse function body.");
        };
    }

    #[test]
    fn parse_simple_class_definition() {
        let src = r#"
            class foo {}
        "#;
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt {
            kind: StmtKind::Class(name, None, body),
            ..
        }) = ast.pop()
        else {
            panic!("failed to parse Class declaration statement.")
        };
        assert_eq!(&name, "foo");
        assert!(body.is_empty());
    }

    #[test]
    fn parse_class_definition_with_super_class() {
        let src = r#"
            class foo < bar {}
        "#;
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt {
            kind: StmtKind::Class(name, Some(super_name), body),
            ..
        }) = ast.pop()
        else {
            panic!("failed to parse Class declaration statement.")
        };
        assert_eq!(&name, "foo");
        assert_eq!(&super_name, "bar");
        assert!(body.is_empty());
    }

    #[test]
    fn parse_class_definition_with_methods() {
        let src = r#"
            class foo {
                bar() { }
                baz() { }
            }
        "#;
        let mut ast = parse_statement(src).unwrap();
        let Some(Stmt {
            kind: StmtKind::Class(name, None, mut body),
            ..
        }) = ast.pop()
        else {
            panic!("failed to parse Class declaration statement.")
        };
        assert_eq!(&name, "foo");
        let Some(Stmt {
            kind: StmtKind::Function(method_name, _args, _fn_body),
            ..
        }) = body.pop()
        else {
            panic!("Failed to parse class method.");
        };
        assert_eq!(&method_name, "baz");
        let Some(Stmt {
            kind: StmtKind::Function(method_name, _args, _fn_body),
            ..
        }) = body.pop()
        else {
            panic!("Failed to parse class method.");
        };
        assert_eq!(&method_name, "bar");
    }
}
