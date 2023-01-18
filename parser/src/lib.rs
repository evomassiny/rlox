mod ast;
mod cursor;
mod parser;

pub use crate::ast::{Expr, ExprKind, LiteralKind, Stmt, StmtKind};
pub use crate::parser::Parser;

#[cfg(test)]
mod tests {
    use super::*;
    use lexer::{Lexer, StrPeeker, TokenKind, Tokenize};

    #[test]
    fn parse_number_literal_stmt() {
        let src = "1;";
        let lexer: Lexer<StrPeeker<'_, 64>> = Lexer::from_str(src);
        let mut parser = parser::Parser::new(Box::new(lexer));

        let ast = parser.parse().unwrap().pop().unwrap();
        if let Stmt {
            kind: StmtKind::Expr(expr),
            ..
        } = ast
        {
            assert_eq!(expr.kind, ExprKind::Literal(LiteralKind::Num(1.)));
        } else {
            panic!("failed to parse literal");
        }
    }

    #[test]
    fn parse_sum_stmt() {
        let src = "1 + 2;";
        let lexer: Lexer<StrPeeker<'_, 64>> = Lexer::from_str(src);
        let mut parser = parser::Parser::new(Box::new(lexer));

        let ast = parser.parse().unwrap().pop().unwrap();
        let Stmt { kind: StmtKind::Expr(expr), .. } = ast else {
            panic!("failed to parse sum");
        };
        let ExprKind::Binary(lhs, token_kind, rhs) = (*expr).kind else { panic!("failed to parse sum") };
        assert_eq!(lhs.kind, ExprKind::Literal(LiteralKind::Num(1.)));
        assert_eq!(rhs.kind, ExprKind::Literal(LiteralKind::Num(2.)));
        assert_eq!(token_kind, TokenKind::Plus);
    }
}
