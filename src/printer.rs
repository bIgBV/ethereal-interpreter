use crate::common::{Expr, ExprVisitor};

#[derive(Debug)]
pub struct Printer {}

impl Printer {
    pub fn print(&self, expr: &Expr) -> String {
        self.visit_expr(expr)
    }
}

impl ExprVisitor for Printer {
    type Out = String;

    fn visit_expr(&self, expr: &Expr) -> Self::Out {
        match expr {
            Expr::Literal(_) => self.visit_literal(expr),
            Expr::Binary(_) => self.visit_binary(expr),
            Expr::Unary(_) => self.visit_unary(expr),
            Expr::Group(_) => self.visit_group(expr),
        }
    }

    fn visit_binary(&self, expr: &Expr) -> Self::Out {
        if let Expr::Binary(e) = expr {
            let left = self.visit_expr(&e.left);
            let operator = format!("{}", e.operator.0);
            let right = self.visit_expr(&e.right);
            format!(" ({} {} {})", left, operator, right)
        } else {
            format!("")
        }
    }

    fn visit_literal(&self, expr: &Expr) -> Self::Out {
        if let Expr::Literal(tok) = expr {
            format!(" {}", tok)
        } else {
            format!("")
        }
    }

    fn visit_unary(&self, expr: &Expr) -> Self::Out {
        if let Expr::Unary(e) = expr {
            let right = self.visit_expr(&e.right);
            format!(" {}{}", e.operator, right)
        } else {
            format!("")
        }
    }

    fn visit_group(&self, expr: &Expr) -> Self::Out {
        if let Expr::Group(e) = expr {
            let e = self.visit_expr(e);
            format!("( {} )", e)
        } else {
            format!("")
        }
    }
}
