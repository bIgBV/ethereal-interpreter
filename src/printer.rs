use crate::parser::{Expr, Visitor};

#[derive(Debug)]
pub struct Printer {
    buf: String,
}

impl Printer {
    pub fn new() -> Self {
        Self { buf: String::new() }
    }

    pub fn print(&mut self, expr: &Expr) -> String {
        self.expr(expr);
        let ret = self.buf.clone();
        self.buf.clear();
        ret
    }

    fn expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Literal(_) => self.visit_literal(expr),
            Expr::Binary(_) => self.visit_binary(expr),
            Expr::Unary(_) => self.visit_unary(expr),
            Expr::Group(_) => self.visit_group(expr),
        }
    }
}

impl Visitor for Printer {
    type Out = ();

    fn visit_binary(&mut self, expr: &Expr) -> Self::Out {
        if let Expr::Binary(e) = expr {
            self.buf.push_str(" binary ");
            self.expr(&e.left);
            let operator = format!("{}", e.operator.0);
            self.buf.push_str(operator.as_str());
            self.expr(&e.right)
        }
    }

    fn visit_literal(&mut self, expr: &Expr) -> Self::Out {
        if let Expr::Literal(tok) = expr {
            let out = format!(" lit {}", tok);
            self.buf.push_str(out.as_str());
        }
    }

    fn visit_unary(&mut self, expr: &Expr) -> Self::Out {
        if let Expr::Unary(e) = expr {
            let out = format!(" unary {}", e.operator);
            self.buf.push_str(out.as_str());
            self.expr(&e.right);
        }
    }

    fn visit_group(&mut self, expr: &Expr) -> Self::Out {
        if let Expr::Group(e) = expr {
            self.buf.push_str(" ( ");
            self.expr(e);
            self.buf.push_str(" ) ");
        }
    }
}
