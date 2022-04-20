use either::*;
use std::fmt;
use std::rc::Rc;

pub type Name = String;
pub type Params = Vec<Name>;
pub type RcTerm = Rc<Term>;
pub type Args = Vec<Rc<Term>>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TKind {
    Ctr,
    FCall,
    GCall,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Term {
    Var {
        name: Name,
    },

    CFG {
        kind: TKind,
        name: Name,
        args: Args,
    },

    Let {
        body: Rc<Term>,
        bindings: Vec<(Name, RcTerm)>,
    },
}

impl Term {
    pub fn var(name: &str) -> RcTerm {
        let v = Term::Var {
            name: String::from(name),
        };
        Rc::new(v)
    }

    pub fn mk_cfg(kind: TKind, name: &str, args: Args) -> RcTerm {
        let cfg = Term::CFG {
            kind: kind,
            name: String::from(name),
            args: args,
        };
        Rc::new(cfg)
    }

    pub fn ctr(name: &str, args: Args) -> RcTerm {
        Term::mk_cfg(TKind::Ctr, name, args)
    }

    pub fn fcall(name: &str, args: Args) -> RcTerm {
        Term::mk_cfg(TKind::FCall, name, args)
    }

    pub fn gcall(name: &str, args: Args) -> RcTerm {
        Term::mk_cfg(TKind::GCall, name, args)
    }

    pub fn mk_let(term: RcTerm, bindings: Vec<(Name, RcTerm)>) -> RcTerm {
        let let_term = Term::Let {
            body: term,
            bindings: bindings,
        };
        Rc::new(let_term)
    }

    pub fn is_var(&self) -> bool {
        match self {
            Term::Var { .. } => true,
            _ => false,
        }
    }

    pub fn is_ctr(&self) -> bool {
        match self {
            Term::CFG { kind, .. } if *kind == TKind::Ctr => true,
            _ => false,
        }
    }

    pub fn is_fg_call(&self) -> bool {
        match self {
            Term::CFG { kind, .. }
                if (*kind == TKind::FCall || *kind == TKind::GCall) =>
            {
                true
            }
            _ => false,
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Term::Var { name } => write!(f, "{}", name),
            Term::CFG { kind, name, args } => {
                let args: Vec<String> =
                    args.iter().map(|a| a.to_string()).collect();
                let args = if kind == &TKind::Ctr && args.is_empty() {
                    String::from("")
                } else {
                    format!("({})", args.join(","))
                };
                write!(f, "{}{}", name, args)
            }
            Term::Let { body, bindings } => {
                let bindings: Vec<String> = bindings
                    .iter()
                    .map(|(v, e)| {
                        format!("{}={}", v.to_string(), e.to_string())
                    })
                    .collect();
                write!(f, "let {} in {}", bindings.join(","), body)
            }
        }
    }
}

pub fn pat_to_string(cname: &str, cparams: &[&str]) -> String {
    let cparams = if cparams.is_empty() {
        String::default()
    } else {
        format!("({})", cparams.join(","))
    };
    format!("{}{}", cname, cparams)
}

#[derive(Debug, Clone)]
pub struct FRule {
    pub name: Name,
    pub params: Params,
    pub body: RcTerm,
}

#[derive(Debug, Clone)]
pub struct GRule {
    pub name: Name,
    pub cname: Name,
    pub cparams: Params,
    pub params: Params,
    pub body: RcTerm,
}

pub type Rule = Either<FRule, GRule>;

impl FRule {
    pub fn new(name: &str, params: &[&str], body: RcTerm) -> FRule {
        FRule {
            name: String::from(name),
            params: params.iter().map(|s| s.to_string()).collect(),
            body: body,
        }
    }

    pub fn rule(name: &str, params: &[&str], body: RcTerm) -> Rule {
        Left(FRule::new(name, params, body))
    }
}

impl GRule {
    pub fn new(
        name: &str,
        cname: &str,
        cparams: &[&str],
        params: &[&str],
        body: RcTerm,
    ) -> GRule {
        GRule {
            name: String::from(name),
            cname: String::from(cname),
            cparams: cparams.iter().map(|s| s.to_string()).collect(),
            params: params.iter().map(|s| s.to_string()).collect(),
            body: body,
        }
    }
    pub fn rule(
        name: &str,
        cname: &str,
        cparams: &[&str],
        params: &[&str],
        body: RcTerm,
    ) -> Rule {
        Right(GRule::new(name, cname, cparams, params, body))
    }
}

impl fmt::Display for FRule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let params: Vec<String> =
            self.params.iter().map(|a| a.to_string()).collect();
        let body: String = self.body.to_string();
        write!(f, "{}({})={};", self.name, params.join(","), body)
    }
}

impl fmt::Display for GRule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let cparams: Vec<&str> =
            self.cparams.iter().map(String::as_ref).collect();
        let pat: String = pat_to_string(&self.cname, &cparams);
        let params: Vec<String> =
            self.params.iter().map(|a| a.to_string()).collect();
        let body: String = self.body.to_string();
        let sep: &str = if params.is_empty() { "" } else { "," };
        write!(
            f,
            "{}({}{}{})={};",
            self.name,
            pat,
            sep,
            params.join(","),
            body
        )
    }
}

#[derive(Debug)]
pub struct Program {
    pub rules: Vec<Rule>,
}

impl Program {
    pub fn new(rules: Vec<Rule>) -> Program {
        Program { rules: rules }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let rules: Vec<String> =
            self.rules.iter().map(|a| a.to_string()).collect();
        write!(f, "{}", rules.join(""))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn args<const N: usize>(ts: [RcTerm; N]) -> Args {
        Vec::from(ts.map(|t| t))
    }

    // Display

    fn to_string_test<T: fmt::Display>(expected: &str, t: T) -> () {
        assert_eq!(expected, t.to_string());
    }

    #[test]
    fn display_term() {
        to_string_test("x", Term::var("x"));
        to_string_test("C", Term::ctr("C", vec![]));
        to_string_test("C(x)", Term::ctr("C", args([Term::var("x")])));
        to_string_test(
            "C(x,y)",
            Term::ctr("C", args([Term::var("x"), Term::var("y")])),
        );
        to_string_test(
            "fX(x,y)",
            Term::fcall("fX", args([Term::var("x"), Term::var("y")])),
        );
        to_string_test(
            "gX(x,y)",
            Term::gcall("gX", args([Term::var("x"), Term::var("y")])),
        );
        to_string_test(
            "let x=y in y",
            Term::mk_let(Term::var("y"), vec![("x".to_string(), Term::var("y"))]),
        );
        to_string_test(
            "let x=a,y=b in x",
            Term::mk_let(
                Term::var("x"),
                vec![
                    ("x".to_string(), Term::var("a")),
                    ("y".to_string(), Term::var("b")),
                ],
            ),
        );
    }

    #[test]
    fn display_pat() {
        assert_eq!("A", pat_to_string("A", &[]));
        assert_eq!("A(x)", pat_to_string("A", &["x"]));
        assert_eq!("A(x,y)", pat_to_string("A", &["x", "y"]));
    }

    #[test]
    fn display_rule() {
        to_string_test(
            "f(x,y)=y;",
            FRule::new("f", &["x", "y"], Term::var("y")),
        );
        to_string_test(
            "g(C(x),y)=y;",
            GRule::new("g", "C", &["x"], &["y"], Term::var("y")),
        );
        to_string_test(
            "g(C,y)=y;",
            GRule::new("g", "C", &[], &["y"], Term::var("y")),
        );
        to_string_test(
            "g(C)=C;",
            GRule::new("g", "C", &[], &[], Term::ctr("C", vec![])),
        );
    }
    #[test]
    fn display_program() {
        to_string_test(
            "f()=A;f1()=A1;",
            Program::new(vec![
                FRule::rule("f", &[], Term::ctr("A", vec![])),
                FRule::rule("f1", &[], Term::ctr("A1", vec![])),
            ]),
        );
        to_string_test(
            "g(C)=A;g1(C,x)=A;g2(C(x))=A;",
            Program::new(vec![
                GRule::rule("g", "C", &[], &[], Term::ctr("A", vec![])),
                GRule::rule("g1", "C", &[], &["x"], Term::ctr("A", vec![])),
                GRule::rule("g2", "C", &["x"], &[], Term::ctr("A", vec![])),
            ]),
        );
    }

    // Eq

    #[test]
    fn term_eq() {
        let nil: Args = Vec::new();

        assert!(Term::var("x") == Term::var("x"));
        assert!(Term::var("x") != Term::var("y"));
        assert!(Term::ctr("A", vec![]) == Term::ctr("A", vec![]));
        assert!(Term::ctr("A", vec![]) != Term::ctr("B", vec![]));
        assert!(nil == nil);
        assert!(vec![Term::var("x")] == vec![Term::var("x")]);
        assert!(vec![Term::var("x")] != vec![Term::var("y")]);
        assert!(vec![Term::var("x")] != vec![Term::var("x"), Term::var("z")]);
        assert!(
            Term::ctr("A", args([Term::var("x")]))
                == Term::ctr("A", args([Term::var("x")]))
        );
        assert!(
            Term::ctr("A", args([Term::var("x")]))
                != Term::ctr("A", args([Term::var("y")]))
        );
    }
}
