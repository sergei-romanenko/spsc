use crate::language::*;

use itertools::Itertools;
use std::collections::BTreeMap;
use std::rc::Rc;

pub fn vars(t: &RcTerm) -> Vec<Name> {
    // We don't use sets here, in order to preserve
    // the original order of variables in the expression.
    // (The order is preserved just for readability of
    // residual programs.)
    let mut vs: Vec<Name> = Vec::new();
    match &**t {
        Term::Var { name } => vs.push(name.clone()),
        Term::CFG { args, .. } => {
            for arg in args {
                for v in vars(&arg) {
                    if !(vs.contains(&v)) {
                        vs.push(v.clone());
                    }
                }
            }
        }
        Term::Let {
            body: _,
            bindings: _,
        } => (),
    }
    vs
}

pub fn the_same_functor(t1: &Term, t2: &Term) -> bool {
    match (t1, t2) {
        (
            Term::CFG {
                kind: kind1,
                name: name1,
                args: args1,
            },
            Term::CFG {
                kind: kind2,
                name: name2,
                args: args2,
            },
        ) => kind1 == kind2 && name1 == name2 && args1.len() == args2.len(),
        _ => false,
    }
}

pub type Subst = BTreeMap<Name, RcTerm>;

pub fn subst_to_string(s: &Subst) -> String {
    s.keys()
        .sorted()
        .map(|key| format!("{}->{};", key, s[key]))
        .join("")
}

pub fn apply_subst(s: &Subst, t: &RcTerm) -> RcTerm {
    match &**t {
        Term::Var { name } => match s.get(name) {
            None => Rc::clone(t),
            Some(t2) => Rc::clone(t2),
        },
        Term::CFG { kind, name, args } => {
            let args = args
                .iter()
                .map(|arg| apply_subst(s, arg))
                .collect();
            Rc::new(Term::CFG {
                kind: kind.clone(),
                name: name.clone(),
                args,
            })
        }
        _ => unimplemented!(),
    }
}

fn match_against_acc(s: &mut Subst, t1: &RcTerm, t2: &RcTerm) -> bool {
    match (&**t1, &**t2) {
        (Term::Var { name }, _) => match s.get(name) {
            Some(t) => *t == *t2,
            None => {
                s.insert(name.clone(), t2.clone());
                true
            }
        },
        (Term::CFG { args: args1, .. }, Term::CFG { args: args2, .. }) => {
            the_same_functor(&t1, &t2) && {
                for (a1, a2) in args1.iter().zip(args2.iter()) {
                    if !match_against_acc(s, &a1, &a2) {
                        return false;
                    }
                }
                true
            }
        }
        (_, _) => false,
    }
}

pub fn match_against(t1: &RcTerm, t2: &RcTerm) -> Option<Subst> {
    let mut s: Subst = Subst::new();
    if match_against_acc(&mut s, t1, t2) {
        Some(s)
    } else {
        None
    }
}

pub fn inst_of(t1: &RcTerm, t2: &RcTerm) -> bool {
    match_against(t2, t1).is_some()
}

pub fn equiv(t1: &RcTerm, t2: &RcTerm) -> bool {
    inst_of(t1, t2) && inst_of(t2, t1)
}

// Name generator

pub struct NameGen {
    prefix: String,
    tick: usize,
}

impl NameGen {
    pub fn new(prefix: &str, tick: usize) -> Self {
        NameGen {
            prefix: String::from(prefix),
            tick: tick,
        }
    }

    pub fn fresh_name(&mut self) -> String {
        let tick = self.tick;
        self.tick = tick + 1;
        format!("{}{}", self.prefix, tick)
    }

    pub fn fresh_name_list(&mut self, n: usize) -> Vec<String> {
        let tick = self.tick;
        self.tick = tick + n;
        (0..n)
            .map(|k| format!("{}{}", self.prefix, tick + k))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::*;

    #[test]
    fn test_vars() {
        assert_eq!(vec!["x", "y", "a"], vars(&parse_term("A(x,B(y,x),a)")));
    }

    fn run_the_same_functor(t1: &str, t2: &str) -> bool {
        the_same_functor(&parse_term(t1), &parse_term(t2))
    }

    #[test]
    fn test_the_same_functor() {
        assert!(run_the_same_functor("A", "A"));
        assert!(!run_the_same_functor("A", "A(x)"));
        assert!(run_the_same_functor("f(A)", "f(B)"));
        assert!(run_the_same_functor("g(A)", "g(B)"));
        assert!(!run_the_same_functor("A", "B"));
        assert!(!run_the_same_functor("A", "f()"));
    }

    #[test]
    fn test_subst_to_string() {
        let s: Subst = Subst::from([
            (String::from("x"), Term::var("a")),
            (String::from("y"), Term::var("b")),
        ]);
        assert_eq!("x->a;y->b;", subst_to_string(&s));
    }
    #[test]
    fn test_apply_subst() {
        let t1 = parse_term("t1");
        let t2 = parse_term("t2");
        let t = parse_term("Cons(x1,Cons(x2,Cons(x3,Nil)))");
        let s: Subst =
            Subst::from([(String::from("x1"), t1), (String::from("x2"), t2)]);
        assert_eq!(
            "Cons(t1,Cons(t2,Cons(x3,Nil)))",
            apply_subst(&s, &t).to_string()
        );
    }

    fn match_ok(t1: &str, t2: &str, expected: &str) {
        match match_against(&parse_term(t1), &parse_term(t2)) {
            Some(actual) => {
                assert_eq!(expected, subst_to_string(&actual));
            }
            None => {
                assert_eq!(expected, "*");
            }
        }
    }

    fn match_none(t1: &str, t2: &str) {
        assert_eq!(match_against(&parse_term(t1), &parse_term(t2)), None);
    }

    #[test]
    fn test_match_against() {
        match_ok("x", "S(Z)", "x->S(Z);");
        match_none("Z", "x");
        match_ok("C(x,y)", "C(A,B)", "x->A;y->B;");
        match_none("C(x,y)", "D(A,B)");
        match_none("C(x,y)", "f(A,B)");
        match_ok("C(x,x)", "C(A,A)", "x->A;");
        match_none("C(x,y)", "C(A,B,C)");
        match_none("C(x,y,z)", "C(A,B)");
    }

    fn run_test_equiv(t1: &str, t2: &str, expected: bool) {
        assert_eq!(expected, equiv(&parse_term(t1), &parse_term(t2)));
    }

    #[test]
    fn test_equiv() {
        run_test_equiv("gA(fB(x,y),C)", "gA(fB(a,b),C)", true);
        run_test_equiv("gA(fB(x,y),x)", "gA(fB(a,a),b)", false);
    }

    #[test]
    fn test_name_gen() {
        let mut ng = NameGen::new("v", 100);
        assert_eq!("v100", ng.fresh_name());
        assert_eq!("v101", ng.fresh_name());
        assert_eq!(vec!["v102", "v103", "v104"], ng.fresh_name_list(3));
    }
}
