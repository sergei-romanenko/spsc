use crate::algebra::*;
use crate::language::*;

use std::rc::Rc;

fn a_var_is_under_attack(t: RcTerm) -> bool {
  match &*t {
    Term::Var { .. } => true,
    Term::CFG { kind, args, .. } if *kind == TKind::GCall => {
      a_var_is_under_attack(Rc::clone(&args[0]))
    }
    _ => false,
  }
}

// This is the "classic" homeomorphic imbedding relation.

fn he_by_diving(t1: RcTerm, t2: RcTerm) -> bool {
  match &*t2 {
    Term::CFG { args, .. } => {
      args.iter().any(|t| he(Rc::clone(&t1), Rc::clone(t)))
    }
    _ => false,
  }
}

fn he_by_coupling(t1: RcTerm, t2: RcTerm) -> bool {
  match (&*t1, &*t2) {
    (Term::Var { .. }, Term::Var { .. }) => true,
    (Term::CFG { args: args1, .. }, Term::CFG { args: args2, .. }) => {
      the_same_functor(&t1, &t2) && {
        args1
          .iter()
          .zip(args2.iter())
          .all(|(a1, a2)| he(Rc::clone(a1), Rc::clone(a2)))
      }
    }
    (_, _) => false,
  }
}

pub fn he(t1: RcTerm, t2: RcTerm) -> bool {
  he_by_diving(Rc::clone(&t1), Rc::clone(&t2)) || he_by_coupling(t1, t2)
}

// Enhanced homeomorphic embedding:
// expressions are compared only if they belong
// to the same category (as defined by `aVarIsUnderAttack`).

pub fn embedded_in(t1: RcTerm, t2: RcTerm) -> bool {
  a_var_is_under_attack(Rc::clone(&t1)) == a_var_is_under_attack(Rc::clone(&t2))
    && he(t1, t2)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::parser::*;

  fn run_aviua(t: &str) -> bool {
    a_var_is_under_attack(parse_term(t))
  }

  fn run_he(t1: &str, t2: &str) -> bool {
    he(parse_term(t1), parse_term(t2))
  }

  #[test]
  fn test_a_var_is_under_attack() {
    assert!(run_aviua("x"));
    assert!(!run_aviua("A()"));
    assert!(!run_aviua("f(x)"));
    assert!(run_aviua("g(x,y)"));
    assert!(run_aviua("g1(g2(x))"));
    assert!(!run_aviua("g(A())"));
    assert!(!run_aviua("g(f(x))"));
  }

  #[test]
  fn test_he_vv_vf_fv() {
    assert!(run_he("v1", "v2"));
    assert!(run_he("v1", "f(v2)"));
    assert!(!run_he("f(v2)", "v1"));
  }

  #[test]
  fn test_diving() {
    assert!(run_he("F(v1)", "G(v0,F(H(v2)))"));
  }

  #[test]
  fn test_coupling() {
    assert!(run_he("F(v1,G(v2))", "F(H(w1),G(w2))"));
    assert!(!run_he("F(v1)", "G(w1)"));
  }
}
