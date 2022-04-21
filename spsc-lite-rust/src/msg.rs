use crate::algebra::*;
use crate::language::*;

use std::fmt;
use std::rc::Rc;

pub struct Gen {
    pub t: RcTerm,
    pub s1: Subst,
    pub s2: Subst,
}

impl fmt::Display for Gen {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let t_s = self.t.to_string();
        let s1_s = subst_to_string(&self.s1);
        let s2_s = subst_to_string(&self.s2);
        write!(f, "{} =>> {{{}}}{{{}}}", t_s, s1_s, s2_s)
    }
}

fn common_functor(
    ng: &mut NameGen,
    t: &RcTerm,
    s1: &mut Subst,
    s2: &mut Subst,
) -> RcTerm {
    let ks: Vec<Name> = s1.keys().map(|k| k.clone()).collect();
    for n in ks {
        let t1 = Rc::clone(s1.get(&n).unwrap());
        let t2 = Rc::clone(s2.get(&n).unwrap());
        match (&*t1, &*t2) {
            (
                Term::CFG {
                    kind: kind1,
                    name: name1,
                    args: args1,
                    ..
                },
                Term::CFG { args: args2, .. },
            ) if the_same_functor(&t1, &t2) => {
                let ns = (*ng).fresh_name_list(args1.len());
                s1.remove(&n);
                for (k, arg) in ns.iter().zip(args1.iter()) {
                    s1.insert(k.clone(), Rc::clone(arg));
                }
                s2.remove(&n);
                for (k, arg) in ns.iter().zip(args2.iter()) {
                    s2.insert(k.clone(), Rc::clone(arg));
                }
                let nvs = ns.iter().map(|n0| Term::var(n0)).collect();
                // return apply_subst(&Subst::from_iter(nvs), t.clone());
                return apply_subst(
                    &Subst::from([(
                        n,
                        Term::mk_cfg(kind1.clone(), name1, nvs),
                    )]),
                    t,
                );
            }
            _ => (),
        }
    }
    return Rc::clone(t);
}

fn find_common_subst(s1: &Subst, s2: &Subst) -> Option<(Name, Name)> {
    for (n1, t1) in s1 {
        for (n2, t2) in s1 {
            if (n1 != n2 && t1 == t2) && (s2[n1] == s2[n2]) {
                return Some((n1.clone(), n2.clone()));
            }
        }
    }
    return None;
}

fn common_subst(t: &RcTerm, s1: &mut Subst, s2: &mut Subst) -> RcTerm {
    if let Some((n1, n2)) = find_common_subst(&s1, &s2) {
        s1.remove(&n1);
        s2.remove(&n1);
        let m: Subst = Subst::from([(n1.clone(), Term::var(&n2))]);
        return apply_subst(&m, t);
    } else {
        Rc::clone(t)
    }
}

pub fn msg(ng: &mut NameGen, t1: &RcTerm, t2: &RcTerm) -> Gen {
    let n = ng.fresh_name();
    let mut s1 = Subst::from([(n.clone(), Rc::clone(t1))]);
    let mut s2 = Subst::from([(n.clone(), Rc::clone(t2))]);
    let mut t = Term::var(&n);
    loop {
        let old_t = t.clone();
        let t_cf = common_functor(ng, &t, &mut s1, &mut s2);
        t = common_subst(&t_cf, &mut s1, &mut s2);
        if t == old_t {
            break;
        }
    }
    return Gen { t, s1, s2 };
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::*;

    fn msg_ok(t1: &str, t2: &str, expected: &str) {
        let mut ng = NameGen::new("v", 1);
        let gen = msg(&mut ng, &parse_term(t1), &parse_term(t2));
        assert_eq!(expected, gen.to_string());
    }

    #[test]
    fn test_msg_common_functor() {
        msg_ok(
            "A(a1,a2)",
            "A(b1,b2)",
            "A(v2,v3) =>> {v2->a1;v3->a2;}{v2->b1;v3->b2;}",
        );
        msg_ok(
            "A(a1,C(a2,a3))",
            "A(b1,C(b2,b3))",
            "A(v2,C(v4,v5)) =>> {v2->a1;v4->a2;v5->a3;}{v2->b1;v4->b2;v5->b3;}",
        );
    }
    #[test]
    fn test_msg_merge_subterms() {
        msg_ok(
            "f(a1,a2,a1)",
            "f(b1,b2,b1)",
            "f(v4,v3,v4) =>> {v3->a2;v4->a1;}{v3->b2;v4->b1;}",
        );
        msg_ok(
            "f(a,a)",
            "f(b,S(b))",
            "f(v2,v3) =>> {v2->a;v3->a;}{v2->b;v3->S(b);}",
        );
    }
}
