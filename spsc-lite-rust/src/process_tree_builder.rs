use crate::algebra::*;
use crate::language::*;
use crate::process_tree::*;

use either::*;
use std::collections::BTreeMap;
use std::rc::Rc;

pub struct DrivingEngine {
    pub name_gen: NameGen,
    f_rules: BTreeMap<Name, FRule>,
    g_rules: BTreeMap<Name, Vec<GRule>>,
    gc_rule: BTreeMap<(Name, Name), GRule>,
}

impl DrivingEngine {
    pub fn new(ng: NameGen, prog: Program) -> DrivingEngine {
        let mut f_rules = BTreeMap::<Name, FRule>::new();
        let mut g_rules = BTreeMap::<Name, Vec<GRule>>::new();
        let mut gc_rules = BTreeMap::<(Name, Name), GRule>::new();

        for rule in prog.rules {
            match &rule {
                Either::Left(frule) => {
                    f_rules.insert(frule.name.clone(), frule.clone());
                }
                Either::Right(grule) => {
                    if let Some(gs) = g_rules.get_mut(&grule.name) {
                        gs.push(grule.clone());
                    } else {
                        g_rules.insert(grule.name.clone(), vec![grule.clone()]);
                    };
                    gc_rules.insert(
                        (grule.name.clone(), grule.cname.clone()),
                        grule.clone(),
                    );
                }
            }
        }

        DrivingEngine {
            name_gen: ng,
            f_rules: f_rules,
            g_rules: g_rules,
            gc_rule: gc_rules,
        }
    }

    fn driving_step(&mut self, t: &RcTerm) -> Vec<Branch> {
        match &**t {
            Term::Var { .. } => unimplemented!(),
            Term::CFG { kind, name, args } => match kind {
                TKind::Ctr => args
                    .iter()
                    .map(|arg| Branch {
                        term: Rc::clone(arg),
                        contr: None,
                    })
                    .collect(),
                TKind::FCall => {
                    let frule = &self.f_rules[name];
                    let mut p2a = Subst::new();
                    for (n, t) in frule.params.iter().zip(args.iter()) {
                        p2a.insert(n.clone(), Rc::clone(t));
                    }
                    let body = apply_subst(&p2a, &frule.body);
                    vec![Branch {
                        term: body,
                        contr: None,
                    }]
                }
                TKind::GCall => match &*args[0] {
                    Term::Var { name: vname } => {
                        let grules = &self.g_rules[name].clone();
                        let mut new_branches = Vec::new();
                        for grule in grules {
                            let cname = &grule.cname;
                            let cparams = self
                                .name_gen
                                .fresh_name_list(grule.cparams.len());
                            let cargs = cparams
                                .iter()
                                .map(|vn| Term::var(vn))
                                .collect();
                            let mut vname2ctr = Subst::new();
                            vname2ctr
                                .insert(vname.clone(), Term::ctr(cname, cargs));
                            let t1 = apply_subst(&vname2ctr, &t);
                            let branches = self.driving_step(&t1);
                            let t2 = &branches[0].term;
                            let branch = Branch {
                                term: Rc::clone(t2),
                                contr: Some(Rc::new(Contraction {
                                    vname: vname.clone(),
                                    cname: cname.clone(),
                                    cparams,
                                })),
                            };
                            new_branches.push(branch);
                        }
                        new_branches
                    }
                    Term::CFG {
                        kind: kind0,
                        name: cname,
                        args: cargs,
                    } if *kind0 == TKind::Ctr => {
                        let grule =
                            &self.gc_rule[&(name.clone(), cname.clone())];
                        let mut p2a = Subst::new();
                        for (n, t) in grule.cparams.iter().zip(cargs.iter()) {
                            p2a.insert(n.clone(), Rc::clone(t));
                        }
                        for (n, t) in grule.params.iter().zip(args[1..].iter())
                        {
                            p2a.insert(n.clone(), Rc::clone(t));
                        }
                        let body = apply_subst(&p2a, &grule.body);
                        vec![Branch {
                            term: body,
                            contr: None,
                        }]
                    }
                    _ => {
                        let branches0 = self.driving_step(&args[0]);
                        let mut new_branches = Vec::new();
                        for b in branches0 {
                            let mut b_args = Vec::new();
                            b_args.push(b.term);
                            for b_arg in &args[1..] {
                                b_args.push(Rc::clone(b_arg));
                            }
                            new_branches.push(Branch {
                                term: Rc::new(Term::CFG {
                                    kind: TKind::GCall,
                                    name: name.clone(),
                                    args: b_args,
                                }),
                                contr: b.contr,
                            });
                        }
                        new_branches
                    }
                },
            },
            Term::Let { body, bindings } => {
                let mut branches = Vec::new();
                branches.push(Branch {
                    term: Rc::clone(body),
                    contr: None,
                });
                for (_, t) in bindings {
                    branches.push(Branch {
                        term: Rc::clone(t),
                        contr: None,
                    });
                }
                branches
            }
        }
    }

    // This function applies a driving step to the node's expression,
    // and, in general, adds children to the node.

    pub fn expand_node(&mut self, tree: &mut Tree, beta: &RcNode) {
        let branches = self.driving_step(&beta.get_body());
        add_children(tree, beta, branches)
    }
}

// If beta `instOf` alpha, we generalize beta by introducing
// a let-expression, in order to make beta the same as alpha
// (modulo variable names).

pub fn loop_back(beta: &RcNode, alpha: &RcNode) {
    if let Some(subst) = match_against(&alpha.get_body(), &beta.get_body()) {
        let mut bindings = Vec::new();
        for (n, t) in subst.iter() {
            bindings.push((n.clone(), Rc::clone(t)));
        }
        let let_term = Term::mk_let(alpha.get_body(), bindings);
        replace_subtree(beta, &let_term);
    } else {
        unimplemented!()
    };
}

// Kinds of supercompilers

pub trait BuildStep {
    fn build_step(&self, d: &mut DrivingEngine, tree: &mut Tree, beta: &RcNode);
}

struct BasicBuildStep;

impl BuildStep for BasicBuildStep {
    fn build_step(
        &self,
        d: &mut DrivingEngine,
        tree: &mut Tree,
        beta: &RcNode,
    ) {
        if let Some(alpha) = find_more_general_ancestor(beta) {
            loop_back(beta, &alpha);
        } else {
            d.expand_node(tree, beta);
        }
    }
}

pub fn build_process_tree<T: BuildStep>(
    bs: T,
    ng: NameGen,
    k: i64,
    prog: Program,
    t: RcTerm,
) -> Tree {
    let mut d = DrivingEngine::new(ng, prog);
    let mut tree = Tree::new(&t);
    // Specifying k = -1 results in an unlimited building loop.
    let mut k = k;
    loop {
        if k == 0 {
            break;
        }
        k -= 1;
        if let Some(beta) = find_unprocessed_node(&tree) {
            bs.build_step(&mut d, &mut tree, &beta);
        } else {
            break;
        }
    }
    return tree;
}

// Basic supercompiler process tree builder

pub fn build_basic_process_tree(
    ng: NameGen,
    k: i64,
    prog: Program,
    t: RcTerm,
) -> Tree {
    build_process_tree(BasicBuildStep {}, ng, k, prog, t)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::*;

    fn dr_step0(prog: Program, t: RcTerm, expected: &str) {
        let mut d = DrivingEngine::new(NameGen::new("v", 100), prog);
        let branches = d.driving_step(&t);
        let actual = branches
            .iter()
            .map(|b| b.to_string())
            .collect::<Vec<String>>()
            .join("");
        assert_eq!(expected, actual);
    }

    fn dr_step(prog: &str, t: &str, expected: &str) {
        dr_step0(parse_program(prog), parse_term(t), expected);
    }

    // Testing basic process tree builder

    const P_ADD: &str = "gAdd(Z,y)=y;gAdd(S(x),y)=S(gAdd(x,y));";
    const P_ADD_ACC: &str = "gAddAcc(Z,y)=y;gAddAcc(S(x),y)=gAddAcc(x,S(y));";

    #[test]
    fn test_driving_step() {
        dr_step("", "C(a,b)", "(a,*)(b,*)");
        dr_step("f(x)=x;", "f(A(z))", "(A(z),*)");
        dr_step(P_ADD_ACC, "gAddAcc(S(S(Z)),Z)", "(gAddAcc(S(Z),S(Z)),*)");
        dr_step(
            P_ADD_ACC,
            "gAddAcc(a,b)",
            "(b,a=Z)(gAddAcc(v100,S(b)),a=S(v100))",
        );
        dr_step(
            P_ADD_ACC,
            "gAddAcc(gAddAcc(a,b),c)",
            "(gAddAcc(b,c),a=Z)(gAddAcc(gAddAcc(v100,S(b)),c),a=S(v100))",
        );
        dr_step0(
            Program::new(vec![]),
            Term::mk_let(
                Term::ctr("C", vec![Term::var("x"), Term::var("y")]),
                vec![
                    (String::from("x"), Term::var("a")),
                    (String::from("y"), Term::var("b")),
                ],
            ),
            "(C(x,y),*)(a,*)(b,*)",
        )
    }

    fn build_pr_tree(prog: Program, t: RcTerm, k: i64) -> Tree {
        let ng = NameGen::new("v", 100);
        build_basic_process_tree(ng, k, prog, t)
    }

    fn build_pr_tree_1_ok(prog: &str, t: &str, expected: &str) {
        let tree = build_pr_tree(parse_program(prog), parse_term(t), 1);
        assert_eq!(expected, tree.to_string());
    }

    fn build_pr_tree_ok(prog: &str, t: &str, expected: &str) {
        let tree = build_pr_tree(parse_program(prog), parse_term(t), 100);
        assert_eq!(expected, tree.to_string());
    }

    #[test]
    fn test_build_pr_tree_1() {
        build_pr_tree_1_ok("", "x", "{0:(x,,,[])}");
        build_pr_tree_1_ok("", "S(Z)", "{0:(S(Z),,,[1]),1:(Z,,0,[])}");
        build_pr_tree_1_ok(
            P_ADD_ACC,
            "gAddAcc(S(Z),Z)",
            "{0:(gAddAcc(S(Z),Z),,,[1]),1:(gAddAcc(Z,S(Z)),,0,[])}",
        );
        build_pr_tree_1_ok(P_ADD, "gAdd(a,b)",
            "{0:(gAdd(a,b),,,[1,2]),1:(b,a=Z,0,[]),2:(S(gAdd(v100,b)),a=S(v100),0,[])}");
        build_pr_tree_1_ok(
            P_ADD,
            "gAdd(gAdd(a,b),c)",
            "{\
        0:(gAdd(gAdd(a,b),c),,,[1,2]),\
        1:(gAdd(b,c),a=Z,0,[]),\
        2:(gAdd(S(gAdd(v100,b)),c),a=S(v100),0,[])}",
        )
    }

    #[test]
    fn test_build_pr_tree() {
        build_pr_tree_ok(P_ADD_ACC, "gAddAcc(a,b)", "{\
            0:(gAddAcc(a,b),,,[1,2]),\
            1:(b,a=Z,0,[]),2:(let a=v100,b=S(b) in gAddAcc(a,b),a=S(v100),0,[3,4,5]),\
            3:(gAddAcc(a,b),,2,[]),\
            4:(v100,,2,[]),\
            5:(S(b),,2,[6]),\
            6:(b,,5,[])}");
        build_pr_tree_ok(
            P_ADD_ACC,
            "gAddAcc(S(Z),Z)",
            "{\
            0:(gAddAcc(S(Z),Z),,,[1]),1:(gAddAcc(Z,S(Z)),,0,[2]),\
            2:(S(Z),,1,[3]),3:(Z,,2,[])}",
        );
        build_pr_tree_ok(
            P_ADD,
            "gAdd(a,b)",
            "{\
            0:(gAdd(a,b),,,[1,2]),1:(b,a=Z,0,[]),\
            2:(S(gAdd(v100,b)),a=S(v100),0,[3]),3:(gAdd(v100,b),,2,[])}",
        );
        build_pr_tree_ok(
            P_ADD,
            "gAdd(gAdd(a,b),c)",
            "{\
            0:(gAdd(gAdd(a,b),c),,,[1,2]),1:(gAdd(b,c),a=Z,0,[3,4]),\
            3:(c,b=Z,1,[]),4:(S(gAdd(v101,c)),b=S(v101),1,[5]),\
            5:(gAdd(v101,c),,4,[]),2:(gAdd(S(gAdd(v100,b)),c),a=S(v100),0,[6]),\
            6:(S(gAdd(gAdd(v100,b),c)),,2,[7]),7:(gAdd(gAdd(v100,b),c),,6,[])}",
        );
    }
}
