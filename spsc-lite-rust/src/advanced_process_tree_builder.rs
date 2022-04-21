use crate::algebra::*;
use crate::he::*;
use crate::language::*;
use crate::msg::*;
use crate::process_tree::*;
use crate::process_tree_builder::*;

use std::rc::Rc;

// Advanced Supercompiler with homeomorphic embedding and generalization

struct AdvancedBuildStep;

fn abstract_node(alpha: &RcNode, t: &RcTerm, subst: Subst) {
    let bindings = subst
        .iter()
        .map(|(n, t)| (n.clone(), Rc::clone(t)))
        .collect();
    let let_term = Term::mk_let(Rc::clone(t), bindings);
    replace_subtree(alpha, &let_term);
}

fn split_node(ng: &mut NameGen, beta: &RcNode) {
    let t = beta.get_body();
    match &*t {
        Term::CFG { kind, name, args } => {
            let names1 = ng.fresh_name_list(args.len());
            let args1 = names1.iter().map(|x| Term::var(x)).collect();
            let t1 = Term::mk_cfg(kind.clone(), name, args1);
            let bs1 = names1
                .iter()
                .zip(args)
                .map(|(n, t)| (n.clone(), Rc::clone(t)))
                .collect();
            let let_term = Term::mk_let(t1, bs1);
            replace_subtree(beta, &let_term);
        }
        _ => unimplemented!(),
    }
}

fn generalize_alpha_or_split(ng: &mut NameGen, beta: &RcNode, alpha: &RcNode) {
    let g = msg(ng, &alpha.get_body(), &beta.get_body());
    if g.t.is_var() {
        split_node(ng, beta);
    } else {
        abstract_node(alpha, &g.t, g.s1);
    }
}

fn find_embedded_ancestor(beta: &RcNode) -> Option<RcNode> {
    for alpha in Ancestors::new(beta) {
        if alpha.get_body().is_fg_call()
            && embedded_in(&alpha.get_body(), &beta.get_body())
        {
            return Some(alpha);
        }
    }
    return None;
}

impl BuildStep for AdvancedBuildStep {
    fn build_step(
        &self,
        d: &mut DrivingEngine,
        tree: &mut Tree,
        beta: &RcNode,
    ) {
        if let Some(alpha) = find_more_general_ancestor(beta) {
            loop_back(beta, &alpha);
        } else {
            if let Some(alpha) = find_embedded_ancestor(beta) {
                generalize_alpha_or_split(&mut d.name_gen, beta, &alpha)
            } else {
                d.expand_node(tree, beta);
            }
        }
    }
}

pub fn build_advanced_process_tree(
    ng: NameGen,
    k: i64,
    prog: Program,
    t: RcTerm,
) -> Tree {
    build_process_tree(AdvancedBuildStep {}, ng, k, prog, t)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::*;

    fn build_pr_tree(prog: Program, t: RcTerm, k: i64) -> Tree {
        let ng = NameGen::new("v", 100);
        build_advanced_process_tree(ng, k, prog, t)
    }

    fn build_pr_tree_1_ok(prog: &str, t: &str, expected: &str) {
        let tree = build_pr_tree(parse_program(prog), parse_term(t), 1);
        assert_eq!(expected, tree.to_string());
    }

    fn build_pr_tree_ok(prog: &str, t: &str, expected: &str) {
        let tree = build_pr_tree(parse_program(prog), parse_term(t), 100);
        assert_eq!(expected, tree.to_string());
    }

    const P_ADD: &str = "gAdd(Z,y)=y;gAdd(S(x),y)=S(gAdd(x,y));";
    const P_ADD_ACC: &str = "gAddAcc(Z,y)=y;gAddAcc(S(x),y)=gAddAcc(x,S(y));";

    #[test]
    fn test_build_pr_tree_1() {
        build_pr_tree_1_ok("", "x", "{0:(x,,,[])}");
        build_pr_tree_1_ok(
            "",
            "S(Z)",
            "{\
            0:(S(Z),,,[1]),1:(Z,,0,[])}",
        );
        build_pr_tree_1_ok(
            P_ADD_ACC,
            "gAddAcc(S(Z),Z)",
            "{\
            0:(gAddAcc(S(Z),Z),,,[1]),1:(gAddAcc(Z,S(Z)),,0,[])}",
        );
        build_pr_tree_1_ok(
            P_ADD,
            "gAdd(a,b)",
            "{\
            0:(gAdd(a,b),,,[1,2]),1:(b,a=Z,0,[]),\
            2:(S(gAdd(v100,b)),a=S(v100),0,[])}",
        );
        build_pr_tree_1_ok(
            P_ADD,
            "gAdd(gAdd(a,b),c)",
            "{\
            0:(gAdd(gAdd(a,b),c),,,[1,2]),1:(gAdd(b,c),a=Z,0,[]),\
            2:(gAdd(S(gAdd(v100,b)),c),a=S(v100),0,[])}",
        );
    }

    #[test]
    fn test_build_pr_tree() {
        build_pr_tree_ok(
            P_ADD_ACC,
            "gAddAcc(a,b)",
            "{\
            0:(gAddAcc(a,b),,,[1,2]),1:(b,a=Z,0,[]),\
            2:(let a=v100,b=S(b) in gAddAcc(a,b),a=S(v100),0,[3,4,5]),\
            3:(gAddAcc(a,b),,2,[]),4:(v100,,2,[]),\
            5:(S(b),,2,[6]),6:(b,,5,[])}",
        );
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
        build_pr_tree_ok(P_ADD, "gAdd(a,a)", "{\
            0:(let v102=a,v103=a in gAdd(v102,v103),,,[4,5,6]),\
            4:(gAdd(v102,v103),,0,[7,8]),7:(v103,v102=Z,4,[]),\
            8:(S(gAdd(v104,v103)),v102=S(v104),4,[9]),9:(gAdd(v104,v103),,8,[]),\
            5:(a,,0,[]),6:(a,,0,[])}");
    }
}
