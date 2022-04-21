use crate::algebra::*;
use crate::language::*;
use crate::process_tree::*;

use either::*;
use std::collections::BTreeMap;
use std::rc::Rc;

type Sig = (Name, Vec<Name>);
type Sigs = BTreeMap<NodeId, Sig>;

struct ResPrGen {
    tree: Tree,
    sigs: Sigs,
    rules: Vec<Rule>,
}

impl ResPrGen {
    fn new(tree: Tree) -> ResPrGen {
        ResPrGen {
            tree: tree,
            sigs: Sigs::new(),
            rules: Vec::new(),
        }
    }
}

pub fn gen_residual_program(tree: &Tree) -> (Program, RcTerm) {
    let mut g = ResPrGen::new(tree.clone());
    let res_term = gen_term(&mut g, &tree.root);
    return (Program::new(g.rules), res_term);
}

fn gen_term(g: &mut ResPrGen, beta: &RcNode) -> RcTerm {
    if let Some(alpha) = func_ancestor(&beta) {
        let (name, params) = g.sigs[&alpha.get_node_id()].clone();
        let args = params.iter().map(|param| Term::var(param)).collect();
        let subst = match_against(&alpha.get_body(), &beta.get_body()).unwrap();
        if let Some(_) = get_child(&alpha, 0).get_contr() {
            return apply_subst(&subst, &Term::gcall(&name, args));
        } else {
            return apply_subst(&subst, &Term::fcall(&name, args));
        }
    } else {
        gen_term_beta(g, beta)
    }
}

fn gen_term_beta(g: &mut ResPrGen, beta: &RcNode) -> RcTerm {
    let body = beta.get_body();
    match &*body {
        Term::Var { .. } => body,
        Term::CFG { kind, name, .. } if *kind == TKind::Ctr => {
            let res_terms =
                beta.get_children().iter().map(|n| gen_term(g, n)).collect();
            Term::ctr(name, res_terms)
        }
        Term::CFG { name, .. } => gen_call(g, beta, &body, name),
        Term::Let { bindings, .. } => {
            let children: Vec<RcNode> = beta.get_children();
            let res_body = gen_term(g, &children[0]);
            let mut subst = Subst::new();
            for k in 0..bindings.len() {
                subst.insert(
                    bindings[k].0.clone(),
                    gen_term(g, &children[k + 1]),
                );
            }
            apply_subst(&subst, &res_body)
        }
    }
}

fn get_fg_sig(
    g: &mut ResPrGen,
    prefix: &str,
    beta: &RcNode,
    name: &str,
    vs: Params,
) -> Sig {
    let node_id = beta.get_node_id();
    if let Some(sig) = g.sigs.get(&node_id) {
        sig.clone()
    } else {
        let name_str = String::from(name);
        let name1 = format!(
            "{}{}{}",
            prefix,
            name_str.get(1..).unwrap(),
            g.sigs.len() + 1
        );
        let sig1 = (name1, vs);
        g.sigs.insert(node_id, sig1.clone());
        sig1
    }
}

fn gen_call(g: &mut ResPrGen, beta: &RcNode, t: &RcTerm, name: &str) -> RcTerm {
    let params: Params = vars(t);
    let params1: Params = Vec::from(params.get(1..).unwrap());
    let var_params = params.iter().map(|param| Term::var(param)).collect();
    if is_var_test(&beta) {
        let (name1, _) = get_fg_sig(g, "g", &beta, name, params);
        let rules: Vec<Rule> = beta
            .get_children()
            .iter()
            .map(|n| {
                let contr = n.get_contr().unwrap();
                Right(GRule {
                    name: name1.clone(),
                    cname: contr.cname.clone(),
                    cparams: contr.cparams.clone(),
                    params: params1.clone(),
                    body: gen_term(g, n),
                })
            })
            .collect();
        g.rules.extend(rules);
        return Term::gcall(&name1, var_params);
    } else if is_func_node(&g.tree, &beta) {
        let (name1, _) = get_fg_sig(g, "f", &beta, name, params);
        let body1 = gen_term(g, &beta.get_children()[0]);
        let rule = Left(FRule {
            name: name1.clone(),
            params: params1,
            body: body1,
        });
        g.rules.push(rule);
        return Term::fcall(&name1, var_params);
    } else {
        return gen_term(g, &beta.get_children()[0]);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::advanced_process_tree_builder::*;
    use crate::parser::*;
    use crate::process_tree_builder::*;

    // Sample programs

    const P_ADD: &str = "gAdd(Z,y)=y;gAdd(S(x),y)=S(gAdd(x,y));";
    const P_ADD_ACC: &str = "gAddAcc(Z,y)=y;gAddAcc(S(x),y)=gAddAcc(x,S(y));";

    //---- Basic supercompiler

    fn run_basic_scp(prog: Program, t: RcTerm) -> (Program, RcTerm) {
        let ng = NameGen::new("v", 100);
        let tree = build_basic_process_tree(ng, 100, prog, t);
        return gen_residual_program(&tree);
    }

    fn basic_scp_ok(prog: &str, t: &str, expected: &str) {
        let (res_pr, res_term) =
            run_basic_scp(parse_program(prog), parse_term(t));
        let res_s = format!("{}/{}", res_pr, res_term);
        assert_eq!(expected, res_s);
    }

    #[test]
    fn test_basic_scp() {
        basic_scp_ok("", "a", "/a");
        basic_scp_ok("", "C(a,b)", "/C(a,b)");
        basic_scp_ok(
            P_ADD,
            "gAdd(a,b)",
            "gAdd1(Z,b)=b;gAdd1(S(v100),b)=S(gAdd1(v100,b));/gAdd1(a,b)",
        );
        basic_scp_ok(P_ADD, "gAdd(gAdd(a,b),c)", "\
            gAdd2(Z,c)=c;gAdd2(S(v101),c)=S(gAdd2(v101,c));\
            gAdd1(Z,b,c)=gAdd2(b,c);gAdd1(S(v100),b,c)=S(gAdd1(v100,b,c));/gAdd1(a,b,c)");
        basic_scp_ok(P_ADD_ACC, "gAddAcc(a,b)",
            "gAddAcc1(Z,b)=b;gAddAcc1(S(v100),b)=gAddAcc1(v100,S(b));/gAddAcc1(a,b)");
    }

    //---- Advanced supercompiler

    fn run_advanced_scp(prog: Program, t: RcTerm) -> (Program, RcTerm) {
        let ng = NameGen::new("v", 100);
        let tree = build_advanced_process_tree(ng, 100, prog, t);
        return gen_residual_program(&tree);
    }

    fn advanced_scp_ok(prog: &str, t: &str, expected: &str) {
        let (res_pr, res_term) =
            run_advanced_scp(parse_program(prog), parse_term(t));
        let res_s = format!("{}/{}", res_pr, res_term);
        assert_eq!(expected, res_s);
    }

    #[test]
    fn test_advanced_scp() {
        advanced_scp_ok(
            P_ADD,
            "gAdd(a,b)",
            "gAdd1(Z,b)=b;gAdd1(S(v100),b)=S(gAdd1(v100,b));/gAdd1(a,b)",
        );
        advanced_scp_ok(P_ADD, "gAdd(a,a)",
            "gAdd1(Z,v103)=v103;gAdd1(S(v104),v103)=S(gAdd1(v104,v103));/gAdd1(a,a)");
        advanced_scp_ok(P_ADD, "gAdd(gAdd(a,b),c)", "\
            gAdd2(Z,c)=c;gAdd2(S(v101),c)=S(gAdd2(v101,c));\
            gAdd1(Z,b,c)=gAdd2(b,c);gAdd1(S(v100),b,c)=S(gAdd1(v100,b,c));/gAdd1(a,b,c)");
        advanced_scp_ok(P_ADD_ACC, "gAddAcc(a,b)",
            "gAddAcc1(Z,b)=b;gAddAcc1(S(v100),b)=gAddAcc1(v100,S(b));/gAddAcc1(a,b)");
        advanced_scp_ok(P_ADD_ACC, "gAddAcc(a,a)",
            "gAddAcc1(Z,v103)=v103;gAddAcc1(S(v104),v103)=gAddAcc1(v104,S(v103));/gAddAcc1(a,a)");
        advanced_scp_ok(P_ADD_ACC, "gAddAcc(gAddAcc(a,b),c)", "\
            gAddAcc2(Z,c)=c;gAddAcc2(S(v101),c)=gAddAcc2(v101,S(c));\
            gAddAcc1(Z,b,c)=gAddAcc2(b,c);gAddAcc1(S(v100),b,c)=gAddAcc1(v100,S(b),c);\
            /gAddAcc1(a,b,c)");
    }
}
