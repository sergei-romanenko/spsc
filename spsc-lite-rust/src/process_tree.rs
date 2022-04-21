use crate::algebra::*;
use crate::language::*;

use std::cell::RefCell;
use std::fmt;
use std::rc::{Rc, Weak};

pub struct Contraction {
    pub vname: Name,
    pub cname: Name,
    pub cparams: Params,
}

pub type RcContraction = Rc<Contraction>;

impl fmt::Display for Contraction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let () = write!(f, "{}={}", self.vname, self.cname)?;
        if self.cparams.is_empty() {
            Ok(())
        } else {
            write!(f, "({})", self.cparams.join(","))
        }
    }
}

pub type NodeId = i64;
pub type RcNode = Rc<Node>;

pub struct Node {
    // The constructor is supposed to be called via Tree::new_node only.
    // nodeId is only used for unit testing purposes.
    node_id: NodeId,
    body: RefCell<RcTerm>,
    contr: Option<RcContraction>,
    parent: RefCell<Weak<Node>>,
    children: RefCell<Vec<RcNode>>,
}

impl Node {
    pub fn get_node_id(&self) -> NodeId {
        self.node_id
    }

    pub fn get_body(&self) -> RcTerm {
        Rc::clone(&self.body.borrow())
    }

    pub fn get_contr(&self) -> Option<RcContraction> {
        self.contr.clone()
    }

    pub fn get_children(&self) -> Vec<RcNode> {
        self.children.borrow().clone()
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:({},", self.node_id, self.body.borrow())?;
        if let Some(contr) = &self.contr {
            write!(f, "{}", contr)?;
        }
        write!(f, ",")?;
        if let Some(parent) = &self.parent.borrow().upgrade() {
            write!(f, "{}", parent.node_id)?;
        }
        write!(f, ",[")?;
        let children: Vec<String> = self
            .children
            .borrow()
            .iter()
            .map(|child| child.node_id.to_string())
            .collect();
        write!(f, "{}])", children.join(","))?;
        Ok(())
    }
}

pub struct Ancestors {
    current: RcNode,
}

impl Ancestors {
    pub fn new(node: &RcNode) -> Ancestors {
        Ancestors {
            current: Rc::clone(node),
        }
    }
}

impl Iterator for Ancestors {
    type Item = RcNode;

    fn next(&mut self) -> Option<Self::Item> {
        let opt_parent = self.current.parent.borrow().upgrade();
        match opt_parent {
            None => None,
            Some(parent) => {
                self.current = Rc::clone(&parent);
                Some(Rc::clone(&parent))
            }
        }
    }
}

pub fn func_ancestor(n: &RcNode) -> Option<RcNode> {
    for a in Ancestors::new(n) {
        if equiv(&n.body.borrow(), &a.body.borrow()) {
            return Some(a);
        }
    }
    return None;
}

pub fn find_more_general_ancestor(n: &RcNode) -> Option<RcNode> {
    for a in Ancestors::new(n) {
        if a.body.borrow().is_fg_call()
            && inst_of(&n.body.borrow(), &a.body.borrow())
        {
            return Some(a);
        }
    }
    return None;
}

fn is_processed(n: &RcNode) -> bool {
    let body = &**n.body.borrow();
    match body {
        Term::Var { .. } => true,
        Term::CFG { args, .. } if body.is_ctr() => args.is_empty(),
        Term::CFG { .. } => func_ancestor(n).is_some(),
        Term::Let { .. } => false,
    }
}

pub fn is_var_test(n: &RcNode) -> bool {
    let children = n.children.borrow();
    !children.is_empty() && children[0].contr.is_some()
}

pub fn get_child(n: &RcNode, i: usize) -> RcNode {
    Rc::clone(&n.children.borrow()[i])
}

struct SubtreeNodes {
    node: RcNode,
    start: bool,
    path: Vec<usize>,
}

impl SubtreeNodes {
    fn new(node: &RcNode) -> SubtreeNodes {
        SubtreeNodes {
            node: Rc::clone(node),
            start: true,
            path: vec![0],
        }
    }
}

impl Iterator for SubtreeNodes {
    type Item = RcNode;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.start {
                self.start = false;
                return Some(Rc::clone(&self.node));
            }
            match self.path.pop() {
                None => return None,
                Some(i) => {
                    if i == self.node.children.borrow().len() {
                        let opt_parent = self.node.parent.borrow().upgrade();
                        match opt_parent {
                            None => return None,
                            Some(parent) => self.node = Rc::clone(&parent),
                        }
                    } else {
                        let child = Rc::clone(&self.node.children.borrow()[i]);
                        self.node = child;
                        self.start = true;
                        self.path.push(i + 1);
                        self.path.push(0);
                    }
                }
            }
        }
    }
}

fn is_leaf(n: &RcNode) -> bool {
    n.children.borrow().is_empty()
}

fn subtree_leaves(n: &RcNode) -> Box<dyn Iterator<Item = RcNode>> {
    Box::new(SubtreeNodes::new(n).filter(|r| is_leaf(r)))
}

#[derive(Clone)]
pub struct Tree {
    fresh_node_id: RefCell<NodeId>,
    // By convention, the root node's id is 0.
    pub root: RcNode,
}

impl Tree {
    pub fn new(t: &RcTerm) -> Tree {
        Tree {
            fresh_node_id: RefCell::new(0),
            root: Rc::new(Node {
                node_id: 0,
                body: RefCell::new(Rc::clone(t)),
                contr: None,
                parent: RefCell::new(Weak::new()),
                children: RefCell::new(Vec::new()),
            }),
        }
    }
}

fn nodes(tree: &Tree) -> Box<dyn Iterator<Item = RcNode>> {
    Box::new(SubtreeNodes::new(&tree.root))
}

impl fmt::Display for Tree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let nodes: Vec<String> = nodes(self).map(|n| n.to_string()).collect();
        write!(f, "{{")?;
        write!(f, "{}", nodes.join(","))?;
        write!(f, "}}")?;
        Ok(())
    }
}

fn leaves(tree: &Tree) -> Box<dyn Iterator<Item = RcNode>> {
    subtree_leaves(&tree.root)
}

pub fn find_unprocessed_node(tree: &Tree) -> Option<RcNode> {
    for n in leaves(tree) {
        if !is_processed(&n) {
            return Some(n);
        }
    }
    return None;
}

pub fn is_func_node(tree: &Tree, node: &RcNode) -> bool {
    for leaf in leaves(tree) {
        match func_ancestor(&leaf) {
            Option::None => (),
            Option::Some(a) => {
                if node.node_id == a.node_id {
                    return true;
                }
            }
        }
    }
    return false;
}

fn new_node(
    tree: &mut Tree,
    t: RcTerm,
    contr: Option<RcContraction>,
    parent: Option<RcNode>,
    children: Vec<RcNode>,
) -> RcNode {
    *tree.fresh_node_id.borrow_mut() += 1;
    let r = Rc::new(Node {
        node_id: *tree.fresh_node_id.borrow(),
        body: RefCell::new(t),
        contr: contr,
        parent: RefCell::new(Weak::new()),
        children: RefCell::new(children),
    });
    if let Some(p) = parent {
        *r.parent.borrow_mut() = Rc::downgrade(&p);
    }
    return r;
}

pub struct Branch {
    pub term: RcTerm,
    pub contr: Option<RcContraction>,
}

impl fmt::Display for Branch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;
        write!(f, "{}", self.term)?;
        write!(f, ",")?;
        if let Some(contr) = &self.contr {
            write!(f, "{}", contr)?;
        } else {
            write!(f, "*")?;
        }
        write!(f, ")")?;
        Ok(())
    }
}

pub fn add_children(tree: &mut Tree, node: &RcNode, branches: Vec<Branch>) {
    let children = branches.iter().map(|b| {
        new_node(
            tree,
            Rc::clone(&b.term),
            b.contr.clone(),
            Some(Rc::clone(&node)),
            Vec::new(),
        )
    });
    // node.children.borrow_mut().clear();
    node.children.borrow_mut().extend(children);
}

pub fn replace_subtree(node: &RcNode, body: &RcTerm) {
    node.children.borrow_mut().clear();
    *node.body.borrow_mut() = Rc::clone(body);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tree_test() {
        let mut tree = Tree::new(&Term::var("r"));
        assert_eq!(format!("{}", tree), "{0:(r,,,[])}");

        let b1 = Branch {
            term: Term::var("m1"),
            contr: None,
        };
        let b2 = Branch {
            term: Term::var("m2"),
            contr: None,
        };
        let root = Rc::clone(&tree.root);

        add_children(&mut tree, &root, vec![b1, b2]);
        assert_eq!(
            format!("{}", tree),
            "{0:(r,,,[1,2]),1:(m1,,0,[]),2:(m2,,0,[])}"
        );

        let m1 = &root.children.borrow()[0];
        let m2 = &root.children.borrow()[1];

        add_children(
            &mut tree,
            m1,
            vec![Branch {
                term: Term::var("n"),
                contr: None,
            }],
        );
        assert_eq!(
            format!("{}", tree),
            "{0:(r,,,[1,2]),1:(m1,,0,[3]),3:(n,,1,[]),2:(m2,,0,[])}"
        );

        replace_subtree(m2, &Term::var("x"));
        assert_eq!(
            format!("{}", tree),
            "{0:(r,,,[1,2]),1:(m1,,0,[3]),3:(n,,1,[]),2:(x,,0,[])}"
        );

        assert_eq!(
            nodes(&tree).map(|n| n.node_id).collect::<Vec<_>>(),
            [0, 1, 3, 2]
        );

        assert_eq!(
            leaves(&tree).map(|n| n.node_id).collect::<Vec<_>>(),
            [3, 2]
        );
    }
}
