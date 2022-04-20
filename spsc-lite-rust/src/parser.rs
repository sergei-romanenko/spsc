use crate::language::{FRule, GRule, Program, RcTerm, Rule, Term};

use std::convert::identity;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alphanumeric0, char, satisfy},
    combinator::{all_consuming, map, opt, recognize},
    multi::{many0, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, terminated},
    IResult,
};

fn cond_id<F>(cond: F, i: &str) -> IResult<&str, &str>
where
    F: Fn(char) -> bool,
{
    recognize(pair(satisfy(cond), alphanumeric0))(i)
}

fn u_id(i: &str) -> IResult<&str, &str> {
    cond_id(|c| (c.is_ascii_uppercase() && c.is_ascii_alphabetic()), i)
}

fn l_id(i: &str) -> IResult<&str, &str> {
    cond_id(|c| c.is_ascii_lowercase() && c.is_ascii_alphabetic(), i)
}

fn f_id(i: &str) -> IResult<&str, &str> {
    cond_id(|c| c == 'f', i)
}

fn g_id(i: &str) -> IResult<&str, &str> {
    cond_id(|c| c == 'g', i)
}

fn vrb(i: &str) -> IResult<&str, RcTerm> {
    map(l_id, Term::var)(i)
}

fn term(i: &str) -> IResult<&str, RcTerm> {
    alt((ctr, fcall, gcall, vrb))(i)
}

fn ctr(i: &str) -> IResult<&str, RcTerm> {
    let (i, name) = u_id(i)?;
    let (i, opt_args) = opt(delimited(
        char('('),
        separated_list0(char(','), term),
        char(')'),
    ))(i)?;
    let args = opt_args.map_or(Vec::new(), identity);
    Ok((i, Term::ctr(name, args)))
}

fn fcall(i: &str) -> IResult<&str, RcTerm> {
    let (i, name) = f_id(i)?;
    let (i, args) =
        delimited(char('('), separated_list0(char(','), term), char(')'))(i)?;
    Ok((i, Term::fcall(name, args)))
}

fn gcall(i: &str) -> IResult<&str, RcTerm> {
    let (i, name) = g_id(i)?;
    let (i, args) =
        delimited(char('('), separated_list1(char(','), term), char(')'))(i)?;
    Ok((i, Term::gcall(name, args)))
}

fn pat(i: &str) -> IResult<&str, (&str, Vec<&str>)> {
    let (i, cname) = u_id(i)?;
    let (i, opt_cparams) = opt(delimited(
        char('('),
        separated_list0(char(','), l_id),
        char(')'),
    ))(i)?;
    let cparams = opt_cparams.map_or(Vec::new(), identity);
    Ok((i, (cname, cparams)))
}

fn frule(i: &str) -> IResult<&str, Rule> {
    let (i, name) = terminated(f_id, char('('))(i)?;
    let (i, params) = separated_list0(char(','), l_id)(i)?;
    let (i, body) = delimited(tag(")="), term, char(';'))(i)?;
    Ok((i, FRule::rule(name, &params, body)))
}

fn grule(i: &str) -> IResult<&str, Rule> {
    let (i, name) = terminated(g_id, char('('))(i)?;
    let (i, (cname, cparams)) = pat(i)?;
    let (i, params) = many0(preceded(char(','), l_id))(i)?;
    let (i, body) = delimited(tag(")="), term, char(';'))(i)?;
    Ok((i, GRule::rule(name, cname, &cparams, &params, body)))
}

fn rule(i: &str) -> IResult<&str, Rule> {
    alt((frule, grule))(i)
}

fn program(i: &str) -> IResult<&str, Program> {
    map(many0(rule), |rules| Program::new(rules))(i)
}

pub fn parse_term(i: &str) -> RcTerm {
    match all_consuming(term)(i) {
        Ok((_, t)) => t,
        Err(err) => panic!("{}", err),
    }
}

pub fn parse_program(i: &str) -> Program {
    match all_consuming(program)(i) {
        Ok((_, p)) => p,
        Err(err) => panic!("{}", err),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_ok<'a, P, O>(parser: P, expected: &'a str, i: &'a str)
    where
        P: FnMut(&str) -> IResult<&str, O>,
        O: std::fmt::Display,
        O: std::fmt::Debug,
    {
        assert_matches::assert_matches! (
            all_consuming(parser)(i),
                Ok((_, actual)) =>
                    {assert_eq!(expected, actual.to_string());});
    }

    fn parse_err<'a, P, O>(parser: P, i: &'a str)
    where
        P: FnMut(&'a str) -> IResult<&'a str, O>,
        O: std::fmt::Display,
        O: std::fmt::Debug,
    {
        assert!(all_consuming(parser)(i).is_err());
    }

    fn term_ok(expected: &str, i: &str) {
        parse_ok(term, expected, i);
    }

    fn pat_ok(expected: &str, i: &str) {
        assert_matches::assert_matches! (
            all_consuming(pat)(i),
                Ok((_, (cname, cparams))) =>
                    {assert_eq!(expected, crate::language::pat_to_string(cname, &cparams));});
    }

    fn frule_ok(expected: &str, i: &str) {
        parse_ok(frule, expected, i);
    }

    fn grule_ok(expected: &str, i: &str) {
        parse_ok(grule, expected, i);
    }

    fn program_ok(expected: &str, i: &str) {
        parse_ok(program, expected, i);
    }

    #[test]
    fn parse_ident() {
        assert_eq!(u_id("A1###"), Ok(("###", "A1")));
        assert_eq!(l_id("a1###"), Ok(("###", "a1")));
        assert_eq!(f_id("fA1###"), Ok(("###", "fA1")));
        assert_eq!(g_id("gA1###"), Ok(("###", "gA1")));

        assert_eq!(vrb("x1###"), Ok(("###", Term::var("x1"))));

        parse_err(l_id, "Aa");
        parse_err(u_id, "aa");
        parse_err(l_id, "A*a");
    }

    #[test]
    fn parse_term() {
        assert_eq!(vrb("x1###"), Ok(("###", Term::var("x1"))));
        term_ok("x1", "x1");
        term_ok("fA", "fA");
        term_ok("gA", "gA");
        term_ok("A(x,y)", "A(x,y)");
        term_ok("A(x)", "A(x)");
        term_ok("A", "A()");
        term_ok("A", "A");
        term_ok("fA(x,y)", "fA(x,y)");
        term_ok("fA(x)", "fA(x)");
        term_ok("fA()", "fA()");
        term_ok("gA(x,y)", "gA(x,y)");
        parse_err(term, "gA()");
    }

    #[test]
    fn parse_pat() {
        pat_ok("C", "C()");
        pat_ok("C", "C");
        pat_ok("C(x)", "C(x)");
        pat_ok("C(x,y)", "C(x,y)");
    }

    #[test]
    fn parse_frule() {
        frule_ok("f(x,y)=x;", "f(x,y)=x;");
        frule_ok("f(x)=x;", "f(x)=x;");
        frule_ok("f()=C;", "f()=C;");
    }

    #[test]
    fn parse_grule() {
        grule_ok("g(C(x),y)=x;", "g(C(x),y)=x;");
        grule_ok("g(C,y)=x;", "g(C,y)=x;");
        grule_ok("g(C)=C;", "g(C)=C;");
        parse_err(grule, "g()");
    }

    #[test]
    fn parse_program() {
        program_ok("", "");
        program_ok("f(x)=A;", "f(x)=A;");
        program_ok("f(x)=A;f1(x)=A1;", "f(x)=A;f1(x)=A1;");
        program_ok("f()=f();", "f()=f();");
        program_ok("g(C)=A;", "g(C)=A;");
        program_ok("g(C(x))=A;", "g(C(x))=A;");
        program_ok("g(C(x))=A;", "g(C(x))=A;");
        program_ok("g(C(x,y),z)=A;", "g(C(x,y),z)=A;");
        program_ok("g(C,y)=A;", "g(C,y)=A;");

        parse_err(program, "g(C,y)=A;###");
    }
}
