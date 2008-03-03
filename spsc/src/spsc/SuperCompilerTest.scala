package spsc;

import org.junit.Test
import org.junit.Assert._
import SmallLanguage._
import TestUtils._

class SuperCompilerTest {
  
  def buildProcessTree(sc: SuperCompiler, initial: Expression) {
    val result1 = sc.buildProcessTree(initial)
    println("--------")
    println(initial)
    println()
    println(result1)
  }
  
  // currently only prints final process tree to console
  @Test def simpleProcessTree(): Unit =
  {
    val programText = 
    """
    |test1() = append(Nil, Nil);
    |test2() = append(Cons(A1, Cons(A2, Nil)), Cons(A3, Cons(A4, Nil)));
    |test3() = reverse(Cons(A1, Cons(A2, Cons(A3, Nil))));
    |append(Nil, vs) = vs;
    |append(Cons(u, us), vs) = Cons(u, append(us, vs));
    |reverse(Nil) = Nil;
    |reverse(Cons(x, xs)) = append(reverse(xs), Cons(x, Nil));
    |from(n) = Cons(n, from(S(n)));
    |take(Z, xs) = Nil;
    |take(S(n), xs) = Cons(head(xs), take(n, tail(xs)));
    |mapAdd1(Nil) = Nil;
    |mapAdd1(Cons(x, xs)) = Cons(S(x), mapAdd1(xs));
    |null(Nil) = True;
    |null(Cons(x, xs)) = False;
    |head(Cons(x, xs)) = x;
    |tail(Cons(x, xs)) = xs;
    |test4() = mapAdd1(from(Z));
    |test5(n, start) = take(n, mapAdd1(from(start)));
    |ab(A(x)) = B(ab(x));
    |ab(B(x)) = A(ab(x));
    |test6(x) = ab(ab(x));
    |reva(Nil, ys) = ys;
    |reva(Cons(x, xs), ys) = reva(xs, Cons(x, ys));
    |test7(xs) = reva(xs, Nil);
    |eq(Z, y) = eqZ(y);
    |eq(S(x), y) = eqS(y, x);
    |eqZ(Z) = True;
    |eqZ(S(x)) = False;
    |eqS(Z, x) = False;
    |eqS(S(y), x) = eq(x, y);
    |test8(x) = eq(x, x);
    |test9(x) = eq(x, S(x));
    |test10(x) = eq(S(x), x);
    |test11(x) = eq(S(Z), x);
    |if(True, x, y) = x;
    |if(False, x, y) = y;
    |not(x) = if(x, False, True);
    |or(x, y) = if(x, True, y);
    |and(x, y) = if(x, y, False);
    |test12(x, y) = not(or(not(x), not(y)));
    |member(x, list) = and(not(null(list)), or(eq(x, head(list)), member(x, tail(list))));
    |test13(x, list) = member(x, list);
    |test14(list) = member(S(Z), list);
    |test15(x) = member(x, Cons(Z, Cons(S(Z), Nil)));
    |member2(Nil, x) = False;
    |member2(Cons(y, ys), x) = if(eq(x, y), True, member2(ys, x));
    |test16(x, list) = member2(list, x);
    |test17(list) = member2(list, S(Z));
    |test18(x) = member2(Cons(Z, Cons(S(Z), Nil)), x);
    |member3(Nil, x) = False;
    |member3(Cons(y, ys), x) = member3if(eq(x, y), x, ys);
    |member3if(True, x, ys) = True;
    |member3if(False, x, ys) = member3(ys, x);
    |test19(x, list) = member3(list, x);
    |test20(list) = member3(list, S(Z));
    |test21(x) = member3(Cons(Z, Cons(S(Z), Nil)), x);
    """

    val program = programFromString(programText.stripMargin)
    val sc = new SuperCompiler(program)

    buildProcessTree(sc,
        FCall("test1", Nil)) 
    
    buildProcessTree(sc,
        GCall("append", Variable("xs"), Variable("ys")::Nil))
    
    buildProcessTree(sc,
        GCall("append", GCall("append", Variable("xs"), Variable("ys")::Nil), Variable("zs")::Nil))
    
    buildProcessTree(sc,
        GCall("append", GCall("append", Variable("xs"), Variable("ys")::Nil), Variable("xs")::Nil))
        
    buildProcessTree(sc,
        FCall("test4", Nil))
        
    buildProcessTree(sc,
        FCall("test5", Variable("n")::Variable("start")::Nil))
        
    buildProcessTree(sc,
        FCall("test6", Variable("x")::Nil))

    buildProcessTree(sc,
        FCall("test7", Variable("xs")::Nil))

    buildProcessTree(sc,
        FCall("test8", Variable("x")::Nil))
            
    buildProcessTree(sc,
        FCall("test9", Variable("x")::Nil))

    buildProcessTree(sc,
        FCall("test10", Variable("x")::Nil))

    buildProcessTree(sc,
        FCall("test11", Variable("x")::Nil))
        
    buildProcessTree(sc,
        FCall("test12", Variable("x")::Variable("y")::Nil))

    buildProcessTree(sc,
        FCall("test13", Variable("x")::Variable("list")::Nil))

    buildProcessTree(sc,
        FCall("test14", Variable("list")::Nil))

    buildProcessTree(sc,
        FCall("test15", Variable("x")::Nil))

    buildProcessTree(sc,
        FCall("test16", Variable("x")::Variable("list")::Nil))

    buildProcessTree(sc,
        FCall("test17", Variable("list")::Nil))

    buildProcessTree(sc,
        FCall("test18", Variable("x")::Nil))

    buildProcessTree(sc,
        FCall("test19", Variable("x")::Variable("list")::Nil))

    buildProcessTree(sc,
        FCall("test20", Variable("list")::Nil))

    buildProcessTree(sc,
        FCall("test21", Variable("x")::Nil))
  }
}
