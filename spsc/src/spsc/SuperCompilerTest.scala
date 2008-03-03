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
    |testXXX(x, list) = member(x, list);
    |test13(x, list) = if(True, test13(x, list), True);
    """
/*  
    """
     */

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
  }
}
