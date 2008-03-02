package spsc;

import org.junit.Test
import org.junit.Assert._
import SmallLanguage._
import TestUtils._
import Tree._

class SuperCompilerTest {
  @Test def simpleDrive(): Unit =
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
    """

    val program = programFromString(programText.stripMargin)
    val sc = new SuperCompiler(program)
    val initialNode = Node(FCall("test1", Nil), null, Nil)
    sc.driveNode(initialNode)
    println(initialNode)
    
    val expected = termFromString("Nil")
    assertEquals(1, initialNode.outs.size)
    
    val childNode1 = initialNode.outs.head.child
    assertEquals(1, childNode1.outs.size)
    
    val childNode2 = childNode1.outs.head.child
    assertEquals(0, childNode2.outs.size)
    
    assertEquals(expected, childNode2.expr)
  }
  
  def buildProcessTree(sc: SuperCompiler, initial: Expression) {
    val result1 = sc.buildProcessTree(initial)
    println("--------")
    println(initial)
    println()
    println(result1)
  }
  
  // currently only prints final process tree to console
  @Test def simpleDrive1(): Unit =
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
  }
}
