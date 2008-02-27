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

}
