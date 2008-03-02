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
    """

    val program = programFromString(programText.stripMargin)
    val sc = new SuperCompiler(program)
    
    val initial1 = FCall("test1", Nil)
    val result1 = sc.superCompile(initial1)
    println("--------")
    println(initial1)
    println()
    println(result1)
    
    val initial2 = GCall("append", Variable("xs"), Variable("ys")::Nil)
    val result2 = sc.superCompile(initial2)
    println("--------")
    println(initial2)
    println()
    println(result2)
    
    val initial3 = GCall("append", GCall("append", Variable("xs"), Variable("ys")::Nil), Variable("zs")::Nil)
    val result3 = sc.superCompile(initial3)
    println("--------")
    println(initial3)
    println()
    println(result3)
    
    val initial4 = GCall("append", GCall("append", Variable("xs"), Variable("ys")::Nil), Variable("xs")::Nil)
    val result4 = sc.superCompile(initial4)
    println("--------")
    println(initial4)
    println()
    println(result4)
    
    println("--------")
    
  }

}
