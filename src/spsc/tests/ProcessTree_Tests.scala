package spsc.tests

import org.junit.Test
import org.junit.Before
import org.junit.Assert._

class ProcessTree_Tests {

  var tree : Tree = null
  
  @Before def setup() : Unit = {
    tree = Tree.create(Var("r"))
    val r = tree.root
    tree = tree.addChildren(r,List((Var("m1"), null), (Var("m2"), null)))
    val m1 = tree.children(r)(0)
    val m2 = tree.children(r)(1)
    tree = tree.addChildren(m1, List((Var("n"), null)))
    tree = tree.replace(m2, Var("x"))
  }

  @Test def test01PrTreeBuilding() = {
    assertEquals(
    "{,0:(r,,,[1,4]),1:(m1,,0,[3]),3:(n,,1,[]),4:(x,,0,[])}",
    tree.toString)
  }

  def test02PrTreeLeaves() : Unit =
    assertEquals(List(3,2), tree.leaves map {n => n.nodeId})
}
