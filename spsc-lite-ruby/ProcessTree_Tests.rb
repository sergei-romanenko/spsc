require 'test/unit'
require 'SLL'
require 'ProcessTree'

class ProcessTree_Tests < Test::Unit::TestCase
  include SLL
  include ProcessTree
  def setup()
    t = Tree.new(Var.new("r"))
    r = t.root
    t.addChildren(r,[[Var.new("m1"), nil], [Var.new("m2"), nil]])
    m1 = r.children[0]
    m2 = r.children[1]
    t.addChildren(m1, [[Var.new("n"), nil]])
    t.replaceSubtree(m2, Var.new("x"))
    @tree = t
  end

  def test01PrTreeBuilding()
    assert_equal(
    "{0:(r,,,[1,2]),1:(m1,,0,[3]),3:(n,,1,[]),2:(x,,0,[])}",
    @tree.to_s)
  end

  def test02PrTreeNodes()
    assert_equal([0,1,3,2], @tree.nodes.map{|n| n.nodeId})
  end

  def test03PrTreeLeaves()
    assert_equal([3,2], @tree.leaves.map{|n| n.nodeId})
  end

end