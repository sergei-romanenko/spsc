require 'SLL'
require 'Algebra'

module ProcessTree
  class Contraction
    attr_accessor :vname, :cname, :cparams
    def initialize(vname, cname, cparams)
      @vname = vname
      @cname = cname
      @cparams = cparams
    end

    def to_s
      pat_s = @cname
      if @cparams.length > 0
        pat_s += "(#{@cparams * ","})"
      end
      "#{@vname}=#{pat_s}"
    end
  end

  class Node
    include Algebra
    attr_reader :nodeId
    attr_accessor :exp
    attr_reader:contr, :parent
    attr_accessor :children
    # The constructor is supposed to be called via ProcessTree#newNode only.
    def initialize(tree, exp, contr, parent, children)
      # nodeId is only used for unit testing purposes
      @nodeId = tree.getFreshNodeId()
      @exp = exp
      @contr = contr
      @parent = parent
      @children = children
    end

    def showNodeId(node)
      node != nil ? node.nodeId : nil
    end

    def to_s
      children_s = @children.map{|n| n.nodeId.to_s} * ","
      "#{@nodeId}:(#{@exp},#{@contr.to_s},#{showNodeId(@parent)},[#{children_s}])"
    end

    def ancestors
      acc = Array.new
      n = @parent
      while n != nil do
        acc << n
        n = n.parent
      end
      acc
    end

    def funcAncestor()
      ancestors.find{|n| equiv(@exp, n.exp)}
    end

    def findMoreGeneralAncestor()
      ancestors.find{|n| n.exp.isFGCall() && instOf(@exp, n.exp)}
    end

    def isProcessed()
      if @exp.isVar()
        true
      elsif @exp.isCtr()
        @exp.args == []
      elsif @exp.isFGCall()
        funcAncestor() != nil
      elsif @exp.isLet()
        false
      else
        raise "Invalid exp"
      end
    end

    def subtreeNodes
      acc = [self]
      @children.each do |child|
        acc.push(*child.subtreeNodes)
      end
      acc
    end

    def isLeaf()
      @children == []
    end

    def subtreeLeaves
      if isLeaf
        [self]
      else
        acc =Array.new
        @children.each do |child|
          acc.push(*child.subtreeLeaves)
        end
        acc
      end
    end

  end

  class Tree
    # NB: The tree is not functional, since its nodes are updated in place.
    attr_reader :root
    def initialize(exp)
      @freshNodeId = -1
      @root = newNode(exp, nil, nil, [])
    end

    def to_s
      "{#{nodes.map{|n| n.to_s} * ","}}"
    end

    def getFreshNodeId()
      @freshNodeId += 1
      @freshNodeId
    end

    def newNode(exp, contr, parent, children)
      return Node.new(self, exp, contr, parent, children)
    end

    def nodes()
      @root.subtreeNodes
    end

    def leaves()
      #  Here, the tree is supposed not to be empty.
      @root.subtreeLeaves
    end

    def findUnprocessedNode()
      leaves.find{|leaf| ! leaf.isProcessed}
    end

    def isFuncNode(node)
      leaves.find{|leaf| node == leaf.funcAncestor} != nil
    end

    def addChildren(node, branches)
      children = branches.map {|b| newNode(b[0], b[1], node, [])}
      node.children = children
    end

    def replaceSubtree(node, exp)
      node.children = []
      node.exp = exp
    end
  end

end