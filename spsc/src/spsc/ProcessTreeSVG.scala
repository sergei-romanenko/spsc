package spsc;

object ProcessTreeSVG {
  def treeToSVG(tree: ProcessTree) = 
    <svg xmlns="http://www.w3.org/2000/svg" width={"" + width(tree.rootNode)} height={"" + height(tree.rootNode)} >
    <defs>
    <style type="text/css">
    <![CDATA[
    rect {fill: none;stroke: black; stroke-width: 1;}
    text {text-anchor: middle; font-family: monospace; font-size: 10px;}
    line {stroke: black; stroke-width: 1}]]></style>
    </defs>
    {nodeToSVG(tree.rootNode,0, 0)}
    </svg>
    
    def nodeToSVG(node: ProcessTree.Node, trX: Int, trY: Int): scala.xml.NodeBuffer = {
      def childrenToSVG() = {
        val children = new scala.xml.NodeBuffer
        var trChX = (width(node) - childrenWidth(node))/2
        for (out<-node.outs) {
          val child = out.child
          children += 
            <line x1={"" + (trX+width(node)/2)} y1={""+(trY+30)} x2={"" + (trX+trChX+width(child)/2)} y2={""+(trY+100)}/>
          if (!out.substitution.isEmpty)
          children +=
            <text x={"" + (trX + trChX + width(child)/2)} y = {"" + (80 + trY)}>{out.substitution.toList.map(kv => kv._1 + "=" + kv._2).mkString("", ", ", "")}</text>
          children ++= nodeToSVG(child, trX + trChX, trY + 100)
          trChX += width(child)
        }
        children
      }
    <rect x={"" + (trX + (width(node) - rectWidth(node))/2)} y={"" + trY} width={"" + rectWidth(node)} height="30" />
    <text x={"" + (trX + width(node)/2)} y ={"" + (trY + 15)}>{node.expr.toString}</text> 
    &+ childrenToSVG
    }   
      
    def width(node: ProcessTree.Node): Int = {
      val myWidth = rectWidth(node) + 40
      Math.max(myWidth, childrenWidth(node))
    }
    
    def height(node: ProcessTree.Node): Int = {
      var childrenHeight = 0
      for (out <- node.outs) childrenHeight = Math.max(height(out.child), childrenHeight)
      if (childrenHeight > 0) childrenHeight + 100 else 30
    }
    
    def childrenWidth(node: ProcessTree.Node): Int = {
      var childrenWidth = 0
      for (out <- node.outs){
        childrenWidth += width(out.child)
      }
      childrenWidth
    }
    
    
    def rectWidth(node: ProcessTree.Node): Int = node.expr.toString.length*6 + 10

}
