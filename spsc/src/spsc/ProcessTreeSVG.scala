package spsc;

class ProcessTreeSVG(tree: ProcessTree) {
  var map = Map[ProcessTree.Node, Tuple4[Int, Int, Int, Int]]()
  def treeToSVG() = 
    <svg:svg xmlns:svg="http://www.w3.org/2000/svg" width={"" + width(tree.rootNode)} height={"" + height(tree.rootNode)} >
    <svg:defs>
    <svg:style type="text/css">
    <![CDATA[
    rect {fill: none;stroke: black; stroke-width: 1;}
    text {text-anchor: middle; font-family: monospace; font-size: 10px;}
    line {stroke: black; stroke-width: 1}
    path {fill:none; stroke:black;stroke-width:1;stroke-dasharray: 4,4;}]]></svg:style>]]
    </svg:defs>
    {nodeToSVG(tree.rootNode,0, 0)}
    {repeatEdges()}
    </svg:svg>
    
    def nodeToSVG(node: ProcessTree.Node, trX: Int, trY: Int): scala.xml.NodeBuffer = {
      def childrenToSVG() = {
        val children = new scala.xml.NodeBuffer
        var trChX = (width(node) - childrenWidth(node))/2
        for (out<-node.outs) {
          val child = out.child
          children += 
            <svg:line x1={"" + (trX+width(node)/2)} y1={""+(trY+30)} x2={"" + (trX+trChX+width(child)/2)} y2={""+(trY+100)}/>
          if (!out.substitution.isEmpty)
          children +=
            <svg:text x={"" + (trX + trChX + width(child)/2)} y = {"" + (80 + trY)}>{out.substitution.toList.map(kv => kv._1 + "=" + kv._2).mkString("", ", ", "")}</svg:text>
          children ++= nodeToSVG(child, trX + trChX, trY + 100)
          trChX += width(child)
        }
        children
      }
    { 
      val tw = width(node)
      val w = rectWidth(node);
      val x = (trX + (tw - w)/2) + w/2;
      val y = trY + 15      
      map = map + (node -> (x, y, w, tw))
    }
    <svg:rect x={"" + (trX + (width(node) - rectWidth(node))/2)} y={"" + trY} width={"" + rectWidth(node)} height="30" />
    <svg:text x={"" + (trX + width(node)/2)} y ={"" + (trY + 15)}>{node.expr.toString}</svg:text> 
    &+ childrenToSVG
    }
    
    def repeatEdges() : scala.xml.NodeBuffer = {
      val edges = new scala.xml.NodeBuffer
      for (n <- tree.leafs) {
        val pn = n.getRepParent
        if (pn != null) {
          val (cx, cy, cw, ctw) = map(n)
          val (px, py, pw, ptw) = map(pn)
          val s = if (cx > px) 1 else -1 
          val d = "M " + (cx + s*cw/2) + ", " + cy + 
                  " C " + (px + s*ptw/2) + ", " + cy + " " + 
                  (px + s*ptw/2) + ", " + py + " " +
                  (px + s*pw/2) + ", " + py;
          edges += <svg:path d={d}/>        
        }
      }
      edges
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
