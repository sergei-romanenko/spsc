package bootstrap.liftweb

import net.liftweb.util.Full
import net.liftweb.http.{ResponseInfo, LiftRules, S}
import net.liftweb.sitemap.{Menu, SiteMap, Loc}
import net.liftweb.sitemap.Loc._

class Boot {
  
  val xhtmlSvgMathMl = """<?xml-stylesheet type="text/xsl" href="dummy.xsl"?>
  <!DOCTYPE html PUBLIC 
  "-//W3C//DTD XHTML 1.1 plus MathML 2.0 plus SVG 1.1//EN" 
  "http://www.w3.org/2002/04/xhtml-math-svg/xhtml-math-svg.dtd">"""
  def boot {    
    ResponseInfo.docType = {
      case _ if S.getDocType._1 => S.getDocType._2
      case _ => Full(xhtmlSvgMathMl)
    }
    
    // where to search snippet
    LiftRules.addToPackages("spsc")     

    // Build SiteMap
    val entries = Menu(Loc("Home", List("index"), "Home")) :: 
                  Menu(Loc("spsc1", List("spsc"), "SPSC online")) ::
                  Menu(Loc("spsc2", List("spsc_result"), "SPSC result", Hidden)) ::
                  Nil 
    LiftRules.setSiteMap(SiteMap(entries:_*))
  }
}

