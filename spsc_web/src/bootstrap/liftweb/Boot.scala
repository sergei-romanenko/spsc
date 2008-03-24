package bootstrap.liftweb

import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import Helpers._
 
/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
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
    LiftServlet.addToPackages("spsc")     

    // Build SiteMap
    val entries = Menu(Loc("Home", "/", "Home")) :: 
                  Menu(Loc("spsc1", "/spsc", "SPSC online")) ::
                  Menu(Loc("spsc2", "/spsc_result", "SPSC result", Hidden)) ::
                  Nil 
    LiftServlet.setSiteMap(SiteMap(entries:_*))
  }
}

