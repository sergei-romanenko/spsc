package spsc.snippet;

import net.liftweb.http.S
import spsc.ProcessTreeSVG
import spsc.ResidualProgramGenerator._
import spsc.SmallLanguage._
import spsc.SuperCompiler
import scala.util.parsing.input.CharArrayReader

class SPSC {
  def input = <pre>{S.param("program").openOr("")}</pre>
  def fname = <pre>{S.param("fname").openOr("")}</pre>
  def output = 
  
  {
    val result = SmallLanguageParsers.parseProgram(new CharArrayReader(S.param("program").openOr("").toCharArray))
    if (!result.successful){
      <div><pre>{result.toString}</pre></div>
    } else {
      val program = new Program(result.get)
      val fname = S.param("fname").openOr("")
      program.definitions.find(d => d match {case f:FFunction if f.name == fname => true; case _ => false;}) match {
        case None => <pre>Error: f-function with name {fname} is not defined.</pre>
        case Some(f) =>
          val function = program.getFFunction(fname)
          val sc = new SuperCompiler(program)
          val pt = sc.buildProcessTree(FCall(function.name, function.args))
          <div>
            <h2>Supercompiled code</h2>            
            <pre>{generateResidualProgram(pt).toString}</pre>
            <h2>Partial process tree</h2>
            {new ProcessTreeSVG(pt).treeToSVG()}
          </div>
      }      
    }
  }
}
