package spsc.service

import net.liftweb.http.{LiftRules, Req, PostRequest, S, XmlResponse}
import net.liftweb.util.Full
import spsc.SmallLanguageParsers
import spsc.SmallLanguageParsers._
import scala.util.parsing.input.CharArrayReader

object AppService {
  val dispatcher: LiftRules.DispatchPF = {
    case Req(action::Nil, _, PostRequest) if action == "validate" || action == "run" => {
      () => {
        val result = SmallLanguageParsers.parseProgram(new CharArrayReader(S.param("program").openOr("").toCharArray))
        val xml = result match {
          case Success(defs, _) => {
            val fname = S.param("fname").openOr("") 
            defs.find(d => d match {case f:FFunction if f.name == fname => true; case _ => false;}) match {
              case Some(_) => action match {
                case "validate" => (<result status="ok" />)
                case "run" => {
                  val program = new Program(defs)
                  val sc = new SuperCompiler(program)
                  val function = program.getFFunction(fname)
                  val processTree = sc.buildProcessTree(FCall(function.name, function.args))
                  val residualCode = ResidualProgramGenerator.generateResidualProgram(processTree).toString
                  val svg = new ProcessTreeSVG(processTree).treeToSVG()
                  (<result status="ok"><code>{residualCode}</code><tree>{svg}</tree></result>)
                }
              }
              case None => (<result status="unknownFunction" />)
            }
          }
          case e:NoSuccess => {
            val msg = e.msg
            val pos = if (e.isInstanceOf[SError]) e.asInstanceOf[SError].pos.pos else e.next.pos  
            (<result status="parseError"><details message={msg} line={pos.line.toString()} column={pos.column.toString()} /></result>)
          } 
        }
        Full(XmlResponse(xml))
      }
    }
  }
}
