package spsc.web

import unfiltered.request._
import unfiltered.response._

import spsc._
import spsc.SmallLanguageParsers
import spsc.SmallLanguageParsers._
import scala.util.parsing.input.CharArrayReader

class App extends unfiltered.filter.Plan {
  def intent = {
    case POST(Path(action) & Params(params)) if action == "/validate" || action == "/run" => {
      val input: String =
        params.get("program").flatMap { _.headOption } getOrElse ""
      val result =
        SmallLanguageParsers.parseProgram(new CharArrayReader(input.toCharArray))
      val xml: scala.xml.Elem = result match {
        case Success(defs, _) =>
          val fname = params.get("fname").flatMap { _.headOption } getOrElse ""
          defs.find { case f: FFunction if f.name == fname => true; case _ => false;} match {
            case Some(_) => action match {
              case "/validate" =>
                <result status="ok"/>
              case "/run" =>
                val program = new Program(defs)
                val sc = new SuperCompiler(program)
                val function = program.getFFunction(fname)
                val processTree = sc.buildProcessTree(FCall(function.name, function.args))
                val residualCode = ResidualProgramGenerator.generateResidualProgram(processTree).toString
                val svg = new ProcessTreeSVG(processTree).treeToSVG()
                <result status="ok"><code>{residualCode}</code><tree>{svg}</tree></result>
            }
            case None =>
              <result status="unknownFunction" />
          }
        case e:NoSuccess =>
          val msg = e.msg
          val pos = e match {
            case se: SError => se.pos.pos
            case _ => e.next.pos
          }
          <result status="parseError"><details message={msg} line={pos.line.toString()} column={pos.column.toString()} /></result>
      }
      new ComposeResponse(ApplicationXmlContent ~> ResponseString(xml.toString()))
    }
  }

}
