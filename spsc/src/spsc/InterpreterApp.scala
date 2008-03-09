package spsc;

import scala.util.parsing.input.StreamReader
import scala.util.parsing.input.CharArrayReader

import java.io.File
import java.io.FileInputStream
import java.io.InputStreamReader
import java.io.FileReader
import java.io.FileWriter
import java.io.BufferedReader

import SmallLanguage._
import SmallLanguageParsers._
import ResidualProgramGenerator._

object InterpreterApp {
  val help = """usage: spcs.InterpreterApp -i input_file -e expression_to_evaluate
  |Where:
  |input_file                  path to input file (relative or absolute)
  |expression_to_evaluate      name of f-function to be supercompiled
  |""".stripMargin
  def main(args : Array[String]) : Unit = {
    var fileName: String = null
    var expr: String = null
    args.toList match {
      case "-i" :: input_file :: "-e" :: expr_text :: Nil =>
        fileName = input_file
        expr = expr_text
      case "-help" :: Nil => 
        println(help)
        return
      case _ => 
        throw new IllegalArgumentException("run spcs.InterpreterApp -help for help")       
    }
    val file = new File(fileName)
    val sb = new StringBuilder
    val in = new BufferedReader(new FileReader(fileName));
    var str: String = null
    do {
      str = in.readLine
      if (str != null){
        sb.append(str)
        sb.append("\n")
      }
    } while (str != null)
    in.close();
    val result = SmallLanguageParsers.parseProgram(new CharArrayReader(sb.toString.toCharArray))
    if (!result.successful){
      throw new IllegalArgumentException(result.toString)
    }
    val program = new Program(result.get)
    val interpreter = new Interpreter(program)
    Console.println(interpreter.eval(expr))
  }
}
