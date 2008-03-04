package spsc;

import scala.util.parsing.input.StreamReader
import scala.util.parsing.input.CharArrayReader

import java.io.File
import java.io.FileInputStream
import java.io.InputStreamReader
import java.io.FileReader
import java.io.BufferedReader

import SmallLanguage._
import SmallLanguageParsers._

object SuperCompilerApp {
  val help = """usage: spcs.SuperCompilerApp -i input_file -f function_name [-o output_file]
  |Where:
  |input_file       path to input file (relative or absolute)
  |function_name    name of f-function to be supercompiled
  |output_file      path to file where supercompilation results will be placed
  |
  |if output_dir is not specified supercompilation results will be placed 
  |in current working directory
  |""".stripMargin
  def main(args : Array[String]) : Unit = {
    var fileName: String = null
    var funName: String = null
    var outFileName: String = ""
    args.toList match {
      case "-i" :: input_file :: "-f" :: function_name :: "-o" :: output_file :: Nil =>
        fileName = input_file
        funName = function_name
        outFileName = output_file
      case "-help" :: Nil => 
        println(help)
        return
      case _ => 
        throw new IllegalArgumentException("run spcs.SuperCompilerApp -help for help")       
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
    val function = program.getFFunction(funName)
    val sc = new SuperCompiler(program)
    val pt = sc.buildProcessTree(FCall(function.name, function.args))    
    val svg = ProcessTreeSVG.treeToSVG(pt)
    
    val svgFile = new java.io.File(outFileName)
    if (!svgFile.exists){
      svgFile.createNewFile()
    } 
    scala.xml.XML.save(outFileName, svg)
  }
}
