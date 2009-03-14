package spsc;

import scala.util.parsing.input.StreamReader
import scala.util.parsing.input.CharArrayReader

import java.io.File
import java.io.FileInputStream
import java.io.InputStreamReader
import java.io.FileReader
import java.io.FileWriter
import java.io.BufferedReader

import SLanguageParsers._
import ResidualProgramGenerator._

object SuperCompilerApp {
  val help = """usage: spcs.SuperCompilerApp -i input_file -f function_name -t tree_output_file -p program_output_file
  |Where:
  |input_file            path to input file (relative or absolute)
  |function_name         name of f-function to be supercompiled
  |tree_output_file      path to file where process tree will be placed (in SVG format)
  |program_output_file   path to file where residual program will be placed
  |
  |if output_dir is not specified supercompilation results will be placed 
  |in current working directory
  |""".stripMargin
  def main(args : Array[String]) : Unit = {
    var fileName: String = null
    var funName: String = null
    var outFileName: String = null
    var outProgramFileName: String = null
    args.toList match {
      case "-i" :: input_file :: "-f" :: function_name :: "-t" :: output_file :: "-p" :: output_file_1 :: Nil =>
        fileName = input_file
        funName = function_name
        outFileName = output_file
        outProgramFileName = output_file_1
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
    val program = SLanguageParsers.parseProgram(new CharArrayReader(sb.toString.toCharArray))
    val function = program.f(funName)
    val sc = new SuperCompiler(program)
    val pt = sc.buildProcessTree(FCall(function.name, function.args))    
    val svg = new ProcessTreeSVG(pt).treeToSVG()
    
    val svgFile = new java.io.File(outFileName)
    if (!svgFile.exists){
      svgFile.createNewFile()
    } 
    scala.xml.XML.save(outFileName, svg)
    val text = generateResidualProgram(pt).toString
    val slFile = new java.io.File(outProgramFileName)
    if (!slFile.exists){
      slFile.createNewFile()
    }
    val fw = new FileWriter(slFile);
    fw.write(text);
    fw.flush();
    fw.close();
  }
}
