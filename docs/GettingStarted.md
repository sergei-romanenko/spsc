# Getting Started with SPSC

**SPSC** stands for _A Small Positive Supercompiler in Scala_. In order to use 
SPSC you need to have Scala installed. Download and install Scala from the 
[Scala home page](http://www.scala-lang.org). Download **spsc\_beta.jar** from 
Downloads section.

## Interpreter

You can play with Small Language interpreter in Scala shell:

```
$scala -cp spsc_alpha.jar 
Welcome to Scala version 2.7.0-final (Java HotSpot(TM) Client VM, Java 1.5.0_13).
Type in expressions to have them evaluated.
Type :help for more information.

scala> import spsc.Interpreter
import spsc.Interpreter

scala> val program = """
     | append(Nil, vs) = vs;
     | append(Cons(u, us), vs) = Cons(u, append(us, vs));
     | append2(xs, ys, zs) = append(append(xs, ys), zs);
     | append3(xs, ys) = append(append(xs, ys), xs);"""
program: java.lang.String = 

       append(Nil, vs) = vs;
       append(Cons(u, us), vs) = Cons(u, append(us, vs));
       append2(xs, ys, zs) = append(append(xs, ys), zs);
       append3(xs, ys) = append(append(xs, ys), xs);

scala> val in = new Interpreter(program)
in: spsc.Interpreter = spsc.Interpreter@44331c

scala> in.eval("append(Nil, Nil)")
res0: spsc.SmallLanguage.Term = Nil()
```

Also you can use `spsc.InterpreterApp` scala application:
```
usage: spcs.InterpreterApp -i input_file -e expression_to_evaluate
Where:
input_file                  path to input file (relative or absolute)
expression_to_evaluate      expression to evaluate
```

example:
```
scala -cp spsc_alpha.jar spsc.InterpreterApp -i samples/test.sl -e "test19(Cons(Nil,Nil),Nil)"
```

## Supercompiler

Supercompiler takes input program and name of f-function to be supercompiled and 
produces partial process tree (in SVG format) and residual program.

```
usage: spcs.SuperCompilerApp -i input_file -f function_name -t tree_output_file -p program_output_file
Where:
input_file            path to input file (relative or absolute)
function_name         name of f-function to be supercompiled
tree_output_file      path to file where process tree will be placed (in SVG format)
program_output_file   path to file where residual program will be placed
```

example:
```
scala -cp spsc_alpha.jar spsc.SuperCompilerApp -i samples/test.sl -f test19 -t 1.svg -p 1.sl
```
