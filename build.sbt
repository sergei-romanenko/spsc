scalaVersion := "2.11.6"

name := "spsc-lite"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10" % "test"

// there is a global genVar
parallelExecution in Test := false

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v")
