scalaVersion := "2.10.3"

name := "spsc-lite"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10" % "test"

// there is a global genVar
parallelExecution in Test := false

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v")
