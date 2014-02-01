scalaVersion := "2.10.3"

name := "spsc-lite"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.9" % "test"

// there is a global genVar
parallelExecution in Test := false
