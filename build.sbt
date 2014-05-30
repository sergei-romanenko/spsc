lazy val standardSettings = Defaults.defaultSettings ++ Seq(
  version      := "1.1",
  scalaVersion := "2.11.1"
)

lazy val spsc =
  project.
    in(file("spsc")).
    settings(standardSettings:_*)
    .settings(
      libraryDependencies += "com.novocode" % "junit-interface" % "0.10" % "test",
      libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.1",
      libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"
    )
