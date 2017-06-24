lazy val standardSettings = Defaults.defaultSettings ++ Seq(
  version      := "1.1",
  scalaVersion := "2.11.1"
)

lazy val spsc =
  project
    .in(file("spsc"))
    .settings(standardSettings:_*)
    .settings(
      libraryDependencies += "com.novocode" % "junit-interface" % "0.10" % "test",
      libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.1",
      libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"
    )

lazy val spsc_back =
  project
    .in(file("spsc_back"))
    .settings(standardSettings:_*)
    .settings(
      libraryDependencies += "net.databinder" %% "unfiltered-filter" % "0.8.0",
      libraryDependencies += "javax.servlet" % "servlet-api" % "2.5" % "provided",
      libraryDependencies += "org.mortbay.jetty" % "jetty" % "6.1.22" % "container"
    )
    .settings(appengineSettings:_*)
    .dependsOn(spsc)

lazy val root =
  Project(id = "parent", base = file(".")) aggregate(spsc, spsc_back)
