lazy val wordRacer = (project in file("."))
  .settings(
    name := "wordracer-scala",
    version := "1.0.0-SNAPSHOT",
    scalaVersion := "2.12.8",
    libraryDependencies ++= Seq(scalatest, junit, jsoup, sprayJson),
    mainClass in assembly := Some("com.codingnirvana.wordracer.Runner"),
    assemblyJarName in assembly := "wordracer-scala-my-name.jar" // Replace this with a jar name of your choice
  )

val scalatest = "org.scalatest" %% "scalatest" % "3.0.8" % Test
val junit = "junit" % "junit" % "4.12" % Test
val jsoup = "org.jsoup" % "jsoup" % "1.8.3"
val sprayJson = "io.spray" %% "spray-json" % "1.3.2"