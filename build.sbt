ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.6"

lazy val root = (project in file("."))
  .aggregate(foo.js, foo.jvm)
  .settings(
    name := "EvolutionGame"
  )

val circeVersion = "0.14.14"

lazy val foo = crossProject(JSPlatform, JVMPlatform).in(file(".")).
  settings(
    name := "foo",
    version := "0.1-SNAPSHOT",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test
  ).
  jvmSettings(
    // Add JVM-specific settings here
  ).
  jsSettings(
    // Add JS-specific settings here
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.0",
    libraryDependencies += "io.circe" %%% "circe-core" % circeVersion,
    libraryDependencies += "io.circe" %%% "circe-generic" % circeVersion,
    libraryDependencies += "io.circe" %%% "circe-parser" % circeVersion,
    scalaJSUseMainModuleInitializer := false,
  )