import sbt._
import Dependencies._
import Settings.{projectSettings, _}
import com.typesafe.sbt.packager.archetypes.JavaAppPackaging

lazy val macroAnnotationSettings = Seq(
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M10" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  scalacOptions in(Compile, console) ~= (_ filterNot (_ contains "paradise")) // macroparadise plugin doesn't work in repl yet.
)

lazy val macros = (project in file("macros")).
  settings(common: _*).
  settings(resolverSettings: _*).
  settings(libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value).
  settings(libraryDependencies ++= dependencies).
  settings(macroAnnotationSettings)

lazy val cobolCopyBook = (project in file("core")).
  settings(projectSettings: _*).
  settings(resolverSettings: _*).
  settings(libraryDependencies ++= dependencies).
  settings(macroAnnotationSettings).
  settings(
    publishTo := Some(Resolver.file("file", new File(Path.userHome.absolutePath + "/.m2/repository")))
  ).
  dependsOn(macros).
  enablePlugins(JavaAppPackaging)


        