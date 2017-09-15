import sbt._
import Dependencies._
import Settings.{projectSettings, _}
import com.typesafe.sbt.packager.archetypes.JavaAppPackaging

lazy val cobolCopyBook = (project in file(".")).
  settings(projectSettings: _*).
  settings(resolverSettings: _*).
  settings(libraryDependencies ++= dependencies).
  settings(
    publishTo := Some(Resolver.file("file", new File(Path.userHome.absolutePath + "/.m2/repository")))
  ).enablePlugins(JavaAppPackaging)



        