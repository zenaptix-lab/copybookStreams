import sbt.Keys.{resolvers, _}
import sbt._
import java.util.Properties

object Settings {
  val appProperties = settingKey[Properties]("The application properties")

  lazy val common = Seq(
    scalaVersion := "2.11.11",
    organization in ThisBuild := "com.zenaptix",
    resolvers += Resolver.jcenterRepo,
    publishMavenStyle := true,
    publishArtifact in Test := false
  )

  lazy val projectSettings = Seq(
    name := "cobol_copybook",
    version := "1.0"
  ) ++ common

  lazy val resolverSettings = Seq(
    resolvers += Resolver.sonatypeRepo("releases"),
    resolvers ++= Seq(
      "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository",
      "Local Ivy Repository" at "file://" + Path.userHome.absolutePath + "/.ivy2/local",
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
      Resolver.bintrayRepo("stanch", "maven"),
      Resolver.jcenterRepo,
      Resolver.url("bintry",url("https://dl.bintray.com/stanch/maven"))
    )
  )

}