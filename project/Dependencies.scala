import sbt._

object Dependencies {
  lazy val version = new {
    val avro4S = "1.6.4"
  }

  val dependencies = Seq(
    "com.sksamuel.avro4s" %% "avro4s-core" % version.avro4S,
    "com.sksamuel.avro4s" %% "avro4s-json" % version.avro4S,
    "com.sksamuel.avro4s" %% "avro4s-generator" % "1.6.0",
    "tech.allegro.schema.json2avro" % "converter" % "0.2.5",
    "com.github.nscala-time" %% "nscala-time" % "2.16.0",
    "org.scodec" % "scodec-bits_2.11" % "1.1.4",
    "org.scodec" % "scodec-core_2.11" % "1.10.3",
    "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    "com.gensler" % "scalavro_2.10" % "0.6.2",
    "com.chuusai" %% "shapeless" % "2.3.2",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
    "ch.qos.logback" % "logback-classic" % "1.2.3",
    "org.scalameta" %% "scalameta" % "1.8.0",
    "org.stanch" %% "reftree" % "1.2.0"
  )
}