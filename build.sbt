
name := "cobol_copybook"

version := "1.0"

scalaVersion := "2.11.8"

organization in ThisBuild := "com.zenaptix"

libraryDependencies ++= Seq(
  "com.sksamuel.avro4s" %% "avro4s-core" % "1.6.4",
  "com.sksamuel.avro4s" %% "avro4s-json" % "1.6.4",
  "com.sksamuel.avro4s" %% "avro4s-generator" % "1.6.0",
  "com.snowplowanalytics" %% "schema-guru" % "0.6.2",
  "tech.allegro.schema.json2avro" % "converter" % "0.2.5",
  "com.github.nscala-time" %% "nscala-time" % "2.16.0",
  "org.scodec" % "scodec-bits_2.11" % "1.1.4",
  "org.scodec" % "scodec-core_2.11" % "1.10.3",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "com.gensler" % "scalavro_2.10" % "0.6.2",
  "com.chuusai" %% "shapeless" % "2.3.2"

)
        