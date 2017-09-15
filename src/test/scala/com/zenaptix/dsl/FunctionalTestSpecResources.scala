package com.zenaptix.dsl

import com.typesafe.config.ConfigFactory

import scala.io.{BufferedSource, Source}

/**
  * Created by rikus on 8/10/17.
  */

object FunctionalTestSpecResources {
  val conf = ConfigFactory.load()
  val source: BufferedSource = Source.fromFile("/home/rikus/Downloads/mainframe_test/CQSF602.txt")
  val lines: String = try source.getLines().mkString("\n") finally source.close()
  val forest: Seq[Group] = CopyBookSchema(lines).parseTree(EBCDIC())
  val roots = forest.head.traverseAll
}
