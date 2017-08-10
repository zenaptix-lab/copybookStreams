package com.zenaptix.dsl

import java.io.File

import org.scalatest.{Tag, WordSpec}

import scala.collection.mutable

class CopyBookSchemaSpec extends WordSpec {

  import com.zenaptix.dsl.CopyBookResources._

  "CopyBookSchema" should {

    "build a navigable AST" in {
      println(Console.GREEN + "build a navigable AST" + Console.WHITE)
      val tree = Group(1, "root", mutable.ArrayBuffer(), parent = None)
      val node = tree
        .add(Group(10, "test", mutable.ArrayBuffer(), parent = None))
        .asInstanceOf[Group]
      node.add(Group(20, "blah", mutable.ArrayBuffer(), parent = None))
      val ll1 = node.add(Statement(20, "l1", parent = None, dataType = AlphaNumeric(length = 1)))
      val ll2 = node.add(Statement(20, "l2", parent = None, dataType = AlphaNumeric(length = 2)))
      val ll3 = node.add(Statement(20, "l3", parent = None, dataType = AlphaNumeric(length = 3)))
      val ll4 = node.add(Statement(20, "l4", parent = None, dataType = AlphaNumeric(length = 4)))
      node.add(Group(20, "b1", mutable.ArrayBuffer(), parent = None))
      node.add(Group(20, "b2", mutable.ArrayBuffer(), parent = None))

      val path = tree.children.head.asInstanceOf[Group].children.head.path()
      println(s"path: $path")
      val x = tree.get(path)
      println(x)
      val y = tree.get("root/test/blah")
      println(s"get - $y")
      val z = tree.get("test/blah")
      println(s"get - $z")
      val e = tree.get("root/tst/blah")
      println(s"get - $e")
      val d = node.up()
      println(d)
      val f = tree.up()
      println(f)
      println(s"\n\n path ---- ${ll2.path()}")

      val l2 = tree.get("/root/test/l2")
      println(s"l2 ---- $l2")

      println(s"l3 ---- ${ll2.nextSiblingLeaf()}")
      println(s"l3 ---- ${tree.get("/root/test/l3")}")

      println(s"l4 ---- ${ll3.nextSiblingLeaf()}")
      println(s"l4 ---- ${tree.get("/root/test/l4")}")

      println(s"none ---- ${ll4.nextSiblingLeaf()}")
      println(s"none ---- ${tree.get("/root/test/l5")}")

    }

    "traverse the AST" in {
      println(Console.GREEN + "traverse the AST" + Console.WHITE)
      roots.foreach { root =>
        println("Root : " + root.name)
        val s = root.traverseStatements
        println(s"\n\nTRAVERSAL 1 {Statements}:\n ${s.mkString("\n")}")

        val t = root.traverseAll
        println(s"\n\nTRAVERSAL 2 {All}:\n ${t.mkString("\n")}")

        val g = root.traverseGroups
        println(s"\n\nTRAVERSAL 3 {Groups}:\n ${g.mkString("\n")}")
      }
    }

    "traverse the statements of an AST " in {
      println(Console.GREEN + "traverse the statements of an AST " + Console.WHITE)
      roots.foreach({ root =>
        root.traverseStatements.map({ statement =>
          statement.name
        })
      })
    }

    "camelCase a variable name and" in {
      println(Console.GREEN + "camelCase a variable name and" + Console.WHITE)
      roots.foreach { root =>
        println(s"${root.name} camelCased is ${root.camelCased} ->  ${root.camelCaseVar} ")
      }
    }

    "parse data using CBTree" in {
      println(Console.GREEN + "parse data using CBTree" + Console.WHITE)
      import scodec._
      import scodec.bits._
      import codecs._
      val y: BitVector = BitVector("1234testing".getBytes) ++ BitVector(0x10, 0x2a, 0x03, 0xff) ++ BitVector("5678MoreTesting".getBytes)
      println("bitVector : " + y)
      val firstCodec = uint4 ~ uint4 ~ uint16
      val result2: Attempt[DecodeResult[(Int ~ Int ~ Int)]] = Codec.decode(y)(firstCodec)

      println("Decoded Result : " + result2)
      println(result2.require.remainder.decodeAscii.right)
      println(result2.require.value)

      println(y)

    }

    "create case classes source from AST" in {
      println(Console.GREEN + "create case classes source from AST" + Console.WHITE)
      val packageName = "com.zenaptix.test"
      roots.foreach { root =>
        println(root.name)
        val c = root.traverseGroups.map(g => g.asCaseClass)
        //        println("Case Classes : ")
        //        c.foreach(g => println(g))
        root.printToFile(new File(s"src/test/scala/${packageName.replace(".", "/")}/${root.camelCased}.scala")) { p =>
          p.println(s"package $packageName")
          c.foreach(p.println)
        }
      }
    }
  }
}


