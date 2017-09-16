package com.zenaptix.dsl

import java.io.{File, FileInputStream, FileOutputStream, IOException}
import java.nio.ByteBuffer

import org.apache.avro.Schema
import org.apache.avro.generic.GenericData
import scodec.bits.BitVector

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.matching.Regex
import scodec._
import scodec.bits._
import codecs._
import com.sksamuel.avro4s.AvroSchema
import com.typesafe.scalalogging.LazyLogging
import shapeless._
import syntax.std.traversable._

import scala.reflect.api
import scala.util.{Failure, Success, Try}
import scala.reflect.runtime.universe._


/**
  * Created by ian on 2017/01/21.
  */

/**
  *
  */
sealed trait CobolType

/**
  *
  */
sealed trait Position

/**
  *
  */
case object Left extends Position

/**
  *
  */
case object Right extends Position

/**
  *
  */
sealed trait Encoding {
  def codec(comp: Option[Int], scale: Int, signPosition: Option[Position]): Codec[_ <: AnyVal]
}

/**
  *
  */
case class ASCII() extends Encoding {
  def codec(comp: Option[Int], scale: Int, signPosition: Option[Position]): Codec[_ <: AnyVal] = {
    val cd = comp match {
      case Some(x) if x.isInstanceOf[Int] => {
        x match {
          case bin if bin == 0 || bin == 4 => {
            scale match { //if native binary follow IBM guide to digit binary length
              case a if a >= 1 && a <= 4 => {
                if (signPosition.getOrElse(None) != None) int16 else uint16
              }
              case b if b >= 5 && b <= 9 => {
                if (signPosition.getOrElse(None) != None) int32 else uint32
              }
              case c if c >= 10 && c <= 18 => {
                if (signPosition.getOrElse(None) != None) int64 else uint(64)
              }
            }
          }
          case spfloat if spfloat == 1 => float
          case dpfloat if dpfloat == 2 => floatL
          case bcd if bcd == 3 => uint4
        }
      }
      case None => uint8 // DISPLAY(Every digit=byte)
    }
    cd
  }
}

/**
  *
  */
case class EBCDIC() extends Encoding {
  def codec(comp: Option[Int], scale: Int, signPosition: Option[Position]): Codec[_ <: AnyVal] = {
    val cd = comp match {
      case Some(x) if x.isInstanceOf[Int] => {
        x match {
          case bin if bin == 0 || bin == 4 => {
            scale match { //if native binary follow IBM guide to digit binary length
              case a if a >= 1 && a <= 4 => {
                if (signPosition.getOrElse(None) != None) int16 else uint16
              }
              case b if b >= 5 && b <= 9 => {
                if (signPosition.getOrElse(None) != None) int32 else uint32
              }
              case c if c >= 10 && c <= 18 => {
                if (signPosition.getOrElse(None) != None) int64 else uint(64)
              }
            }
          }
          case spfloat if spfloat == 1 => float
          case dpfloat if dpfloat == 2 => floatL
          case bcd if bcd == 3 => uint4
        }
      }
      case None => uint8 // DISPLAY(Every digit=byte), remember highest nybble of LSB contains the sign
    }
    cd
  }
}

/**
  *
  * @param scale
  * @param precision
  * @param signPosition
  * @param wordAlligned
  * @param compact
  */
case class Decimal(
                    scale: Int,
                    precision: Int,
                    signPosition: Option[Position] = None,
                    wordAlligned: Option[Position] = None,
                    compact: Option[Int] = None,
                    enc: Option[Encoding] = None
                  )
  extends CobolType

/**
  *
  * @param scale
  * @param signPosition
  * @param wordAlligned
  * @param compact
  */
case class Integer(
                    scale: Int,
                    signPosition: Option[Position] = None,
                    wordAlligned: Option[Position] = None,
                    compact: Option[Int] = None,
                    enc: Option[Encoding] = None
                  )
  extends CobolType

/**
  *
  * @param length
  * @param wordAlligned
  */

case class AlphaNumeric(
                         length: Int,
                         wordAlligned: Option[Position] = None,
                         enc: Option[Encoding] = None
                       )
  extends CobolType

/**
  *
  * @param cpyBook
  */
case class CopyBookSchema(cpyBook: String) extends LazyLogging{
  val matcher: Regex = "\\(([^)]+)\\)".r

  /**
    *
    * @return
    */
  def parseTree(enc: Encoding): Seq[Group] = {
    val tokens: Array[Array[String]] = tokenize()
    val lexed: Array[Map[String, String]] = tokens.map(l => lex(l))

    val lines: Seq[((Int, String), Map[String, String])] =
      tokens.map(l => (l.head.toInt, l(1))).zip(lexed)
    // create a tuple of index value and root names for all the 01 levels
    val b: Seq[(String, Int)] = lines.view.zipWithIndex.collect {
      case (((1, s: String), (_)), i: Int) => (s, i)
    }
    val e: Seq[Int] = b.drop(1).map(_._2) :+ lines.length
    val c: Seq[((String, Int), Int)] = b.zip(e)
    val breakpoints: Seq[(String, (Int, Int))] = c.map(i => (i._1._1, (i._1._2, i._2)))
    logger.info("breakpoints : " + breakpoints.mkString(";"))

    val forest: Seq[Seq[((Int, String), Map[String, String])]] = breakpoints.map(p => lines.slice(p._2._1, p._2._2)) //forest should only have multiple items if there is a duplicate level
    logger.info(Console.GREEN + "FOREST : " + forest.mkString("\n") + Console.WHITE)

    forest.map { f =>
      val root = Group(1, f.head._1._2,
        mutable.ArrayBuffer(),
        parent = None,
        redefines = None)
      val trees = f
        .drop(1) // root already added so drop first line
        .foldLeft[CBTree](root)((q, f) => {
        val keywords = f._2.keys.toList
        val isLeaf = keywords.contains("PIC") || keywords.contains("COMP-")
        val level: Int = f._1._1
        val name = f._1._2
        val redefines = f._2.get("REDEFINES")
        val occurs = f._2.get("OCCURS").map(i => i.toInt)
        val to = f._2.get("TO").map(i => i.toInt)
        level match {
          case i if i > q.level =>
            if (isLeaf) {
              val t = typeAndLengthFromString(keywords, f._2)(enc)
              q.asInstanceOf[Group]
                .add(
                  Statement(
                    level,
                    name,
                    t,
                    redefines = redefines,
                    occurs = occurs,
                    to = to
                  )
                )
            } else
              q.asInstanceOf[Group]
                .add(
                  Group(
                    level,
                    name,
                    redefines = redefines,
                    occurs = occurs,
                    to = to
                  )
                )
          case i if i < q.level =>
            val u = q.up().get.up().get.asInstanceOf[Group]
            //            println("q.up().up()", u.camelCaseVar)
            val uu = if (level == u.level) {
              u.up().get.asInstanceOf[Group]
            }
            else u

            if (isLeaf) {
              val t = typeAndLengthFromString(keywords, f._2)(enc)
              uu.add(
                Statement(
                  level,
                  name,
                  t,
                  redefines = redefines,
                  occurs = occurs,
                  to = to
                )
              )
            } else {
              uu.add(
                Group(
                  level,
                  name,
                  redefines = redefines,
                  occurs = occurs,
                  to = to
                )
              )
            }
          case i if i == q.level =>
            if (isLeaf) {
              val t = typeAndLengthFromString(keywords, f._2)(enc)
              q.up()
                .get
                .asInstanceOf[Group]
                .add(
                  Statement(
                    level,
                    name,
                    t,
                    redefines = redefines,
                    occurs = occurs,
                    to = to
                  )
                )
            } else
              q.up()
                .get
                .asInstanceOf[Group]
                .add(
                  Group(
                    level,
                    name,
                    redefines = redefines,
                    occurs = occurs,
                    to = to
                  )
                )
        }
      })
      root
    }
  }

  /**
    *
    * @return
    */
  def tokenize(): Array[Array[String]] = {
    val doc = cpyBook
      // line breaks
      .split("\\r?\\n")
      // ignore first 6 columns (historically for line numbers)
      .map(
      l =>
        l.drop(6)
          // remove unnecessary white space
          .replaceAll("\\s\\s+", " ")
          .trim()
    )
      // ignore commented lines§
      .filterNot(l => l.startsWith("*"))
      .mkString(" ")
    doc
      .split('.')
      .map(l => l.replaceAll("^\\s+", ""))
      .filterNot(l =>
        l.startsWith("66") || l.startsWith("77") || l
          .startsWith("88") || l.trim().isEmpty)
      .map(l => {
        l.trim().split("\\s+")
      })

  }

  /**
    *
    * @param tokens
    * @return
    */
  def lex(tokens: Array[String]): Map[String, String] = {
    val keywords =
      List("REDEFINES ", "OCCURS ", "TO ", "PIC ", "COMP-", "COMP")
    keywords.flatMap { k =>
      reservedWordModifier(tokens.mkString(" "), k).collect {
        case v: String => (k.trim(), v)
      }
    }.toMap
  }

  /**
    *
    * @param statement
    * @param word
    * @return
    */
  def reservedWordModifier(statement: String, word: String): Option[String] = {
    if (word.startsWith("SYNC") && statement.contains("SYNC")) Some("Right")
    else if (word == "COMP" && statement.contains("COMP"))
      Some("") // comp is a special case - binary encoded
    else if (statement.contains(word))
      statement.split(s" $word").drop(1).head.split(' ').headOption
    else None
  }

  /**
    *
    * @param keywords
    * @param modifiers
    * @return
    */
  def typeAndLengthFromString(
                               keywords: List[String],
                               modifiers: Map[String, String]
                             )(enc: Encoding): CobolType = {
    val comp: Option[Int] =
      if (keywords.contains("COMP-"))
        Some(modifiers.getOrElse("COMP-", "1").toInt)
      else {
        if (keywords.contains("COMP")) Some(4)
        else None
      }

    val sync = keywords.contains("SYNC")
    modifiers.get("PIC").head match {
      case s if s.contains("X(") || s.contains("A(") =>
        AlphaNumeric(
          matcher.findFirstIn(s).getOrElse("(1)").drop(1).dropRight(1).toInt,
          wordAlligned = if (sync) Some(Left) else None,
          Some(enc)
        )
      case s if s.contains("X") || s.contains("A") =>
        AlphaNumeric(s.length, wordAlligned = if (sync) Some(Left) else None, Some(enc))
      case s if s.contains("V") =>
        val dl = decimalLength(s)
        Decimal(
          dl._2,
          dl._1 + dl._2,
          if (s.startsWith("S")) Some(Left) else None,
          if (sync) Some(Right) else None,
          comp,
          Some(enc)
        )
      case s if s.contains("9(") =>
        Integer(
          scale =
            matcher.findFirstIn(s).getOrElse("(1)").drop(1).dropRight(1).toInt,
          signPosition = if (s.startsWith("S")) Some(Left) else None,
          wordAlligned = if (sync) Some(Right) else None,
          compact = comp,
          Some(enc)
        )
      case s if s.contains("9") =>
        Integer(
          scale = s.length,
          signPosition = if (s.startsWith("S")) Some(Left) else None,
          wordAlligned = if (sync) Some(Right) else None,
          comp,
          Some(enc)
        )
    }
  }

  /**
    *
    * @param s
    * @return
    */
  def decimalLength(s: String): (Int, Int) = {
    //get all the numbers in brackets i.e. 9(5)v9(2)
    val parts = s.split('V')
    val match1 = matcher.findFirstIn(parts.head.drop(1).dropRight(1))
    val match2 = matcher.findFirstIn(parts.last.drop(1).dropRight(1))
    // but 9(5)V99 is also valid so count 9's in each part
    val nines1 = parts.head.count(_ == '9').toString
    val nines2 = parts.last.count(_ == '9').toString
    //use the occurence else the number of 9's
    (match1.getOrElse(nines1).toInt, match2.getOrElse(nines2).toInt)
  }
}

/**
  *
  */
sealed trait CBTree {
  val counter: Iterator[Int] = (0 to 999).toArray.toIterator
  /**
    *
    */
  val camelCased: String = {
    if (name == "FILLER") {
      camelCase(parent.getOrElse(Group(1, "Root")).camelCased) + counter
        .next()
    } else camelCase(name)
  }
  /**
    *
    */
  val camelCaseVar: String =
    camelCased.updated(0, camelCased(0).toLower)

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try {
      op(p)
    } finally {
      p.close()
    }
  }

  def level: Int

  def name: String

  def parent: Option[Group]

  def redefines: Option[String]

  def occurs: Option[Int]

  def to: Option[Int]

  /**
    *
    * @return
    */
  def nextSiblingLeaf(): Option[Statement] = parent match {
    case Some(b) =>
      val me = b.children.indexOf(this)
      b.children.drop(me + 1).collectFirst {
        case l: Statement => l
      }
    case _ => None
  }

  /**
    *
    * @return
    */
  def path(): String = {
    @tailrec
    def addToPath(linage: String, tree: CBTree): String = {
      tree.parent match {
        case Some(branch) =>
          addToPath("/" + tree.name + linage, branch)
        case None =>
          "/" + tree.name + linage
      }
    }

    addToPath("", this)
  }

  /**
    *
    * @return
    */
  def up(): Option[CBTree] = this.parent

  /**
    *
    * @return
    */
  override def toString: String = {
    s"${" " * 2 * level}$camelCased ${camelCase(redefines.getOrElse(""))}"
    //s"${getClass.getName} -> name:$name, parent:$parent"
  }

  /**
    *
    * @param s
    * @return
    */
  def camelCase(s: String): String = {
    s.replace(".", "")
      .split("-")
      .map(c => c.toLowerCase.capitalize)
      .mkString
  }

  /**
    *
    * @return
    */
  def asScala: String = s"$camelCased $scalaType"

  /**
    *
    * @return
    */
  def scalaType: String

}

/**
  *
  * @param level
  * @param name
  * @param dataType
  * @param parent
  * @param redefines
  * @param occurs
  * @param to
  */
case class Statement(
                      level: Int,
                      name: String,
                      dataType: CobolType,
                      parent: Option[Group] = None,
                      redefines: Option[String] = None,
                      occurs: Option[Int] = None,
                      to: Option[Int] = None
                    )
  extends CBTree {
  /**
    *
    * @return
    */
  override def toString: String = {
    s"${" " * 2 * level}$camelCased ${camelCase(redefines.getOrElse(""))} $dataType"
  }

  /**
    *
    * @return
    */
  def scalaType: String = {
    val d = dataType match {
      case a: AlphaNumeric => "String"
      case a: Integer =>
        a.compact
          .collect {
            case 1 => "Float"
            case 2 => "Double"
            case 3 => "Long"
            case 4 => "Int"
          }
          .getOrElse("Long")
      case a: Decimal =>
        a.compact
          .collect {
            case 1 => "Float"
            case 2 => "Double"
            case 3 => "Double"
            case 4 => "Double"
          }
          .getOrElse("Double")
    }
    if (occurs.isDefined) s":Array[$d]" else s":$d"
  }
}

/**
  *
  * @param level
  * @param name
  * @param children
  * @param parent
  * @param redefines
  * @param occurs
  * @param to
  */
case class Group(
                  level: Int,
                  name: String,
                  children: mutable.ArrayBuffer[CBTree] = mutable.ArrayBuffer(),
                  parent: Option[Group] = None,
                  redefines: Option[String] = None,
                  occurs: Option[Int] = None,
                  to: Option[Int] = None
                )
  extends CBTree {

  /**
    *
    * @return
    */
  def asCaseClass: String = {
    mutable.StringBuilder.newBuilder
      .appendAll(s"case class $camelCased (")
      .appendAll(
        children.map(c => c.camelCaseVar + c.scalaType).mkString(", ")
      )
      .append(')')
      .toString()
  }

  /**
    *
    * @param path
    * @return
    */
  def get(path: String): Option[CBTree] = {
    var parts = path.split("/").filter(_ != "")
    parts = if (parts.head == name) parts.drop(1) else parts
    val instance = parts.last
    parts = parts.dropRight(1)

    val directParent = parts.foldLeft(Option(this: CBTree))((c, p) => {
      c.flatMap {
        case b: Group =>
          b.children.collectFirst {
            case b: Group if b.name == p => b
          }
        case _ => None
      }
    })
    directParent.flatMap {
      case b: Group =>
        b.children.collectFirst {
          case l: CBTree if l.name == instance => l
        }
      case _ => None
    }
  }

  /**
    *
    * @return
    */
  def traverseStatements: Seq[Statement] = {
    def dfs(r: Group): Seq[Statement] = {
      if (r.children.nonEmpty) {
        r.children.flatMap {
          case g: Group => dfs(g)
          case s: Statement => Seq(s)
        }
      } else if (r.parent.isDefined) dfs(r.parent.get)
      else Nil
    }

    dfs(this)
  }

  /**
    *
    * @return
    */
  def traverseGroups: Seq[Group] = {
    val comb: mutable.ArrayBuffer[Group] = mutable.ArrayBuffer[Group](this)

    // Todo @tailrec
    def dfs(r: Group): Unit = {
      if (r.children.nonEmpty) {
        r.allChildGroups().foreach { g =>
          comb += g
          dfs(g)
        }
      }
    }

    dfs(this)
    comb
  }

  /**
    *
    * @return
    */
  def allChildGroups(): Seq[Group] = {
    children.collect {
      case b: Group => b
    }
  }

  /**
    *
    * @return
    */
  def traverseAll: Seq[CBTree] = {
    val comb: mutable.ArrayBuffer[CBTree] = mutable.ArrayBuffer[CBTree](this)

    // Todo @tailrec
    def dfs(r: Group): Unit = {
      if (r.children.nonEmpty) {
        r.children.foreach {
          case g: Group =>
            comb += g
            dfs(g)
          case s: Statement => comb += s
        }
      } else if (r.parent.isDefined) {
        dfs(r.parent.get)
      } else Nil
    }

    dfs(this)
    comb
  }

  /**
    *
    * @param data
    * @return
    */
  def parseData(data: String): (String, Seq[Any]) = {
    val d = BitVector(data.getBytes)
    // Todo

    traverseAll.map {
      case l: Statement =>
        (l.dataType.getClass.getSimpleName, l.dataType match {
          case a: AlphaNumeric => a.length
          case a: Decimal =>
            val l = a.precision + a.scale + (if (a.signPosition.isDefined) 1 else 0)
            if (a.compact.getOrElse(1) == 3) l / 2 + 1 else l
          case a: Integer =>
            val l = a.scale + (if (a.signPosition.isDefined) 1 else 0)
            if (a.compact.getOrElse(1) == 3) l / 2 + 1
            else if (a.compact.getOrElse(1) == 4) 4
            else l
        })
      case g: Group => g.parseData(data)
    }

    // Todo
    ("", Nil)
  }

  /**
    *
    * @param tree
    * @tparam T
    * @return
    */
  def add[T <: CBTree](tree: T): CBTree = {
    val c = tree.getClass.getSimpleName match {
      case "Group" =>
        Group(
          tree.level,
          tree.name,
          children = mutable.ArrayBuffer[CBTree](),
          parent = Some(this),
          redefines = tree.redefines
        )
      case "Statement" =>
        Statement(
          tree.level,
          tree.name,
          dataType = tree.asInstanceOf[Statement].dataType,
          parent = Some(this),
          redefines = tree.asInstanceOf[Statement].redefines,
          occurs = tree.asInstanceOf[Statement].occurs,
          to = tree.asInstanceOf[Statement].to
        )
    }
    children += c
    c
  }

  /**
    *
    * @return
    */
  def scalaType: String =
    if (occurs.isDefined) s":Array[$camelCased]" else s":$camelCased"

}

sealed abstract class ParseResult[+T]

case class ParseSuccess[T](result: T, remainder: BitVector) extends ParseResult[T]

case class ParseError[T](result: T, remainder: BitVector) extends ParseResult[Nothing]

object Files extends LazyLogging{
  /**
    *
    * @param dir : Files directory
    * @return
    */
  def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  def createCaseClasses(roots: Seq[Group], packageName: String = "com.zenaptix.test") = {
    roots.foreach { root =>
      logger.info(s"Creating case classes for root : ${root.name}")
      val c = root.traverseGroups.map(g => {
        logger.info("group : " + g)
        logger.info("group.children " + g.children)
        logger.info("g.asCaseClass" + g.asCaseClass)
        g.asCaseClass
      })
      root.printToFile(new File(s"src/main/scala/${packageName.replace(".", "/")}/${root.camelCased}.scala")) { p =>
        p.println(s"package $packageName")
        c.foreach(p.println)
      }
    }
  }

  def copyBytes(fileName: String): BitVector = {
    var vec = BitVector.empty
    var in = None: Option[FileInputStream]
    var out = None: Option[FileOutputStream]
    try {
      in = Some(new FileInputStream(fileName))
      //    out = Some(new FileOutputStream("/tmp/Test.class.copy"))
      var c = 0
      while ( {
        c = in.get.read()
        c != -1
      }) {
        //      println(c)
        vec = vec ++ BitVector(c.toByte)
      }
    } catch {
      case e: IOException => BitVector.empty
    } finally {
      if (in.isDefined) in.get.close
      //    if (out.isDefined) out.get.close
    }
    vec
  }

  def wordAlign(f: BitVector, wordSize: Int, align: Position): BitVector = {
    require(f.size <= wordSize)
    align match {
      case Left if f.size != wordSize => f.padLeft(wordSize - f.size)
      case Right if f.size != wordSize => f.padRight(wordSize - f.size)
      case _ => f
    }
  }

  def getBitCount(codec: Codec[_ <: AnyVal], comp: Option[Int], scale: Int) = {
    comp match {
      case Some(x) => {
        x match {
          case a if a == 3 => (scale + 1) * codec.sizeBound.lowerBound.toInt //bcd
          case _ => codec.sizeBound.lowerBound.toInt // bin/float/floatL
        }
      }
      case None => scale * codec.sizeBound.lowerBound.toInt
    }
  }

  def decode(codec: Codec[_ <: AnyVal], enc: Encoding, scale: Int, bits: BitVector, comp: Option[Int], align: Option[Position] = None, signPosition: Option[Position]): Array[Byte] = {
    val digitBitSize = codec.sizeBound.lowerBound.toInt
    val bytes = enc match {
      case asc: ASCII => comp match {
        case Some(compact) => compact match {
          case a if a == 3 => { //bcd
            val bte = for (x <- 0 until scale) yield {
              val bts = wordAlign(bits.slice(x * digitBitSize, (x * digitBitSize) + digitBitSize), digitBitSize, align.getOrElse(Left))
              Codec.decode(bts)(codec).require.value.asInstanceOf[Double].toByte
            }
            bte.toArray
          }
          case _ => { //bin
            //            val bts = wordAlign(bits, digitBitSize, align.getOrElse(Left))
            val bte = Codec.decode(bits)(codec).require.value.asInstanceOf[Double].toByte
            (bte :: Nil).toArray
          }
        }
        case None => { // display i.e. no comp
          val bte = for (x <- 0 until scale) yield {
            val bts = wordAlign(bits.slice(x * digitBitSize, (x * digitBitSize) + digitBitSize), digitBitSize, align.getOrElse(Left))
            Codec.decode(bts)(codec).require.value.asInstanceOf[Double].toByte
          }
          bte.toArray
        }
      }
      case ebc: EBCDIC => comp match {
        case Some(compact) => compact match {
          case a if a == 3 => { //bcd
            logger.info("BCD")
            logger.info("bits : " + bits)
            val bte = for (x <- 0 to scale) yield {
              val bts = wordAlign(bits.slice(x * digitBitSize, (x * digitBitSize) + digitBitSize), digitBitSize, align.getOrElse(Left))
              //              println("bts : " + bts.toBin)
              //              println("codec : " + codec)
              //              println("value : " + Codec.decode(bts)(codec).require.value.asInstanceOf[Number].doubleValue())
              Codec.decode(bts)(codec).require.value.asInstanceOf[Number].doubleValue().toByte
            }
            bte.toArray
          }
          case _ => { //bin
            //            val bts = wordAlign(bits, digitBitSize, align.getOrElse(Left))
            val buf = ByteBuffer.allocate(8)
            val decValue = Codec.decode(bits)(codec).require.value.asInstanceOf[Number].doubleValue()
            val byteArr = buf.putDouble(decValue).array()
            byteArr
          }
        }
        case None => { // display i.e. no comp
          val bte = for (x <- 0 until scale) yield {
            val bts = wordAlign(bits.slice(x * digitBitSize, (x * digitBitSize) + digitBitSize), digitBitSize, align.getOrElse(Left))
            logger.info("bts : " + bts.toBin)
            Codec.decode(bts)(codec).require.value.asInstanceOf[Number].doubleValue().toByte
          }
          bte.toArray
        }
      }
    }
    bytes
  }

  def charDecode(byteArr: Array[Byte], enc: Option[Encoding], comp: Option[Int]) = {
    val ans = enc match {
      case Some(ASCII()) => byteArr.map(byte => {
        byte.toInt
      })
      case Some(EBCDIC()) =>
        val finalStringVal = comp match {
          case Some(compact) => {
            val compValue = compact match {
              case a if a == 3 => { //bcd
                val digitString = for {
                  idx <- byteArr.indices
                } yield {
                  if (idx == byteArr.length - 1) { //last byte is sign
                    byteArr(idx) match {
                      case 0x0C => "+"
                      case 0x0D => "-"
                      case 0x0F => "+" //unsigned
                      case _ => byteArr(idx).toString // No sign
                    }
                  }
                  else {
                    byteArr(idx).toString
                  }
                }
                logger.info("digitString : " + digitString)
                s"${digitString.last}${digitString.head}${digitString.tail.dropRight(1).mkString("")}"
              }
              case _ => { //bin
                val buf = ByteBuffer.wrap(byteArr)
                buf.flip()
                buf.getDouble.toString //returns number value as a string "1500"
                //                buf.clear()
              }
            }
            compValue
          }
          case None => {
            //display
            val digitString = for {
              idx <- byteArr.indices
            } yield {
              //byte => ebcidic character
              val cobolChar = byteArr(idx).toInt match { //change hex to int
                case -63 => "A"
                case -62 => "B"
                case -61 => "C"
                case -60 => "D"
                case -59 => "E"
                case -58 => "F"
                case -57 => "G"
                case -56 => "H"
                case -55 => "I"
                case -47 => "J"
                case -46 => "K"
                case -45 => "L"
                case -44 => "M"
                case -43 => "N"
                case -42 => "O"
                case -41 => "P"
                case -40 => "Q"
                case -39 => "R"
                case -30 => "S"
                case -29 => "T"
                case -28 => "U"
                case -27 => "V"
                case -26 => "W"
                case -25 => "X"
                case -24 => "Y"
                case -23 => "Z"
                case -16 => "0"
                case -15 => "1"
                case -14 => "2"
                case -13 => "3"
                case -12 => "4"
                case -11 => "5"
                case -10 => "6"
                case -9 => "7"
                case -8 => "8"
                case -7 => "9"
                case _ => " "
              }
              cobolChar
            }
            digitString.mkString("")
          }
        }
        finalStringVal

      case None => throw new Exception("No character set was defined for decoding")
    }
    ans.toString
  }

  def getScalaType(typeString: String) = {
    typeString match {
      case "string" => Class.forName(s"java.lang.${typeString.capitalize}")
      case _ => Class.forName(s"scala.${typeString.capitalize}")
    }
  }

  def stringToTypeTag[A](name: String): TypeTag[A] = {
    val c = name match {
      case "string" => Class.forName(s"java.lang.${name.capitalize}")
      case _ => Class.forName(s"scala.${name.capitalize}")
    }
    val mirror = runtimeMirror(c.getClassLoader) // obtain runtime mirror
    val sym = mirror.staticClass(c.getName) // obtain class symbol for `c`
    val tpe = sym.selfType // obtain type object for `c`
    // create a type tag which contains above type object
    TypeTag(mirror, new api.TypeCreator {
      def apply[U <: api.Universe with Singleton](m: api.Mirror[U]) =
        if (m eq mirror) tpe.asInstanceOf[U#Type]
        else throw new IllegalArgumentException(s"Type tag defined in $mirror cannot be migrated to other mirrors.")
    })
  }

  def cast[A](a: Any, tt: TypeTag[A]): A = a.asInstanceOf[A]


  def recursiveBuilder(root: CBTree, roots: Seq[CBTree], origRec: GenericData.Record, values: Iterator[HList]): GenericData.Record = {
    val fields = origRec.getSchema.getFields
    logger.info("FIELDS REC : " + fields)
    val fieldsList = fields.toArray.toList.map(field => {
      field.toString.split(" ").toList.head
    })
    logger.info("fields List : " + fieldsList)
    var newRec = origRec
    if (fields.size() > 0) { //root has child
      fieldsList.foreach(fieldName => {
        logger.info("FOR EACH CHILD FIELD : " + fieldName)
        val childField = origRec.getSchema.getField(fieldName)
        logger.info("origRec.getSchema.getField(fieldName)/childField " + childField)
        logger.info("origRec.getSchema.getField(fieldName).schema/childField.schema : " + childField.schema())
        val isLeaf = Try {
          childField.schema().getFields
        } match {
          case Success(ans) =>
            val newFieldRec = new GenericData.Record(childField.schema())
            newRec = recursiveBuilder(root, roots, newFieldRec, values)
            false
          case Failure(e) =>
            logger.info("ERROR : " + e)
            logger.info("else origRec.getSchema.getName " + origRec.getSchema.getName)
            logger.info("Put " + fieldName + " IN " + origRec.getSchema.getName)
            val fieldVal = values.next() match {
              case h :: HNil => h
              case _ => println("&&&!!!!!")
            }
            val fieldType = origRec.getSchema.getField(fieldName).schema().getType
            val stringToType = stringToTypeTag(fieldType.getName)
            origRec.put(fieldName, cast(fieldVal, stringToType))
            logger.info("origRec put : " + origRec.toString)
            true
        }
        if (!isLeaf) {
          logger.info("group put " + fieldName + " IN " + origRec.getSchema.getName)
          logger.info(s"origRec.put &&&& (${fieldName} , ${childField.toString})")
          logger.info("NEW REC :  " + newRec.toString)
          origRec.put(fieldName, newRec)
          logger.info("origRec put : " + origRec.toString)
        }
      })
      logger.info("origRec IF before exit" + origRec.toString)
      origRec
    }
    else {
      logger.info("Put " + root.camelCaseVar + " IN " + origRec.getSchema.getName)
      val fieldVal = values.next() match {
        case h :: HNil => h
        case _ => println("&&&!!!!!")
      }
      val fieldType = origRec.getSchema.getField(root.camelCaseVar).schema().getType
      val stringToType = stringToTypeTag(fieldType.getName)
      logger.info("FIELD VAL !!!! ELSE : " + fieldVal.toString)
      origRec.put(root.camelCaseVar, cast(fieldVal, stringToType))
      logger.info("origRec put ELSE : " + origRec.toString)
      origRec
    }
  }

  def rawDataList(fileOffset: Long, f: BitVector, schema: Schema, forest: Seq[Group]): Seq[(List[HList], Long)] = {
    forest.map(tree => {
      val roots: Seq[CBTree] = tree.traverseAll
      var fileIdx = fileOffset
      (roots.map(root => {
        root match {
          case y: Statement => {
            y.dataType match {
              case a: AlphaNumeric => {
                //each character is represented by a byte
                val codec = a.enc.getOrElse(EBCDIC()).codec(None, a.length, None)
                logger.info("AlphaCodec : " + codec)
                val bitCount = getBitCount(codec, None, a.length) //count of entire word
                val bits = f.slice(fileIdx, fileIdx + bitCount) // cut out word form binary file
                val padded: Array[Byte] = decode(codec, a.enc.getOrElse(EBCDIC()), a.length, bits, None, a.wordAlligned, None)
                val ans = charDecode(padded, a.enc, None)
                fileIdx = fileIdx + bitCount.toInt
                ans :: HNil
              }
              case d: Decimal => {
                val codec = d.enc.getOrElse(EBCDIC()).codec(d.compact, d.scale, d.signPosition)
                logger.info("DecCodec : " + codec)
                val bitCount = getBitCount(codec, d.compact, d.scale)
                val bits = f.slice(fileIdx, fileIdx + bitCount)
                val padded: Array[Byte] = decode(codec, d.enc.getOrElse(EBCDIC()), d.scale, bits, d.compact, d.wordAlligned, d.signPosition)
                val ans = charDecode(padded, d.enc, d.compact)
                fileIdx = fileIdx + bitCount.toInt
                //                println("value : " + ans.mkString("-"))
                ans.toFloat :: HNil
              }
              case i: Integer => {
                val codec = i.enc.getOrElse(EBCDIC()).codec(i.compact, i.scale, i.signPosition)
                logger.info("IntCodec : " + codec)
                val bitCount = getBitCount(codec, i.compact, i.scale)
                val bits = f.slice(fileIdx, fileIdx + bitCount)
                val padded: Array[Byte] = decode(codec, i.enc.getOrElse(EBCDIC()), i.scale, bits, i.compact, i.wordAlligned, i.signPosition)
                val ans = charDecode(padded, i.enc, i.compact)
                fileIdx = fileIdx + bitCount.toInt
                ans.toDouble :: HNil
              }
            }
          }
          case _ => {
            logger.info("group")
            HNil
          }
        }
      }).toList, fileIdx)
    })
  }

  def writeValues2File(values: List[HList], fileName: String): Unit = {
    import java.io.FileOutputStream
    import java.io.PrintWriter

    val savestr = s"$fileName"
    val f = new File(savestr)

    val out =
      if (f.exists && !f.isDirectory)
        new PrintWriter(new FileOutputStream(new File(savestr), true))
      else
        new PrintWriter(savestr)

    values.foreach(value => value match {
      case head :: HNil =>
        out.append(head.toString)
        if (value == values.last)
          out.append("\n")
        else
          out.append(",")
      case _ => ???
    })
    //    out.append(mapstring)
    out.close
  }
}