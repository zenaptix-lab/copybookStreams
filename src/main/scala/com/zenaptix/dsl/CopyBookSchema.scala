package com.zenaptix.dsl

import java.io.{File, FileInputStream, FileOutputStream, IOException}

import org.apache.avro.Schema
import org.apache.avro.generic.GenericData
import scodec.bits.BitVector

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.matching.Regex

import scodec._
import scodec.bits._
import codecs._

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
  def codec[A](comp:Option[Int]):Codec[A]
}

/**
 *
 */
case class ASCII() extends Encoding{
 def codec[A](comp:Option[Int]):Codec[A] = {
   val cd = comp match {
     case Some(x) if x.isInstanceOf[Int] => Files.getCodec(comp)
     case None => uint8
   }
   cd.asInstanceOf[Codec[A]]
 }
}

/**
 *
 */
case class EBCDIC() extends Encoding{
  def codec[A](comp:Option[Int]):Codec[A] = {
    val cd = comp match {
      case Some(x) if x.isInstanceOf[Int] => uint8
      case None => uint8
    }
    cd.asInstanceOf[Codec[A]]
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
case class CopyBookSchema(cpyBook: String) {
  val matcher: Regex = "\\(([^)]+)\\)".r

  /**
   *
   * @return
   */
  def parseTree(enc:Encoding): Seq[Group] = {
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
    println("breakpoints : " + breakpoints.mkString(";"))

    val forest: Seq[Seq[((Int, String), Map[String, String])]] = breakpoints.map(p => lines.slice(p._2._1, p._2._2)) //forest should only have multiple items if there is a duplicate level
    println("FOREST : " + forest.mkString("\n"))

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
              if (isLeaf) {
                val t = typeAndLengthFromString(keywords, f._2)(enc)
                u.add(
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
                u.add(
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
      .map(l => { l.trim().split("\\s+") })

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

  /* def genSchema(tokenMap:Map[String,(String,Int)]):String = {
    ""
  }*/

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
  )(enc:Encoding): CobolType = {
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
        AlphaNumeric(s.length, wordAlligned = if (sync) Some(Left) else None,Some(enc))
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
    try { op(p) } finally { p.close() }
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

object Files {
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

  def copyBytes(fileName: String): BitVector = {
    var vec = BitVector.empty
    var in = None: Option[FileInputStream]
    var out = None: Option[FileOutputStream]
    try {
      in = Some(new FileInputStream(fileName))
      //    out = Some(new FileOutputStream("/tmp/Test.class.copy"))
      var c = 0
      while ({
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

  def wordAlign(f: BitVector, wordSize: Int, pos: Position): BitVector = {
    require(f.size <= wordSize)
    pos match {
      case Left if (f.size != wordSize) => f.padLeft(wordSize - f.size)
      case Right if (f.size != wordSize) => f.padRight(wordSize - f.size)
      case _ => f
    }
  }

  def getCodec(compac: Option[Int]): Codec[Int] = {
    compac match {
      case Some(x) if x == 1 || x == 4 => {
        //normal binary decoding
        uint(1)
      }
      case None => uint(1)
      case _ => {
        //bcd encoding
        uint4
      }
    }
  }

  def getBitCount(codec: Codec[Int], scale: Int, precision: Int = 0) = {
    scale match { //bitCount for the number of digits
      case a if a >= 1 && a <= 4 && !codec.equals(uint4) => {
        2 * 8
      }
      case b if b >= 5 && b <= 9 && !codec.equals(uint4) => {
        4 * 8
      }
      case c if c > 0 && codec.equals(uint4) => {
        (c + precision) * 4
      }
      case _ => {
        println("NO MATCH")
        8 * 8
      }
    }
  }

  def decode(codec: Codec[Int], range: Long, bits: BitVector, wordAllign: Option[Position] = Some(Right)): Array[Byte] = codec match {
    case a if a.equals(uint4) => {
      val b = for (x <- 0 until range.toInt) yield {
        val bts = wordAlign(bits.slice(x * 4, (x * 4) + 4), 4, Right)
        Codec.decode(bts)(codec).require.value.toByte
      }
      b.toArray
    }
    case _ => bits.toByteArray
  }
  /**
   *
   * @param f
   * @param schema : Avro schema
   * @param forest : Forest of ASTs'
   * @return
   */
  def rawDataParse(f: BitVector, schema: Schema, forest: Seq[Group]) = {
    forest.map(tree => {
      val roots = tree.traverseAll
      val genRec = new GenericData.Record(schema)
      var fileIdx = 0
      roots.foldLeft(genRec)((genRecAcc, root) => {
        root match {
          case x: Group => {
            //          println("Group : " + x.camelCaseVar)
            if (x.level == 1) {
              genRec
            } else {
              val grpRec = new GenericData.Record(schema.getField(x.camelCaseVar).schema())
              grpRec
            }
          }
          case y: Statement => {
            //          println("Statement : " + y.camelCaseVar)
            val value = y.dataType match {
              case a: AlphaNumeric => { //todo: Here the hardcoded values need to be fetch from flat file, text encoding will determine actual value.
                val codec = a.enc.getOrElse(ASCII()).codec[Int](Some(8))
                println("AlphaCodec : " + codec)
                val bitCount = a.length * 8
                val bits = f.slice(fileIdx, fileIdx + bitCount)
                val range = codec match {
                  case a if a.equals(uint8) => bits.size / 8
                  case _ => bits.size / 8
                }
                val padded: Array[Byte] = decode(codec, range, bits)
                val ans = a.enc match {
                  case Some(ASCII()) => padded.map(byte => {
                    byte.toInt
                  })
                  case Some(EBCDIC()) => padded.map(byte => {
                    byte.toInt
                  })
                }
                fileIdx = fileIdx + bitCount.toInt
                //                println("value : " + ans.mkString("-"))
                ans
              }
              case d: Decimal => {
                println(y.dataType)
                val codec = d.enc.getOrElse(ASCII()).codec[Int](d.compact)
                println("DecCodec : " + codec)
                val bitCount: Long = getBitCount(codec, d.scale, d.precision)
                val bits = f.slice(fileIdx, fileIdx + bitCount)
                val range = codec match {
                  case a if a.equals(uint4) => bits.size / 4
                  case _ => bits.size / 8
                }
                val padded: Array[Byte] = decode(codec, range, bits)
                val ans = d.enc match {
                  case Some(ASCII()) => padded.map(byte => {
                    byte.toInt
                  })
                  case Some(EBCDIC()) => padded.map(byte => {
                    byte.toInt
                  })
                }
                fileIdx = fileIdx + bitCount.toInt
                //                println("value : " + ans.mkString("-"))
                ans
              }
              case i: Integer => {
                println(y.dataType)
                val codec = i.enc.getOrElse(ASCII()).codec[Int](i.compact)
                println("IntCodec : " + codec)
                val bitCount: Long = getBitCount(codec, i.scale)
                val bits = f.slice(fileIdx, fileIdx + bitCount)
                val range = codec match {
                  case a if codec.equals(uint4) => bits.size / 4
                  case _ => bits.size / 8
                }
                val padded: Array[Byte] = decode(codec, range, bits)
                val ans = i.enc match {
                  case Some(ASCII()) => padded.map(byte => {
                    byte.toInt
                  })
                  case Some(EBCDIC()) => padded.map(byte => {
                    byte.toInt
                  })
                }
                fileIdx = fileIdx + bitCount.toInt
                ans
              }
            }
            genRecAcc.put(y.camelCaseVar, value.mkString(",")) // value is an array at this point. Textified the payload for readability
            //            genRecAcc.put(y.camelCaseVar, value)
            genRecAcc
          }
        }
      })
    })
  }
}