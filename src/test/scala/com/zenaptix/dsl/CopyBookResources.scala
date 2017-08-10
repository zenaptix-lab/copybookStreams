package com.zenaptix.dsl

import scala.io.{BufferedSource, Source}

/**
  * Created by rikus on 8/10/17.
  */
object CopyBookResources {
  val cpyBook: String =
    """      01   PKLR1-DETAIL-LOAN-RECORD.
      |      10  PKLR1-BASIC-SECTION.
      |      * 10  PKLR1-BASIC-SECTION.
      |      * 10  PKLR1-BASIC-SECTION.
      |       20  PKLR1-SORT-CONTROL-FIELD.
      |           30  PKLR1-USER-IDENT         PIC X(1).
      |           30  PKLR1-EXTRACT-CODE
      |               PIC X(1).
      |               88  PKLR1-DATE-RECORD            VALUE '*'.
      |               88  PKLR1-DATA-RECORD            VALUE '0'.
      |               88  PKLR1-END-OF-FILE            VALUE '9'.
      |           30  PKLR1-SECTION            PIC X(1).
      |           30  PKLR1-TYPE               PIC X(1).
      |           30  PKLR1-NUMERIC-STATE-CODE PIC X(2).
      |           30  PKLR1-CONTRACT-NUMBER    PIC X(10).
      |       20  PKLR1-PAR-PEN-REG-CODE       PIC X(1).
      |       20  PKLR1-VALUATION-CODE.
      |           30  PKLR1-MORTALITY-TABLE    PIC X(2).
      |           30  PKLR1-LIVES-CODE         PIC X(1).
      |           30  PKLR1-FUNCTION           PIC X(1).
      |           30  PKLR1-VAL-INTEREST       PIC S9(2)V9(3) COMP-3.
      |           30  PKLR1-MODIFICATION       PIC X(1).
      |           30  PKLR1-INSURANCE-CLASS    PIC X(1).
      |           30  PKLR1-SERIES             PIC X(5).
      |       20  PKLR1-POLICY-STATUS          PIC X(2).
      |       20  PKLR1-PAR-CODES.
      |           30  PKLR1-PAR-TYPE           PIC X(1).
      |           30  PKLR1-DIVIDEND-OPTION    PIC X(1).
      |           30  PKLR1-OTHER-OPTION       PIC X(1).
      |       20  PKLR1-ALPHA-STATE-CODE       PIC X(2).""".stripMargin

  val cpyBook2: String =
    """      01   PKLR1-DETAIL-LOAN-RECORD.
      |      10  PKLR1-BASIC-SECTION.
      |      * 10  PKLR1-BASIC-SECTION.
      |      * 10  PKLR1-BASIC-SECTION.
      |       20  PKLR1-SORT-CONTROL-FIELD.
      |           30  PKLR1-USER-IDENT         PIC X(1).
      |           30  PKLR1-EXTRACT-CODE
      |               PIC X(1).
      |               88  PKLR1-DATE-RECORD            VALUE '*'.
      |               88  PKLR1-DATA-RECORD            VALUE '0'.
      |               88  PKLR1-END-OF-FILE            VALUE '9'.
      |           30  PKLR1-SECTION            PIC X(1).
      |           30  PKLR1-TYPE               PIC X(1).
      |           30  PKLR1-NUMERIC-STATE-CODE PIC X(2).
      |           30  PKLR1-CONTRACT-NUMBER    PIC X(10).
      |       20  PKLR1-PAR-PEN-REG-CODE       PIC X(1).
      |       20  PKLR1-VALUATION-CODE.
      |           30  PKLR1-MORTALITY-TABLE    PIC X(2).
      |           30  PKLR1-LIVES-CODE         PIC X(1).
      |           30  PKLR1-FUNCTION           PIC X(1).
      |           30  PKLR1-VAL-INTEREST       PIC S9(2)V9(3) COMP-3.
      |           30  PKLR1-MODIFICATION       PIC X(1).
      |           30  PKLR1-INSURANCE-CLASS    PIC X(1).
      |           30  PKLR1-SERIES             PIC X(5).
      |       20  PKLR1-POLICY-STATUS          PIC X(2).
      |       20  PKLR1-PAR-CODES.
      |           30  PKLR1-PAR-TYPE           PIC X(1).
      |           30  PKLR1-DIVIDEND-OPTION    PIC X(1).
      |           30  PKLR1-OTHER-OPTION       PIC X(1).
      |       20  PKLR1-ALPHA-STATE-CODE       PIC X(2).
      |      01   PKLR1-DETAIL-LOAN-RECORD-COPY.
      |      10  PKLR1-BASIC-SECTION-COPY.
      |       20  PKLR1-SORT-CONTROL-FIELD-COPY.
      |           30  PKLR1-USER-IDENT-COPY         PIC X(1).
      |       """.stripMargin

  val s1: String = """case class PKLR1DetailLoanRecord(val pKLR1SortControlField: PKLR1SortControlField)"""

  //val source: BufferedSource = Source.fromFile("src/test/resources/Cheques_Master.txt")
  val source: BufferedSource = Source.fromFile("src/test/resources/SVSE258.txt")
  val lines: String = try source.getLines().mkString("\n") finally source.close()

  //println(s"lines is $lines")

  val roots: Seq[Group] = CopyBookSchema(lines).parseTree(ASCII())
  //  val roots: Seq[Group] = CopyBookSchema(cpyBook).parseTree()
  println("Roots :  " + roots.mkString("\n"))

  val filesDir = Files.getListOfFiles("./src/test/resources")
  val txtFiles = filesDir.filter(file => file.toString.contains(".txt")).map(file => file.toString)
}
