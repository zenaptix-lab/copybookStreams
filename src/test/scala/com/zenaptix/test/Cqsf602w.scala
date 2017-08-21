package com.zenaptix.test
case class Cqsf602w (cqnf602wStlnCommonDtls:Cqnf602wStlnCommonDtls, cqnf602StudentLoanDtl:Cqnf602StudentLoanDtl)
case class Cqnf602wStlnCommonDtls (cqnf602wKey:Cqnf602wKey, cqnf602wCifk:Cqnf602wCifk)
case class Cqnf602wKey (cqnf602wAcctNo:Long)
case class Cqnf602wCifk (cqnf602wCifkey:String)
case class Cqnf602StudentLoanDtl (stnof602:String, crsef602:String, normf602:Long, instf602:Long, acyrf602:Long, rpdtf602:Long, failf602:Long, compf602:String, repyf602:String, adfnf602:String, fultf602:String, pgrdf602:String, finlf602:String, spare:String)
