import java.io.File

import org.apache.avro.SchemaBuilder

//val schema =
//  """ {
//    |  "type" : "record",
//    |  "name" : "Mbsk861",
//    |  "namespace" : "com.zenaptix.dsl",
//    |  "fields" : [ {
//    |    "name" : "lngth861",
//    |    "type" : "int"
//    |  }, {
//    |    "name" : "pkeyk861",
//    |    "type" : {
//    |      "type" : "record",
//    |      "name" : "Pkeyk861",
//    |      "fields" : [ {
//    |        "name" : "uniqEftNoK861",
//    |        "type" : "string"
//    |      } ]
//    |    }
//    |  }, {
//    |    "name" : "datak861",
//    |    "type" : {
//    |      "type" : "record",
//    |      "name" : "Datak861",
//    |      "fields" : [ {
//    |        "name" : "incrPaymNoK861",
//    |        "type" : "long"
//    |      }, {
//    |        "name" : "tbcK861",
//    |        "type" : "string"
//    |      } ]
//    |    }
//    |  } ]
//    |}
//    |""".stripMargin
//
//val schema2 =
//  """{
//    |  "type" : "record",
//    |  "name" : "Mbsk862",
//    |  "namespace" : "com.zenaptix.dsl",
//    |  "fields" : [ {
//    |    "name" : "lngth862",
//    |    "type" : "int"
//    |  }, {
//    |    "name" : "pkeyk862",
//    |    "type" : {
//    |      "type" : "record",
//    |      "name" : "Pkeyk862",
//    |      "fields" : [ {
//    |        "name" : "paymNoK862",
//    |        "type" : "long"
//    |      } ]
//    |    }
//    |  }, {
//    |    "name" : "datak862",
//    |    "type" : {
//    |      "type" : "record",
//    |      "name" : "Datak862",
//    |      "fields" : [ {
//    |        "name" : "statusK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "paymDateK862",
//    |        "type" : "long"
//    |      }, {
//    |        "name" : "paymTimeK862",
//    |        "type" : "long"
//    |      }, {
//    |        "name" : "effDateK862",
//    |        "type" : "long"
//    |      }, {
//    |        "name" : "effTimeK862",
//    |        "type" : "long"
//    |      }, {
//    |        "name" : "retryCountK862",
//    |        "type" : "long"
//    |      }, {
//    |        "name" : "settleIndK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "paymTypeIndK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "immedBusDirK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "intExtBenIndK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "srcAccNoK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "srcAccTypeK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "srcClrCodeK862",
//    |        "type" : "long"
//    |      }, {
//    |        "name" : "srcInstCodeK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "srcStmtRefK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "trgAccNoK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "trgAccTypeK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "trgClrCodeK862",
//    |        "type" : "long"
//    |      }, {
//    |        "name" : "trgInstCodeK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "trgBusCodeK862",
//    |        "type" : "long"
//    |      }, {
//    |        "name" : "trgStmtRefK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "reqAmntK862",
//    |        "type" : "double"
//    |      }, {
//    |        "name" : "processAmntK862",
//    |        "type" : "double"
//    |      }, {
//    |        "name" : "errCodeK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "routErrCodeK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "routErrCatK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "channK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "iipIndK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "iipAuthK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "busRefK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "wdrNoK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "echoDataK862",
//    |        "type" : {
//    |          "type" : "record",
//    |          "name" : "EchoDataK862",
//    |          "fields" : [ {
//    |            "name" : "redemptionDetailK862",
//    |            "type" : {
//    |              "type" : "record",
//    |              "name" : "RedemptionDetailK862",
//    |              "fields" : [ {
//    |                "name" : "redemptionDtlsK862",
//    |                "type" : {
//    |                  "type" : "record",
//    |                  "name" : "RedemptionDtlsK862",
//    |                  "fields" : [ {
//    |                    "name" : "pinOffsetK862",
//    |                    "type" : "string"
//    |                  }, {
//    |                    "name" : "redAtmK862",
//    |                    "type" : "string"
//    |                  }, {
//    |                    "name" : "redDateK862",
//    |                    "type" : "long"
//    |                  }, {
//    |                    "name" : "redTimeK862",
//    |                    "type" : "long"
//    |                  } ]
//    |                }
//    |              }, {
//    |                "name" : "billingDtls1K862",
//    |                "type" : {
//    |                  "type" : "record",
//    |                  "name" : "BillingDtls1K862",
//    |                  "fields" : [ {
//    |                    "name" : "resCanDate1K862",
//    |                    "type" : "long"
//    |                  }, {
//    |                    "name" : "resetCnt1K862",
//    |                    "type" : "long"
//    |                  }, {
//    |                    "name" : "resendCnt1K862",
//    |                    "type" : "long"
//    |                  }, {
//    |                    "name" : "cancelCnt1K862",
//    |                    "type" : "long"
//    |                  } ]
//    |                }
//    |              }, {
//    |                "name" : "billingDtls2K862",
//    |                "type" : {
//    |                  "type" : "record",
//    |                  "name" : "BillingDtls2K862",
//    |                  "fields" : [ {
//    |                    "name" : "resCanDate2K862",
//    |                    "type" : "long"
//    |                  }, {
//    |                    "name" : "resetCnt2K862",
//    |                    "type" : "long"
//    |                  }, {
//    |                    "name" : "resendCnt2K862",
//    |                    "type" : "long"
//    |                  }, {
//    |                    "name" : "cancelCnt2K862",
//    |                    "type" : "long"
//    |                  } ]
//    |                }
//    |              }, {
//    |                "name" : "redemptiondetailk8620",
//    |                "type" : "string"
//    |              } ]
//    |            }
//    |          }, {
//    |            "name" : "immedBusDirDetK862",
//    |            "type" : {
//    |              "type" : "record",
//    |              "name" : "ImmedBusDirDetK862",
//    |              "fields" : [ {
//    |                "name" : "bvsCallCentreK862",
//    |                "type" : "string"
//    |              }, {
//    |                "name" : "bfSettleAccK862",
//    |                "type" : "string"
//    |              }, {
//    |                "name" : "bfSettleAcctpK862",
//    |                "type" : "string"
//    |              } ]
//    |            }
//    |          }, {
//    |            "name" : "naedoSinglePaymK862",
//    |            "type" : {
//    |              "type" : "record",
//    |              "name" : "NaedoSinglePaymK862",
//    |              "fields" : [ {
//    |                "name" : "nspInstall1AmntK862",
//    |                "type" : "double"
//    |              }, {
//    |                "name" : "nspProcDate1K862",
//    |                "type" : "long"
//    |              }, {
//    |                "name" : "nspErrCode1K862",
//    |                "type" : "string"
//    |              }, {
//    |                "name" : "nspInstall2AmntK862",
//    |                "type" : "double"
//    |              }, {
//    |                "name" : "nspProcDate2K862",
//    |                "type" : "long"
//    |              }, {
//    |                "name" : "nspErrCode2K862",
//    |                "type" : "string"
//    |              }, {
//    |                "name" : "nspIndK862",
//    |                "type" : "string"
//    |              }, {
//    |                "name" : "naedosinglepaymk8620",
//    |                "type" : "string"
//    |              } ]
//    |            }
//    |          }, {
//    |            "name" : "ribAolEchoK862",
//    |            "type" : {
//    |              "type" : "record",
//    |              "name" : "RibAolEchoK862",
//    |              "fields" : [ {
//    |                "name" : "ribBenGenderK862",
//    |                "type" : "string"
//    |              }, {
//    |                "name" : "ribBenNonResStatK862",
//    |                "type" : "string"
//    |              }, {
//    |                "name" : "ribaolechok8620",
//    |                "type" : "string"
//    |              } ]
//    |            }
//    |          } ]
//    |        }
//    |      }, {
//    |        "name" : "tbc2K862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "srcNotifMedK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "trgNotifMedK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "brncK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "brncnK862",
//    |        "type" : "long"
//    |      }, {
//    |        "name" : "tellK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "tellnK862",
//    |        "type" : "long"
//    |      }, {
//    |        "name" : "corpCodeK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "sbuK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "overrideIdK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "trgNotifDetK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "clientTypeK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "paymTypeK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "tbcaK862",
//    |        "type" : {
//    |          "type" : "record",
//    |          "name" : "TbcaK862",
//    |          "fields" : [ {
//    |            "name" : "batchNoK862",
//    |            "type" : "long"
//    |          }, {
//    |            "name" : "consolTxIndK862",
//    |            "type" : "string"
//    |          } ]
//    |        }
//    |      }, {
//    |        "name" : "paymNotesK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "cardAuthK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "overrideId2K862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "srcEventNoK862",
//    |        "type" : "long"
//    |      }, {
//    |        "name" : "trgEventNoK862",
//    |        "type" : "long"
//    |      }, {
//    |        "name" : "instrRefNameK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "paymDestK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "acbUserCodeK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "acbUcIndexK862",
//    |        "type" : "long"
//    |      }, {
//    |        "name" : "csGroupNoK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "csBatchNoK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "csTransNoK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "csSinglePaymIndK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "freeBankIndK862",
//    |        "type" : "string"
//    |      }, {
//    |        "name" : "pinBlockOutK862",
//    |        "type" : "long"
//    |      }, {
//    |        "name" : "swiftWuDataK862",
//    |        "type" : {
//    |          "type" : "record",
//    |          "name" : "SwiftWuDataK862",
//    |          "fields" : [ {
//    |            "name" : "payModeTypeK862",
//    |            "type" : "string"
//    |          }, {
//    |            "name" : "caseIdK862",
//    |            "type" : "string"
//    |          }, {
//    |            "name" : "mtcNoK862",
//    |            "type" : "string"
//    |          }, {
//    |            "name" : "swiftTranNoK862",
//    |            "type" : "string"
//    |          }, {
//    |            "name" : "orgCountryK862",
//    |            "type" : "string"
//    |          }, {
//    |            "name" : "senderNameK862",
//    |            "type" : "string"
//    |          }, {
//    |            "name" : "senderSurnameK862",
//    |            "type" : "string"
//    |          }, {
//    |            "name" : "currencyCodeK862",
//    |            "type" : "string"
//    |          }, {
//    |            "name" : "foreignAmntK862",
//    |            "type" : "double"
//    |          }, {
//    |            "name" : "endorsedIndK862",
//    |            "type" : "string"
//    |          }, {
//    |            "name" : "ibanNumberK862",
//    |            "type" : "string"
//    |          } ]
//    |        }
//    |      }, {
//    |        "name" : "posRedeemDetK862",
//    |        "type" : {
//    |          "type" : "record",
//    |          "name" : "PosRedeemDetK862",
//    |          "fields" : [ {
//    |            "name" : "redeemStoidK862",
//    |            "type" : "string"
//    |          }, {
//    |            "name" : "redeemPointK862",
//    |            "type" : "string"
//    |          }, {
//    |            "name" : "moneyMarketIdK862",
//    |            "type" : "string"
//    |          }, {
//    |            "name" : "captureIdK862",
//    |            "type" : "string"
//    |          }, {
//    |            "name" : "captureDateK862",
//    |            "type" : "long"
//    |          }, {
//    |            "name" : "captureTimeK862",
//    |            "type" : "long"
//    |          }, {
//    |            "name" : "authorizeIdK862",
//    |            "type" : "string"
//    |          }, {
//    |            "name" : "authorizeTimeK862",
//    |            "type" : "long"
//    |          }, {
//    |            "name" : "authorizeDateK862",
//    |            "type" : "long"
//    |          }, {
//    |            "name" : "merchantNumberK862",
//    |            "type" : "long"
//    |          }, {
//    |            "name" : "pinIndK862",
//    |            "type" : "string"
//    |          }, {
//    |            "name" : "pilotAtmIndK862",
//    |            "type" : "string"
//    |          }, {
//    |            "name" : "atmRedemptionSelK862",
//    |            "type" : "string"
//    |          } ]
//    |        }
//    |      } ]
//    |    }
//    |  } ]
//    |}
//    |""".stripMargin
//
//val schemaPat = org.apache.avro.Schema.parse(new File("/home/rikus/Documents/ZenAptix/copybookStreams/src/main/resources/Mbsk861.json"))
//println(schemaPat.toString(true))
//val schemaPat2 = org.apache.avro.Schema.parse(new File("/home/rikus/Documents/ZenAptix/copybookStreams/src/main/resources/Mbsk862.json"))

val builderSchema = SchemaBuilder.record("example").namespace("com.zenaptix")
//val a = builderSchema
//  .fields()
//  .name("lngth861").`type`("int").noDefault()
//
//val b = a
//  .name("pkeyk861").`type`().record("Pkeyk861")
//  .fields()
//  .name("uniqEftNoK861").`type`("string").noDefault()
//
//val c = a
//  .name("datak861").`type`().record("Datak861")
//  .fields()
//  .name("incrPaymNoK861").`type`("long").noDefault()
//  .name("tbcK861").`type`("string").noDefault()
//  .endRecord()
//
//println("***************************" + c.noDefault().endRecord() + "*******************************")


val complete = builderSchema
  .fields()
  .name("lngth861").`type`("int").noDefault()
  .name("pkeyk861").`type`().record("Pkeyk861")
  .fields()
  .name("uniqEftNoK861").`type`("string").noDefault()
  .name("datak861").`type`().record("Datak861")
  .fields()
  .name("incrPaymNoK861").`type`("long").noDefault()
  .name("tbcK861").`type`("string").noDefault()
  .endRecord()


println("completeSchema : " + complete.noDefault().endRecord().noDefault().endRecord())