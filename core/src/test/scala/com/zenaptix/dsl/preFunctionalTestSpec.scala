package com.zenaptix.dsl

import org.scalatest.WordSpec

/**
  * Created by rikus on 9/15/17.
  */
class preFunctionalTestSpec extends WordSpec {
  import FunctionalTestSpecResources._

  "Create case classes source from AST" in {
    Files.createCaseClasses(forest,"com.zenaptix.dsl")
  }
}
