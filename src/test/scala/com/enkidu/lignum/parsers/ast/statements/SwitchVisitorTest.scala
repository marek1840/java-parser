package com.enkidu.lignum.parsers.ast.statements

import com.enkidu.lignum.parsers.ast.VisitorTest
import com.enkidu.lignum.parsers.ast.statement.switch._

class SwitchVisitorTest extends VisitorTest {
  "Visitor should be properly dispatched in" - {
    "DefaultSwitch" in {
      DefaultSwitch.visit()
      visited shouldBe 1
    }
    "EmptySwitchCases" in {
      EmptySwitchCases(expr).visit()
      visited shouldBe 2
    }
    "SwitchCases" in {
      SwitchCases(expr, stmt).visit()
      visited shouldBe 3
    }
    "SwitchStatement" in {
      SwitchStatement(expr, EmptySwitchCases(expr)).visit()
      visited shouldBe 4
    }
  }
}
