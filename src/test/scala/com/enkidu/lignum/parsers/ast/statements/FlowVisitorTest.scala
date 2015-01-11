package com.enkidu.lignum.parsers.ast.statements

import com.enkidu.lignum.parsers.ast.VisitorTest
import com.enkidu.lignum.parsers.ast.statement.flow._

class FlowVisitorTest extends VisitorTest {
  "Visitor should be properly dispatched in" - {
    "ImplicitReturn" in {
      ImplicitReturn(expr).visit()
      visited shouldBe 2
    }
    "Return" in {
      Return(expr).visit()
      visited shouldBe 2
    }
    "Throw" in {
      Throw(expr).visit()
      visited shouldBe 2
    }
    "Yield" in {
      Yield(expr).visit()
      visited shouldBe 2
    }

    "Break" in {
      Break.visit()
      visited shouldBe 1
    }
    "Continue" in {
      Continue.visit()
      visited shouldBe 1
    }
    "EmptyReturn" in {
      EmptyReturn.visit()
      visited shouldBe 1
    }
    "TargetedBreak" in {
      TargetedBreak("").visit()
      visited shouldBe 1
    }
    "TargetedContinue" in {
      TargetedContinue("").visit()
      visited shouldBe 1
    }
  }
}
