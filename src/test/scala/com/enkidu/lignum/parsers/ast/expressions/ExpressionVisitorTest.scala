package com.enkidu.lignum.parsers.ast.expressions

import com.enkidu.lignum.parsers.ast.VisitorTest
import com.enkidu.lignum.parsers.ast.expression._

class ExpressionVisitorTest extends VisitorTest {

  "Visitor should be properly dispatched in" - {
    "BinaryOperations" in {
      BinaryOperations(Vector(), expr).visit()
      visited shouldBe 2
    }
    "ArrayInitializer" in {
      ArrayInitializer(expr).visit()
      visited shouldBe 2
    }
    "Lambda" in {
      Lambda(argParam, stmt).visit()
      visited shouldBe 3
    }
    "Cast" in {
      Cast(typ, expr).visit()
      visited shouldBe 3
    }
    "TernaryConditional" in {
      TernaryConditional(expr, expr, expr).visit()
      visited shouldBe 4
    }
    "UnaryOperations" in {
      UnaryOperations(Vector(), expr).visit()
      visited shouldBe 2
    }
  }
}
