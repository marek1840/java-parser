package com.enkidu.lignum.parsers.ast.statements

import com.enkidu.lignum.parsers.ast.VisitorTest
import com.enkidu.lignum.parsers.ast.statement.parameter._

class ParameterVisitorTest extends VisitorTest {
  "Visitor should be properly dispatched in" - {
    "FormalParameter" in {
      FormalParameter(ann, false, typ, "").visit()
      visited shouldBe 3
    }
    "InferredParameter" in {
      InferredParameter("").visit()
      visited shouldBe 1
    }
    "InstanceReceiverParameter" in {
      InstanceReceiverParameter(ann, typ).visit()
      visited shouldBe 3
    }
    "NestedReceiverParameter" in {
      NestedReceiverParameter(ann, typ, "").visit()
      visited shouldBe 3
    }
    "VariableArityParameter" in {
      VariableArityParameter(ann, false, typ, "").visit()
      visited shouldBe 3
    }
  }
}
