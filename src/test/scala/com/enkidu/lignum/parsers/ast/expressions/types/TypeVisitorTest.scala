package com.enkidu.lignum.parsers.ast.expressions.types

import com.enkidu.lignum.parsers.ast.VisitorTest
import com.enkidu.lignum.parsers.ast.expression.types.annotations._
import com.enkidu.lignum.parsers.ast.expression.types.coupled._
import com.enkidu.lignum.parsers.ast.expression.types.primitives._
import com.enkidu.lignum.parsers.ast.expression.types.references._

class TypeVisitorTest extends VisitorTest {

  "Visitor should be properly dispatched in" - {
    "ArrayType" in {
      ArrayType(typ, dim).visit()
      visited shouldBe 3
    }
    "ClassType" in {
      ClassType(ann, typ, "", arg).visit()
      visited shouldBe 4
    }

    "ChildOfAll" in {
      ChildOfAll(typ).visit()
      visited shouldBe 2
    }
    "ChildOfAny" in {
      ChildOfAny(typ).visit()
      visited shouldBe 2
    }

    "annotations" - {
      "MarkerAnnotation" in {
        MarkerAnnotation("").visit()
        visited shouldBe 1
      }
      "NormalAnnotation" in {
        NormalAnnotation("", ("", expr)).visit()
        visited shouldBe 2
      }
      "SingleElementAnnotation" in {
        SingleElementAnnotation("", expr).visit()
        visited shouldBe 2
      }
    }

    "primitives" - {
      "Boolean" in {
        BooleanPrimitive(ann).visit()
        visited shouldBe 2
      }
      "Byte" in {
        BytePrimitive(ann).visit()
        visited shouldBe 2
      }
      "Char" in {
        CharPrimitive(ann).visit()
        visited shouldBe 2
      }
      "Double" in {
        DoublePrimitive(ann).visit()
        visited shouldBe 2
      }
      "Float" in {
        FloatPrimitive(ann).visit()
        visited shouldBe 2
      }
      "Integer" in {
        IntegerPrimitive(ann).visit()
        visited shouldBe 2
      }
      "Long" in {
        LongPrimitive(ann).visit()
        visited shouldBe 2
      }
      "Short" in {
        ShortPrimitive(ann).visit()
        visited shouldBe 2
      }
      "Void" in {
        VoidPrimitive(ann).visit()
        visited shouldBe 2
      }
    }

  }
}
