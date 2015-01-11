package com.enkidu.lignum.parsers.ast.expressions

import com.enkidu.lignum.parsers.ast.VisitorTest
import com.enkidu.lignum.parsers.ast.expression.ArrayInitializer
import com.enkidu.lignum.parsers.ast.expression.discardable._
import com.enkidu.lignum.parsers.ast.expression.discardable.binary._
import com.enkidu.lignum.parsers.ast.expression.discardable.binary.assignment._
import com.enkidu.lignum.parsers.ast.expression.discardable.dimension._
import com.enkidu.lignum.parsers.ast.expression.discardable.instantiation._
import com.enkidu.lignum.parsers.ast.expression.discardable.literals._
import com.enkidu.lignum.parsers.ast.expression.discardable.unary._
import com.enkidu.lignum.parsers.ast.expression.types.references.ArrayType

class DiscardableVisitorTest extends VisitorTest {

  "Visitor should be properly dispatched in" - {
    "ArrayConstructorReference" in {
      ArrayConstructorReference(ArrayType(typ, dim)).visit()
      visited shouldBe 4
    }
    "ConstructorReference" in {
      ConstructorReference(typ, arg).visit()
      visited shouldBe 3
    }
    "ParentReference$" in {
      ParentReference.visit()
      visited shouldBe 1
    }
    "Select" in {
      Select("").visit()
      visited shouldBe 1
    }
    "ThisReference$" in {
      ThisReference.visit()
      visited shouldBe 1
    }
    "PostDecrementation" in {
      PostDecrementation(expr).visit()
      visited shouldBe 2
    }
    "PostIncrementation" in {
      PostIncrementation(expr).visit()
      visited shouldBe 2
    }
    "PreDecrementation" in {
      PreDecrementation(expr).visit()
      visited shouldBe 2
    }
    "PreIncrementation" in {
      PreIncrementation(expr).visit()
      visited shouldBe 2
    }
    "literals" - {
      "StringLiteral" in {
        StringLiteral("").visit()
        visited shouldBe 1
      }
      "ShortLiteral" in {
        ShortLiteral("").visit()
        visited shouldBe 1
      }
      "BooleanLiteral" in {
        BooleanLiteral("").visit()
        visited shouldBe 1
      }
      "ByteLiteral" in {
        ByteLiteral("").visit()
        visited shouldBe 1
      }
      "FloatLiteral" in {
        FloatLiteral("").visit()
        visited shouldBe 1
      }
      "ClassLiteral" in {
        ClassLiteral(typ).visit()
        visited shouldBe 2
      }
      "VoidLiteral" in {
        VoidLiteral("").visit()
        visited shouldBe 1
      }
      "LongLiteral" in {
        LongLiteral("").visit()
        visited shouldBe 1
      }
      "DoubleLiteral" in {
        DoubleLiteral("").visit()
        visited shouldBe 1
      }
      "IntegerLiteral" in {
        IntegerLiteral("").visit()
        visited shouldBe 1
      }
      "CharLiteral" in {
        CharLiteral("").visit()
        visited shouldBe 1
      }
    }
    "ClassLiteral" in {
      ClassLiteral(typ).visit()
      visited shouldBe 2
    }
    "AnonymousObjectInstantiation" in {
      AnonymousObjectInstantiation(arg, typ, expr, decl).visit()
      visited shouldBe 5
    }
    "EmptyArrayInstantiation" in {
      EmptyArrayInstantiation(typ, dim).visit()
      visited shouldBe 3
    }
    "InitializedArrayInstantiation" in {
      InitializedArrayInstantiation(typ, ArrayInitializer(expr)).visit()
      visited shouldBe 4
    }
    "NestedAnonymousObjectInstantiation" in {
      NestedAnonymousObjectInstantiation(expr, arg, typ, expr, decl).visit()
      visited shouldBe 6
    }
    "NestedObjectInstantiation" in {
      NestedObjectInstantiation(expr, arg, typ, expr).visit()
      visited shouldBe 5
    }
    "ObjecInstantiationt" in {
      SimpleObjectInstantiation(arg, typ, expr).visit()
      visited shouldBe 4
    }
    "AbstractDimension" in {
      AbstractDimension(ann).visit()
      visited shouldBe 2
    }
    "InitializedDimension" in {
      InitializedDimension(ann, expr).visit()
      visited shouldBe 3
    }
    "unary" - {
      "assignment" - {
        "AugmentedBinding" in {
          AugmentedBinding(expr, null, expr).visit()
          visited shouldBe 3
        }
        "Binding" in {
          Binding(expr, expr).visit()
          visited shouldBe 3
        }
      }
      "Extraction" in {
        Extraction(expr, expr).visit()
        visited shouldBe 3
      }
      "FieldAccess" in {
        FieldAccess(expr, "").visit()
        visited shouldBe 2
      }
      "MethodInvocation" in {
        MethodInvocation(arg, "", expr).visit()
        visited shouldBe 3
      }
      "MethodReference" in {
        MethodReference(expr, arg, "").visit()
        visited shouldBe 3
      }
      "QualifiedMethodInvocation" in {
        QualifiedMethodInvocation(expr, arg, "", expr).visit()
        visited shouldBe 4
      }
      "QualifiedParentReference" in {
        QualifiedParentReference("").visit()
        visited shouldBe 1
      }
      "QualifiedThisReference" in {
        QualifiedThisReference("").visit()
        visited shouldBe 1
      }
    }
  }

}
