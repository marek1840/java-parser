package com.enkidu.lignum.parsers.java.types

import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.ast.expression.discardable.dimension.AbstractDimension
import com.enkidu.lignum.parsers.ast.expression.types._
import com.enkidu.lignum.parsers.ast.expression.types.annotations.MarkerAnnotation
import com.enkidu.lignum.parsers.ast.expression.types.primitives.IntegerPrimitive
import com.enkidu.lignum.parsers.ast.expression.types.references.{ArrayType, ClassType}
import com.enkidu.lignum.parsers.ast.expression.types.templates.{AnyBaseClassTemplate, AnySubClassTemplate, AnyTemplate, ArgumentTemplate}
import com.enkidu.lignum.parsers.java.v8.JavaCompilationUnitParser

class ReferenceTypeTest extends ParserTest {
  def parse(string: String): Type = {
    implicit val parser = new JavaCompilationUnitParser(string)
    get(parser.`type`.run())
  }

  "Reference parser should parse" - {

    "class or interface type" - {
      "Class<T.U>" in { parse("Class<T.U>") shouldBe ClassType(Vector, None, "Class", ArgumentTemplate(ClassType(Vector, 
        ClassType(Vector, None, "T", Vector), "U", Vector))) }
      "Class<?>" in { parse("Class<?>") shouldBe ClassType(Vector, None, "Class", AnyTemplate(Vector)) }
      "Class<? extends T>" in { parse("Class<? extends T>") shouldBe ClassType(Vector, None, "Class", AnySubClassTemplate(Vector, ClassType(Vector, None, "T", Vector))) }
      "Class<? super T>" in { parse("Class<? super T>") shouldBe ClassType(Vector, None, "Class", AnyBaseClassTemplate(Vector, ClassType(Vector, None, "T", Vector))) }
    	"Class<T>" in { parse("Class<T>") shouldBe ClassType(Vector, None, "Class", ArgumentTemplate(ClassType(Vector, None, "T", Vector))) }
      "Class.Inner<T,U>" in { parse("Class.Inner<T,U>") shouldBe ClassType(Vector, ClassType(Vector, None, "Class", Vector), "Inner", Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector)), ArgumentTemplate(ClassType(Vector, None, "U", Vector)))) }
      "Class<T,U>" in { parse("Class<T,U>") shouldBe ClassType(Vector, None, "Class", Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector)), ArgumentTemplate(ClassType(Vector, None, "U", Vector)))) }
      "Class<T,U>.Inner" in { parse("Class<T,U>.Inner") shouldBe ClassType(Vector, ClassType(Vector, None, "Class", Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector)), ArgumentTemplate(ClassType(Vector, None, "U", Vector)))), "Inner", Vector) }
      "Class<T,U>.Inner<T,U>" in { parse("Class<T,U>.Inner<T,U>") shouldBe ClassType(Vector, ClassType(Vector, None, "Class", Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector)), ArgumentTemplate(ClassType(Vector, None, "U", Vector)))), "Inner", Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector)), ArgumentTemplate(ClassType(Vector, None, "U", Vector)))) }
    }

    "with annotation" - {
      "@A Class" in { parse("@A Class") shouldBe ClassType(MarkerAnnotation("A"), None, "Class", Vector) }
      "@A Class.Inner" in { parse("@A Class.Inner") shouldBe ClassType(Vector, ClassType(MarkerAnnotation("A"), None, "Class", Vector), "Inner", Vector) }
      "@A Class. @A Inner" in { parse("@A Class. @A Inner") shouldBe ClassType(MarkerAnnotation("A"), ClassType(MarkerAnnotation("A"), None, "Class", Vector), "Inner", Vector) }
      "@A Class.@A Inner<T,U>" in { parse("@A Class.@A Inner<T,U>") shouldBe ClassType(MarkerAnnotation("A"), ClassType(MarkerAnnotation("A"), None, "Class", Vector), "Inner", Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector)), ArgumentTemplate(ClassType(Vector, None, "U", Vector)))) }
      "@A Class<T>" in { parse("@A Class<T>") shouldBe ClassType(MarkerAnnotation("A"), None, "Class", ArgumentTemplate(ClassType(Vector, None, "T", Vector))) }
      "@A Class<T,U>" in { parse("@A Class<T,U>") shouldBe ClassType(MarkerAnnotation("A"), None, "Class", Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector)), ArgumentTemplate(ClassType(Vector, None, "U", Vector)))) }
      "Class<T,U>.@A Inner" in { parse("Class<T,U>.@A Inner") shouldBe ClassType(MarkerAnnotation("A"), ClassType(Vector, None, "Class", Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector)), ArgumentTemplate(ClassType(Vector, None, "U", Vector)))), "Inner", Vector) }
      "@A Class<T,U>.@A Inner<T,U>" in { parse("@A Class<T,U>.@A Inner<T,U>") shouldBe ClassType(MarkerAnnotation("A"), ClassType(MarkerAnnotation("A"), None, "Class", Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector)), ArgumentTemplate(ClassType(Vector, None, "U", Vector)))), "Inner", Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector)), ArgumentTemplate(ClassType(Vector, None, "U", Vector)))) }
    }
    "array type" - {
      "int []" in { parse("int []") shouldBe ArrayType(IntegerPrimitive(Vector), Vector(AbstractDimension(Vector))) }
      "Class[]" in { parse("Class[]") shouldBe ArrayType(ClassType(Vector, None, "Class", Vector), Vector(AbstractDimension(Vector))) }
      "Class@A[]" in { parse("Class@A[]") shouldBe ArrayType(ClassType(Vector, None, "Class", Vector), Vector(AbstractDimension(Vector(MarkerAnnotation("A"))))) }
      "Class @A [] @A @A[]" in { parse("Class @A [] @A @A[]") shouldBe ArrayType(ClassType(Vector, None, "Class", Vector), Vector(AbstractDimension(Vector(MarkerAnnotation("A"))), AbstractDimension(Vector(MarkerAnnotation("A"), MarkerAnnotation("A"))))) }
    }
  }
}