package com.enkidu.lignum.parsers.java.types

import com.enkidu.lignum.parsers.types._
import com.enkidu.lignum.parsers.expressions._
import com.enkidu.lignum.parsers.statements._
import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.JavaParser

class ReferenceTypeTest extends ParserTest {
  def parse(string: String): Type = {
    implicit val parser = new JavaParser(string)
    get(parser.`type`.run())
  }

  "Reference parser should parse" - {

    "class or interface type" - {
      "Class<T.U>" in { parse("Class<T.U>") shouldBe ClassType(Vector, None, "Class", Template.Argument(ClassType(Vector, ClassType(Vector, None, "T", Vector), "U", Vector))) }
      "Class<?>" in { parse("Class<?>") shouldBe ClassType(Vector, None, "Class", Template.Any(Vector)) }
      "Class<? extends T>" in { parse("Class<? extends T>") shouldBe ClassType(Vector, None, "Class", Template.AnySubClass(Vector, ClassType(Vector, None, "T", Vector))) }
      "Class<? super T>" in { parse("Class<? super T>") shouldBe ClassType(Vector, None, "Class", Template.AnyBaseClass(Vector, ClassType(Vector, None, "T", Vector))) }
    	"Class<T>" in { parse("Class<T>") shouldBe ClassType(Vector, None, "Class", Template.Argument(ClassType(Vector, None, "T", Vector))) }
      "Class.Inner<T,U>" in { parse("Class.Inner<T,U>") shouldBe ClassType(Vector, ClassType(Vector, None, "Class", Vector), "Inner", Vector(Template.Argument(ClassType(Vector, None, "T", Vector)), Template.Argument(ClassType(Vector, None, "U", Vector)))) }
      "Class<T,U>" in { parse("Class<T,U>") shouldBe ClassType(Vector, None, "Class", Vector(Template.Argument(ClassType(Vector, None, "T", Vector)), Template.Argument(ClassType(Vector, None, "U", Vector)))) }
      "Class<T,U>.Inner" in { parse("Class<T,U>.Inner") shouldBe ClassType(Vector, ClassType(Vector, None, "Class", Vector(Template.Argument(ClassType(Vector, None, "T", Vector)), Template.Argument(ClassType(Vector, None, "U", Vector)))), "Inner", Vector) }
      "Class<T,U>.Inner<T,U>" in { parse("Class<T,U>.Inner<T,U>") shouldBe ClassType(Vector, ClassType(Vector, None, "Class", Vector(Template.Argument(ClassType(Vector, None, "T", Vector)), Template.Argument(ClassType(Vector, None, "U", Vector)))), "Inner", Vector(Template.Argument(ClassType(Vector, None, "T", Vector)), Template.Argument(ClassType(Vector, None, "U", Vector)))) }
    }

    "with annotation" - {
      "@A Class" in { parse("@A Class") shouldBe ClassType(MarkerAnnotation("A"), None, "Class", Vector) }
      "@A Class.Inner" in { parse("@A Class.Inner") shouldBe ClassType(Vector, ClassType(MarkerAnnotation("A"), None, "Class", Vector), "Inner", Vector) }
      "@A Class. @A Inner" in { parse("@A Class. @A Inner") shouldBe ClassType(MarkerAnnotation("A"), ClassType(MarkerAnnotation("A"), None, "Class", Vector), "Inner", Vector) }
      "@A Class.@A Inner<T,U>" in { parse("@A Class.@A Inner<T,U>") shouldBe ClassType(MarkerAnnotation("A"), ClassType(MarkerAnnotation("A"), None, "Class", Vector), "Inner", Vector(Template.Argument(ClassType(Vector, None, "T", Vector)), Template.Argument(ClassType(Vector, None, "U", Vector)))) }
      "@A Class<T>" in { parse("@A Class<T>") shouldBe ClassType(MarkerAnnotation("A"), None, "Class", Template.Argument(ClassType(Vector, None, "T", Vector))) }
      "@A Class<T,U>" in { parse("@A Class<T,U>") shouldBe ClassType(MarkerAnnotation("A"), None, "Class", Vector(Template.Argument(ClassType(Vector, None, "T", Vector)), Template.Argument(ClassType(Vector, None, "U", Vector)))) }
      "Class<T,U>.@A Inner" in { parse("Class<T,U>.@A Inner") shouldBe ClassType(MarkerAnnotation("A"), ClassType(Vector, None, "Class", Vector(Template.Argument(ClassType(Vector, None, "T", Vector)), Template.Argument(ClassType(Vector, None, "U", Vector)))), "Inner", Vector) }
      "@A Class<T,U>.@A Inner<T,U>" in { parse("@A Class<T,U>.@A Inner<T,U>") shouldBe ClassType(MarkerAnnotation("A"), ClassType(MarkerAnnotation("A"), None, "Class", Vector(Template.Argument(ClassType(Vector, None, "T", Vector)), Template.Argument(ClassType(Vector, None, "U", Vector)))), "Inner", Vector(Template.Argument(ClassType(Vector, None, "T", Vector)), Template.Argument(ClassType(Vector, None, "U", Vector)))) }
    }
    "array type" - {
      "int []" in { parse("int []") shouldBe ArrayType(PrimitiveType.Integer(Vector), Vector(AbstractDimension(Vector))) }
      "Class[]" in { parse("Class[]") shouldBe ArrayType(ClassType(Vector, None, "Class", Vector), Vector(AbstractDimension(Vector))) }
      "Class@A[]" in { parse("Class@A[]") shouldBe ArrayType(ClassType(Vector, None, "Class", Vector), Vector(AbstractDimension(Vector(MarkerAnnotation("A"))))) }
      "Class @A [] @A @A[]" in { parse("Class @A [] @A @A[]") shouldBe ArrayType(ClassType(Vector, None, "Class", Vector), Vector(AbstractDimension(Vector(MarkerAnnotation("A"))), AbstractDimension(Vector(MarkerAnnotation("A"), MarkerAnnotation("A"))))) }
    }
  }
}