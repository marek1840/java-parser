package com.enkidu.lignum.parsers.java.expressions

import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.ast.expression._
import com.enkidu.lignum.parsers.ast.expression.discardable.dimension.AbstractDimension
import com.enkidu.lignum.parsers.ast.expression.discardable.literals.ClassLiteral
import com.enkidu.lignum.parsers.ast.expression.types.primitives._
import com.enkidu.lignum.parsers.ast.expression.types.references.{ArrayType, ClassType}
import com.enkidu.lignum.parsers.java.v8.JavaCompilationUnitParser
import org.parboiled2.ParserInput.apply

class ClassLiteralTest extends ParserTest {
  def parse(string: String): Expression = {
    implicit val parser = new JavaCompilationUnitParser(string)
    get(parser.expression.run())
  }

  "Class literal parser should" - {
    "parse" - {
      "int.class" in { parse("int.class") shouldBe ClassLiteral(IntegerPrimitive(Vector)) }
      "name.class" in (parse("name.class") shouldBe ClassLiteral(ClassType(Vector, None, "name", Vector)))
      "name . class" in (parse("name . class") shouldBe ClassLiteral(ClassType(Vector, None, "name", Vector)))
      "void.class" in (parse("void.class") shouldBe ClassLiteral(VoidPrimitive(Vector)))
      "name1.name2.class" in (parse("name1.name2.class") shouldBe ClassLiteral(ClassType(Vector, Some(ClassType(Vector, None, "name1", Vector)), "name2", Vector)))
      "name[].class" in (parse("name[].class") shouldBe ClassLiteral(ArrayType(ClassType(Vector, None, "name", Vector), Vector(AbstractDimension(Vector)))))
      "name[][].class" in (parse("name[][].class") shouldBe ClassLiteral(ArrayType(ClassType(Vector, None, "name", Vector), Vector(AbstractDimension(Vector), AbstractDimension(Vector)))))
      "name1.name2[][].class" in (parse("name1.name2[][].class") shouldBe ClassLiteral(ArrayType(ClassType(Vector, Some(ClassType(Vector, None, "name1", Vector)), "name2", Vector), Vector(AbstractDimension(Vector), AbstractDimension(Vector)))))
      "name1 . name2[][] . class" in (parse("name1 . name2[][] . class") shouldBe ClassLiteral(ArrayType(ClassType(Vector, Some(ClassType(Vector, None, "name1", Vector)), "name2", Vector), Vector(AbstractDimension(Vector), AbstractDimension(Vector)))))
    }
    "not parse" - {
      "class.class" in (an[Exception] should be thrownBy parse("class.class"))
    }
  }
}