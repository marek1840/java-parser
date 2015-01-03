package com.enkidu.lignum.parsers.java.expressions

import com.enkidu.lignum.parsers.types._
import com.enkidu.lignum.parsers.expressions._
import com.enkidu.lignum.parsers.statements._
import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.JavaParser

class ExpressionTest extends ParserTest {
  def parse(string: String): Expression = {
    implicit val parser = new JavaParser(string)
    get(parser.expression.run())
  }

  "Expression parser should parse" - {
    "reference" - {
      "a" in {parse("a") shouldBe Select("a")}
      "trueHash" in {parse("trueHash") shouldBe Select("trueHash")}
    }

    "lambda expressions" - {
      "() -> {}" in { parse("() -> {}") shouldBe Lambda(Vector, Block(Vector)) }
      "() -> 42" in { parse("() -> 42") shouldBe Lambda(Vector, FlowStatement.ImplicitReturn(PrimitiveLiteral("42"))) }
      "() -> null" in { parse("() -> null") shouldBe Lambda(Vector, FlowStatement.ImplicitReturn(PrimitiveLiteral("null"))) }
      "() -> {return 42; }" in { parse("() -> {return 42; }") shouldBe Lambda(Vector, Block(Vector(FlowStatement.Return(PrimitiveLiteral("42"))))) }
      "() -> {System.gc();}" in { parse("() -> {System.gc();}") shouldBe Lambda(Vector, Block(Vector(QualifiedMethodInvocation(Select("System"), Vector, "gc", Vector)))) }

      "(final int x) -> x+1" in {
        parse("(final int x) -> x+1") shouldBe Lambda(
          Vector(Parameter.Formal(Vector, true, PrimitiveType.Integer(Vector), "x")),
          FlowStatement.ImplicitReturn(BinaryOperationChain(BinaryOperator.+, Vector(Select("x"), PrimitiveLiteral("1")))))
      }
      "(int x) -> { return x+1; }" in {
        parse("(int x) -> { return x+1; }") shouldBe Lambda(
          Vector(Parameter.Formal(Vector, false, PrimitiveType.Integer(Vector), "x")),
          Block(Vector(FlowStatement.Return(BinaryOperationChain(BinaryOperator.+, Vector(Select("x"), PrimitiveLiteral("1")))))))
      }
      "(x) -> x+1" in {
        parse("(x) -> x+1") shouldBe Lambda(
          Vector(Parameter.Inferred("x")),
          FlowStatement.ImplicitReturn(BinaryOperationChain(BinaryOperator.+, Vector(Select("x"), PrimitiveLiteral("1")))))
      }
      "x -> x+1" in {
        parse("x -> x+1") shouldBe Lambda(
          Vector(Parameter.Inferred("x")),
          FlowStatement.ImplicitReturn(BinaryOperationChain(BinaryOperator.+, Vector(Select("x"), PrimitiveLiteral("1")))))
      }

      "(String s) -> s.length()" in {
        parse("(String s) -> s.length()") shouldBe Lambda(
          Vector(Parameter.Formal(Vector, false, ClassType(Vector, None, "String", Vector), "s")),
          FlowStatement.ImplicitReturn(QualifiedMethodInvocation(Select("s"), Vector, "length", Vector)))
      }
      "(Thread t) -> { t.start(); }" in {
        parse("(Thread t) -> { t.start(); }") shouldBe Lambda(
          Vector(Parameter.Formal(Vector, false, ClassType(Vector, None, "Thread", Vector), "t")),
          Block(QualifiedMethodInvocation(Select("t"), Vector, "start", Vector)))
      }
      "s -> s.length()" in {
        parse("s -> s.length()") shouldBe Lambda(
          Vector(Parameter.Inferred("s")),
          FlowStatement.ImplicitReturn(QualifiedMethodInvocation(Select("s"), Vector, "length", Vector)))
      }
      "t -> { t.start(); }" in {
        parse("t -> { t.start(); }") shouldBe Lambda(
          Vector(Parameter.Inferred("t")),
          Block(QualifiedMethodInvocation(Select("t"), Vector, "start", Vector)))
      }

      "(int x, int y) -> x+y" in {
        parse("(int x, int y) -> x+y") shouldBe Lambda(
          Vector(Parameter.Formal(Vector, false, PrimitiveType.Integer(Vector), "x"), Parameter.Formal(Vector, false, PrimitiveType.Integer(Vector), "y")),
          FlowStatement.ImplicitReturn(BinaryOperationChain(BinaryOperator.+, Vector(Select("x"), Select("y")))))
      }
      "(x, y) -> x+y" in {
        parse("(x, y) -> x+y") shouldBe Lambda(
          Vector(Parameter.Inferred("x"), Parameter.Inferred("y")),
          FlowStatement.ImplicitReturn(BinaryOperationChain(BinaryOperator.+, Vector(Select("x"), Select("y")))))
      }
      "(x, int y) -> x+y " in { an[Exception] should be thrownBy parse("(x, int y) -> x+y ") }
      "(x, final y) -> x+y" in { an[Exception] should be thrownBy parse("(x, final y) -> x+y") }
    }

    "assignments" - {
      "expr.field = expr" in { parse("expr.field = expr") shouldBe Binding(Select(Vector("expr", "field")), Select("expr")) }
      "array[1] = expr" in { parse("array[1] = expr") shouldBe Binding(Extraction(Select("array"), PrimitiveLiteral("1")), Select("expr")) }

      "expr= expr" in { parse("expr= expr") shouldBe Binding(Select("expr"), Select("expr")) }
      "expr =expr" in { parse("expr =expr") shouldBe Binding(Select("expr"), Select("expr")) }

      "expr = expr" in { parse("expr = expr") shouldBe Binding(Select("expr"), Select("expr")) }
      "expr *= expr" in { parse("expr *= expr") shouldBe AugmentedBinding(Select("expr"), BinaryOperator.*, Select("expr")) }
      "expr /= expr" in { parse("expr /= expr") shouldBe AugmentedBinding(Select("expr"), BinaryOperator./, Select("expr")) }
      "expr %= expr" in { parse("expr %= expr") shouldBe AugmentedBinding(Select("expr"), BinaryOperator.mod, Select("expr")) }
      "expr += expr" in { parse("expr += expr") shouldBe AugmentedBinding(Select("expr"), BinaryOperator.+, Select("expr")) }
      "expr -= expr" in { parse("expr -= expr") shouldBe AugmentedBinding(Select("expr"), BinaryOperator.-, Select("expr")) }
      "expr <<= expr" in { parse("expr <<= expr") shouldBe AugmentedBinding(Select("expr"), BinaryOperator.<<, Select("expr")) }
      "expr >>= expr" in { parse("expr >>= expr") shouldBe AugmentedBinding(Select("expr"), BinaryOperator.>>, Select("expr")) }
      "expr >>>= expr" in { parse("expr >>>= expr") shouldBe AugmentedBinding(Select("expr"), BinaryOperator.>>>, Select("expr")) }
      "expr &= expr" in { parse("expr &= expr") shouldBe AugmentedBinding(Select("expr"), BinaryOperator.&, Select("expr")) }
      "expr ^= expr" in { parse("expr ^= expr") shouldBe AugmentedBinding(Select("expr"), BinaryOperator.^, Select("expr")) }
      "expr |= expr" in { parse("expr |= expr") shouldBe AugmentedBinding(Select("expr"), BinaryOperator.|, Select("expr")) }
    }
    "ternany" - {
      "x || y ? x : y" in {
        parse("x || y ? x : y") shouldBe TernaryConditional(
          BinaryOperationChain(BinaryOperator.or, Select("x") +: Select("y")),
          Select("x"),
          Select("y"))
      }
      "x || y ? x || y ? x : y : y" in {
        parse("x || y ? x || y ? x : y : y") shouldBe TernaryConditional(
          BinaryOperationChain(BinaryOperator.or, Select("x") +: Select("y")),
          TernaryConditional(
            BinaryOperationChain(BinaryOperator.or, Select("x") +: Select("y")),
            Select("x"),
            Select("y")),
          Select("y"))
      }
      "x || y ? x : x || y ? x : y" in {
        parse("x || y ? x : x || y ? x : y") shouldBe TernaryConditional(
          BinaryOperationChain(BinaryOperator.or, Select("x") +: Select("y")),
          Select("x"),
          TernaryConditional(
            BinaryOperationChain(BinaryOperator.or, Select("x") +: Select("y")),
            Select("x"),
            Select("y")))
      }
    }

    "binary operations" - {
      "x || y" in { parse("x || y") shouldBe BinaryOperationChain(BinaryOperator.or, Select("x") +: Select("y")) }
      "x && y" in { parse("x && y") shouldBe BinaryOperationChain(BinaryOperator.and, Select("x") +: Select("y")) }
      "x | y" in { parse("x | y") shouldBe BinaryOperationChain(BinaryOperator.|, Select("x") +: Select("y")) }
      "x ^ y" in { parse("x ^ y") shouldBe BinaryOperationChain(BinaryOperator.^, Select("x") +: Select("y")) }
      "x & y" in { parse("x & y") shouldBe BinaryOperationChain(BinaryOperator.&, Select("x") +: Select("y")) }
      "x == y" in { parse("x == y") shouldBe BinaryOperationChain(BinaryOperator.==, Select("x") +: Select("y")) }
      "x != y" in { parse("x != y") shouldBe BinaryOperationChain(BinaryOperator.!=, Select("x") +: Select("y")) }
      "x < y" in { parse("x < y") shouldBe BinaryOperationChain(BinaryOperator.<, Select("x") +: Select("y")) }
      "x <= y" in { parse("x <= y") shouldBe BinaryOperationChain(BinaryOperator.<=, Select("x") +: Select("y")) }
      "x > y" in { parse("x > y") shouldBe BinaryOperationChain(BinaryOperator.>, Select("x") +: Select("y")) }
      "x >= y" in { parse("x >= y") shouldBe BinaryOperationChain(BinaryOperator.>=, Select("x") +: Select("y")) }
      "x instanceof y" in { parse("x instanceof y") shouldBe BinaryOperationChain(BinaryOperator.instanceof, Select("x") +: ClassType(Vector(), None, "y", Vector())) }
      "x << y" in { parse("x << y") shouldBe BinaryOperationChain(BinaryOperator.<<, Select("x") +: Select("y")) }
      "x >> y" in { parse("x >> y") shouldBe BinaryOperationChain(BinaryOperator.>>, Select("x") +: Select("y")) }
      "x >>> y" in { parse("x >>> y") shouldBe BinaryOperationChain(BinaryOperator.>>>, Select("x") +: Select("y")) }
      "x + y" in { parse("x + y") shouldBe BinaryOperationChain(BinaryOperator.+, Select("x") +: Select("y")) }
      "x - y" in { parse("x - y") shouldBe BinaryOperationChain(BinaryOperator.-, Select("x") +: Select("y")) }
      "x * y" in { parse("x * y") shouldBe BinaryOperationChain(BinaryOperator.*, Select("x") +: Select("y")) }
      "x / y" in { parse("x / y") shouldBe BinaryOperationChain(BinaryOperator./, Select("x") +: Select("y")) }
      "x % y" in { parse("x % y") shouldBe BinaryOperationChain(BinaryOperator.mod, Select("x") +: Select("y")) }

      "with correct priorities" in {
        parse("x % y / y * y - y + y >>> y >> y << y instanceof Class >= y > y <= y < y != y == y & y ^ y | y && y || y") shouldBe BinaryOperationChain(Vector(BinaryOperator.or),
          Vector(BinaryOperationChain(Vector(BinaryOperator.and), Vector(BinaryOperationChain(Vector(BinaryOperator.|), Vector(BinaryOperationChain(Vector(BinaryOperator.^), Vector(BinaryOperationChain(Vector(BinaryOperator.&), Vector(BinaryOperationChain(Vector(BinaryOperator.!=, BinaryOperator.==), Vector(BinaryOperationChain(Vector(BinaryOperator.instanceof, BinaryOperator.>=, BinaryOperator.>, BinaryOperator.<=, BinaryOperator.<), Vector(BinaryOperationChain(Vector(BinaryOperator.>>>, BinaryOperator.>>, BinaryOperator.<<), Vector(BinaryOperationChain(Vector(BinaryOperator.-, BinaryOperator.+), Vector(BinaryOperationChain(Vector(BinaryOperator.mod, BinaryOperator./, BinaryOperator.*), Vector(Select(Vector("x")), Select(Vector("y")), Select(Vector("y")), Select(Vector("y")))),
            Select(Vector("y")), Select(Vector("y")))),
            Select(Vector("y")), Select(Vector("y")), Select(Vector("y")))),
            ClassType(Vector, None, "Class", Vector), Select(Vector("y")), Select(Vector("y")), Select(Vector("y")), Select(Vector("y")))),
            Select(Vector("y")), Select(Vector("y")))),
            Select(Vector("y")))),
            Select(Vector("y")))),
            Select(Vector("y")))),
            Select(Vector("y")))),
            Select(Vector("y"))))
      }

    }
    "unary operations" - {
      "++i" in { parse("++i") shouldBe PreIncrementation(Select("i")) }
      "--i" in { parse("--i") shouldBe PreDecrementation(Select("i")) }
      "i++" in { parse("i++") shouldBe PostIncrementation(Select("i")) }
      "i--" in { parse("i--") shouldBe PostDecrementation(Select("i")) }
      "+ i" in { parse("+ i") shouldBe UnaryOperationChain(UnaryOperator.+, Select("i")) }
      "-i" in { parse("-i") shouldBe UnaryOperationChain(UnaryOperator.-, Select("i")) }
      "~i" in { parse("~i") shouldBe UnaryOperationChain(UnaryOperator.~, Select("i")) }
      "!i" in { parse("!i") shouldBe UnaryOperationChain(UnaryOperator.not, Select("i")) }
    }

    "casts" - {
      "(int) ++i" in { parse("(int) ++i") shouldBe Cast(PrimitiveType.Integer(Vector), PreIncrementation(Select("i"))) }
      "(Class) x" in { parse("(Class) x") shouldBe Cast(ClassType(Vector, None, "Class", Vector), Select("x")) }
      "(Class ) () -> 3" in { parse("(Class ) () -> 3") shouldBe Cast(ClassType(Vector, None, "Class", Vector), Lambda(Vector, FlowStatement.ImplicitReturn(PrimitiveLiteral("3")))) }
      "(Class & I1 & I2) () -> 3" in { parse("(Class & I1 & I2) () -> 3") shouldBe Cast(ChildOfAll(Seq(ClassType(Vector, None, "Class", Vector), ClassType(Vector, None, "I1", Vector), ClassType(Vector, None, "I2", Vector))), Lambda(Vector, FlowStatement.ImplicitReturn(PrimitiveLiteral("3")))) }
    }

  }
}