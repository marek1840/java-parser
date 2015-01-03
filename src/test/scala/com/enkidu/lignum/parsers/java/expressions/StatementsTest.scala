package com.enkidu.lignum.parsers.java.expressions

import com.enkidu.lignum.parsers.types._
import com.enkidu.lignum.parsers.expressions._
import com.enkidu.lignum.parsers.statements._
import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.JavaParser

class StatementsTest extends ParserTest {
  def parse(string: String): Statement = {
    implicit val parser = new JavaParser(string)
    get(parser.blockStatement.run())
  }

  "Statement parser should parse" - {
    "local variable declaration" - {
      "int a;" in { parse("int a;") shouldBe Declaration.LocalVariable(Vector(), false, PrimitiveType.Integer(Vector), Vector(Declarator.Variable("a"))) }
      "@A  int a;" in { parse("@A  int a;") shouldBe Declaration.LocalVariable(Vector(MarkerAnnotation("A")), false, PrimitiveType.Integer(Vector), Vector(Declarator.Variable("a"))) }
      "final int a;" in { parse("final int a;") shouldBe Declaration.LocalVariable(Vector(), true, PrimitiveType.Integer(Vector), Vector(Declarator.Variable("a"))) }
      "@A final int a;" in { parse("@A final int a;") shouldBe Declaration.LocalVariable(Vector(MarkerAnnotation("A")), true, PrimitiveType.Integer(Vector), Vector(Declarator.Variable("a"))) }
      "@A final @A int a;" in { parse("@A final @A int a;") shouldBe Declaration.LocalVariable(Vector(MarkerAnnotation("A"), MarkerAnnotation("A")), true, PrimitiveType.Integer(Vector), Vector(Declarator.Variable("a"))) }

      "int a, y[] = x;" in { parse("int a, y[] = x;") shouldBe Declaration.LocalVariable(Vector(), false, PrimitiveType.Integer(Vector), Vector(Declarator.Variable("a"), Declarator.InitializedArray("y", Vector(AbstractDimension(Vector)), Select("x")))) }
      "Class a, b = new Class(), c;" in { parse("Class a, b = new Class(), c;") shouldBe Declaration.LocalVariable(Vector(), false, ClassType(Vector, None, "Class", Vector), Vector(Declarator.Variable("a"), Declarator.InitializedVariable("b", Instantiation.Object(Vector, ClassType(Vector, None, "Class", Vector), Vector)), Declarator.Variable("c"))) }
    }
    "discardable expression statements" - {
      "a = b;" in { parse("a = b;") shouldBe Binding(Select("a"), Select("b")) }
      "a++;" in { parse("a++;") shouldBe PostIncrementation(Select("a")) }
      "a--;" in { parse("a--;") shouldBe PostDecrementation(Select("a")) }
      "--a;" in { parse("--a;") shouldBe PreDecrementation(Select("a")) }
      "++a;" in { parse("++a;") shouldBe PreIncrementation(Select("a")) }
      "a().f();" in { parse("a().f();") shouldBe QualifiedMethodInvocation(MethodInvocation(Vector, "a", Vector), Vector, "f", Vector) }
      "new Class();" in { parse("new Class();") shouldBe Instantiation.Object(Vector, ClassType(Vector, None, "Class", Vector), Vector) }
    }
    "assertions" - {
      "assert x;" in { parse("assert x;") shouldBe Assertion(Select("x"), None) }
      "assert x: y;" in { parse("assert x: y;") shouldBe Assertion(Select("x"), Some(Select("y"))) }
    }
    "flow statements" - {
      "break;" in { parse("break;") shouldBe FlowStatement.Break }
      "break a;" in { parse("break a;") shouldBe FlowStatement.TargetedBreak("a") }
      "continue;" in { parse("continue;") shouldBe FlowStatement.Continue }
      "continue a;" in { parse("continue a;") shouldBe FlowStatement.TargetedContinue("a") }
      "return;" in { parse("return;") shouldBe FlowStatement.EmptyReturn }
      "return a;" in { parse("return a;") shouldBe FlowStatement.Return(Select("a")) }
      "throw a;" in { parse("throw a;") shouldBe FlowStatement.Throw(Select("a")) }
    }
    "synchronized blocks" - {
      "synchronized {int x;}" in { parse("synchronized {int x;}") shouldBe SynchronizedBlock(None, Block(Declaration.LocalVariable(Vector, false, PrimitiveType.Integer(Vector), Vector(Declarator.Variable("x"))))) }
      "synchronized(this) {int x;}" in { parse("synchronized(this) {int x;}") shouldBe SynchronizedBlock(Some(ThisReference), Block(Declaration.LocalVariable(Vector, false, PrimitiveType.Integer(Vector), Vector(Declarator.Variable("x"))))) }
    }
    "try statements" - {
      val tb = Block(Vector(Declaration.LocalVariable(Vector, false, PrimitiveType.Integer(Vector), Vector(Declarator.Variable("x")))))
      val fb = Block(Vector())
      val cs = Vector(CatchClause(Declaration.LocalVariable(Vector, true, ClassType(Vector, None, "Exception", Vector), Declarator.Variable("e")), Block(Vector)))

      "try {int x;} catch(Exception e){}" in { parse("try {int x;} catch(Exception e){}") shouldBe TryCatch(tb, cs) }
      "try {int x;} finally{}" in { parse("try {int x;} finally{}") shouldBe TryFinally(tb, fb) }
      "try {int x;} catch(Exception e){} finally{}" in { parse("try {int x;} catch(Exception e){} finally{}") shouldBe TryCatchFinally(tb, cs, fb) }
      "try(C c = new C()){int x;} catch(Exception e){} finally{}" in { parse("try(C c = new C()){int x;} catch(Exception e){} finally{}") shouldBe TryWithResources(Vector(Declaration.LocalVariable(Vector, true, ClassType(Vector, None, "C", Vector), Vector(Declarator.InitializedVariable("c", Instantiation.Object(Vector, ClassType(Vector, None, "C", Vector), Vector))))), tb, cs, Some(fb)) }
    }
    "switch statements" - {
      val s =
        """switch(a){
        |     case 0:
        |     case 1: ;
        |     default: break;
        |     case Enum.Value: ;
        |     case 3:
        |     case 4:
        |}""".stripMargin

      val s2 =
        """switch (data){
        |     default:
        |        packet = new Packet(datagram);
        |        break;
        |}""".stripMargin

      "switch(a){}" in { parse("switch(a){}") shouldBe SwitchStatement(Select("a"), Vector) }
      s in {
        parse(s) shouldBe SwitchStatement(Select("a"), Vector(
          Switch.Cases(Vector(PrimitiveLiteral("0"), PrimitiveLiteral("1")), EmptyStatement),
          Switch.Cases(Vector(Switch.Default), FlowStatement.Break),
          Switch.Cases(Vector(Select(Vector("Enum", "Value"))), EmptyStatement),
          Switch.EmptyCases(Vector(PrimitiveLiteral("3"), PrimitiveLiteral("4")))))
      }
      s2 in {
        parse(s2) shouldBe SwitchStatement(Select(Vector("data")), Vector(
          Switch.Cases(Vector(Switch.Default), Vector(
            Binding(Select(Vector("packet")), Instantiation.Object(Vector(), ClassType(Vector(), None, "Packet", Vector()), Vector(Select(Vector("datagram"))))),
            FlowStatement.Break))))
      }
    }
    "labeled statements" - {
      "label: {}" in { parse("label: {}") shouldBe LabeledStatement("label", Block(Vector)) }
    }
    "conditional statements" - {
      "if(a) {}" in { parse("if(a) {}") shouldBe Conditional.If(Select("a"), Block(Vector)) }
      "if(a) {} else {}" in { parse("if(a) {} else {}") shouldBe Conditional.IfThenElse(Select("a"), Block(Vector), Block(Vector)) }
      "if(a) if(b) {} else {}" in { parse("if(a) if(b) {} else {}") shouldBe Conditional.If(Select("a"), Conditional.IfThenElse(Select("b"), Block(Vector), Block(Vector))) }
      "if(a) if(b) {} else {} else{}" in { parse("if(a) if(b) {} else {} else{}") shouldBe Conditional.IfThenElse(Select("a"), Conditional.IfThenElse(Select("b"), Block(Vector), Block(Vector)), Block(Vector)) }
    }
    "loops" - {
      "for(;;);" in { parse("for(;;);") shouldBe Loop.For(Vector, None, Vector, EmptyStatement) }
      "for (;a instanceof Object[];);" in { parse("for (;a instanceof Object[];);") shouldBe Loop.For(Vector, Some(BinaryOperationChain(BinaryOperator.instanceof, Vector(Select("a"), ArrayType(ClassType(Vector, None, "Object", Vector), Seq(AbstractDimension(Vector)))))), Vector, EmptyStatement) }
      "for(f(); i < 10; ++i)" in { parse("for(f(); i < 10; ++i){}") shouldBe Loop.For(Vector(MethodInvocation(Vector, "f", Vector)), BinaryOperationChain(BinaryOperator.<, Vector(Select("i"), PrimitiveLiteral("10"))), Vector(PreIncrementation(Select("i"))), Block(Vector)) }
      "for(int i = 0, j = i; i < 10; ++i, j++){}" in { parse("for(int i = 0, j=i; i < 10; ++i, j++){}") shouldBe Loop.For(Vector(Declaration.LocalVariable(Vector, false, PrimitiveType.Integer(Vector), Vector(Declarator.InitializedVariable("i", PrimitiveLiteral("0")), Declarator.InitializedVariable("j", Select("i"))))), BinaryOperationChain(BinaryOperator.<, Vector(Select("i"), PrimitiveLiteral("10"))), Vector(PreIncrementation(Select("i")), PostIncrementation(Select("j"))), Block(Vector)) }
      "for(X x :xs){}" in { parse("for(X x :xs){}") shouldBe Loop.Iteration(Declaration.LocalVariable(Vector, false, ClassType(Vector, None, "X", Vector), Vector(Declarator.Variable("x"))), Select("xs"), Block(Vector)) }
      "do ++a; while (b);" in { parse("do ++a; while (b);") shouldBe Loop.DoWhile(PreIncrementation(Select("a")), Select("b")) }
      "while (b) ++a;" in { parse("while (b) ++a;") shouldBe Loop.While(Select("b"), PreIncrementation(Select("a"))) }
    }

  }
}