package com.enkidu.lignum.parsers.java.expressions

import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.ast.expression._
import com.enkidu.lignum.parsers.ast.expression.discardable.binary.assignment.Binding
import com.enkidu.lignum.parsers.ast.expression.discardable.binary.{MethodInvocation, QualifiedMethodInvocation}
import com.enkidu.lignum.parsers.ast.expression.discardable.dimension.AbstractDimension
import com.enkidu.lignum.parsers.ast.expression.discardable.instantiation.SimpleObjectInstantiation
import com.enkidu.lignum.parsers.ast.expression.discardable.literals.IntegerLiteral
import com.enkidu.lignum.parsers.ast.expression.discardable.unary.{PostDecrementation, PostIncrementation, PreDecrementation, PreIncrementation}
import com.enkidu.lignum.parsers.ast.expression.discardable.{Select, ThisReference}
import com.enkidu.lignum.parsers.ast.expression.operators.BinaryOperator
import com.enkidu.lignum.parsers.ast.expression.types.annotations.MarkerAnnotation
import com.enkidu.lignum.parsers.ast.expression.types.primitives.IntegerPrimitive
import com.enkidu.lignum.parsers.ast.expression.types.references.{ArrayType, ClassType}
import com.enkidu.lignum.parsers.ast.statement._
import com.enkidu.lignum.parsers.ast.statement.conditional.{If, IfThenElse}
import com.enkidu.lignum.parsers.ast.statement.declaration.LocalVariableDeclaration
import com.enkidu.lignum.parsers.ast.statement.declarator.{InitializedArrayDeclarator, InitializedVariableDeclarator, VariableDeclarator}
import com.enkidu.lignum.parsers.ast.statement.flow._
import com.enkidu.lignum.parsers.ast.statement.interruptable._
import com.enkidu.lignum.parsers.ast.statement.loop._
import com.enkidu.lignum.parsers.ast.statement.switch.{DefaultSwitch, EmptySwitchCases, SwitchCases, SwitchStatement}
import com.enkidu.lignum.parsers.java.v8.JavaCompilationUnitParser

class StatementsTest extends ParserTest {
  def parse(string: String): Statement = {
    implicit val parser = new JavaCompilationUnitParser(string)
    get(parser.blockStatement.run())
  }

  "Statement parser should parse" - {
    "local variable declaration" - {
      "int a;" in { parse("int a;") shouldBe LocalVariableDeclaration(Vector(), false, IntegerPrimitive(Vector), Vector(VariableDeclarator("a"))) }
      "@A  int a;" in { parse("@A  int a;") shouldBe LocalVariableDeclaration(Vector(MarkerAnnotation("A")), false, IntegerPrimitive(Vector), Vector(VariableDeclarator("a"))) }
      "final int a;" in { parse("final int a;") shouldBe LocalVariableDeclaration(Vector(), true, IntegerPrimitive(Vector), Vector(VariableDeclarator("a"))) }
      "@A final int a;" in { parse("@A final int a;") shouldBe LocalVariableDeclaration(Vector(MarkerAnnotation("A")), true, IntegerPrimitive(Vector), Vector(VariableDeclarator("a"))) }
      "@A final @A int a;" in { parse("@A final @A int a;") shouldBe LocalVariableDeclaration(Vector(MarkerAnnotation("A"), MarkerAnnotation("A")), true, IntegerPrimitive(Vector), Vector(VariableDeclarator("a"))) }

      "int a, y[] = x;" in { parse("int a, y[] = x;") shouldBe LocalVariableDeclaration(Vector(), false, IntegerPrimitive(Vector), Vector(VariableDeclarator("a"), InitializedArrayDeclarator("y", Vector(AbstractDimension(Vector)), Select("x")))) }
      "Class a, b = new Class(), c;" in { parse("Class a, b = new Class(), c;") shouldBe LocalVariableDeclaration(Vector(), false, ClassType(Vector, None, "Class", Vector), Vector(VariableDeclarator("a"), InitializedVariableDeclarator("b", SimpleObjectInstantiation(Vector, ClassType(Vector, None, "Class", Vector), Vector)), VariableDeclarator("c"))) }
    }
    "discardable expression statements" - {
      "a = b;" in { parse("a = b;") shouldBe Binding(Select("a"), Select("b")) }
      "a++;" in { parse("a++;") shouldBe PostIncrementation(Select("a")) }
      "a--;" in { parse("a--;") shouldBe PostDecrementation(Select("a")) }
      "--a;" in { parse("--a;") shouldBe PreDecrementation(Select("a")) }
      "++a;" in { parse("++a;") shouldBe PreIncrementation(Select("a")) }
      "a().f();" in { parse("a().f();") shouldBe QualifiedMethodInvocation(MethodInvocation(Vector, "a", Vector), Vector, "f", Vector) }
      "new Class();" in { parse("new Class();") shouldBe SimpleObjectInstantiation(Vector, ClassType(Vector, None, "Class", Vector),
        Vector) }
    }
    "assertions" - {
      "assert x;" in { parse("assert x;") shouldBe Assertion(Select("x"), None) }
      "assert x: y;" in { parse("assert x: y;") shouldBe Assertion(Select("x"), Some(Select("y"))) }
    }
    "flow statements" - {
      "break;" in { parse("break;") shouldBe Break }
      "break a;" in { parse("break a;") shouldBe TargetedBreak("a") }
      "continue;" in { parse("continue;") shouldBe Continue }
      "continue a;" in { parse("continue a;") shouldBe TargetedContinue("a") }
      "return;" in { parse("return;") shouldBe EmptyReturn }
      "return a;" in { parse("return a;") shouldBe Return(Select("a")) }
      "throw a;" in { parse("throw a;") shouldBe Throw(Select("a")) }
    }
    "synchronized blocks" - {
      "synchronized {int x;}" in { parse("synchronized {int x;}") shouldBe SynchronizedBlock(None, Block(LocalVariableDeclaration(Vector, false, IntegerPrimitive(Vector), Vector(VariableDeclarator("x"))))) }
      "synchronized(this) {int x;}" in { parse("synchronized(this) {int x;}") shouldBe SynchronizedBlock(Some(ThisReference), Block(LocalVariableDeclaration(Vector, false, IntegerPrimitive(Vector), Vector(VariableDeclarator("x"))))) }
    }
    "try statements" - {
      val tb = Block(Vector(LocalVariableDeclaration(Vector, false, IntegerPrimitive(Vector), Vector(VariableDeclarator("x")))))
      val fb = Block(Vector())
      val cs = Vector(CatchClause(LocalVariableDeclaration(Vector, true, ClassType(Vector, None, "Exception", Vector), VariableDeclarator("e")), Block(Vector)))

      "try {int x;} catch(Exception e){}" in { parse("try {int x;} catch(Exception e){}") shouldBe TryCatch(tb, cs) }
      "try {int x;} finally{}" in { parse("try {int x;} finally{}") shouldBe TryFinally(tb, fb) }
      "try {int x;} catch(Exception e){} finally{}" in { parse("try {int x;} catch(Exception e){} finally{}") shouldBe TryCatchFinally(tb, cs, fb) }
      "try(C c = new C()){int x;} catch(Exception e){} finally{}" in { parse("try(C c = new C()){int x;} catch(Exception e){} finally{}") shouldBe TryWithResources(Vector(LocalVariableDeclaration(Vector, true, ClassType(Vector, None, "C", Vector), Vector(InitializedVariableDeclarator("c", SimpleObjectInstantiation(Vector, ClassType(Vector, None, "C", Vector), Vector))))), tb, cs, Some(fb)) }
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
          SwitchCases(Vector(IntegerLiteral("0"), IntegerLiteral("1")), EmptyStatement),
          SwitchCases(Vector(DefaultSwitch), Break),
          SwitchCases(Vector(Select(Vector("Enum", "Value"))), EmptyStatement),
          EmptySwitchCases(Vector(IntegerLiteral("3"), IntegerLiteral("4")))))
      }
      s2 in {
        parse(s2) shouldBe SwitchStatement(Select(Vector("data")), Vector(
          SwitchCases(Vector(DefaultSwitch), Vector(
            Binding(Select(Vector("packet")), SimpleObjectInstantiation(Vector(), ClassType(Vector(), None, "Packet", Vector()), Vector(Select(Vector("datagram"))))),
            Break))))
      }
    }
    "labeled statements" - {
      "label: {}" in { parse("label: {}") shouldBe LabeledStatement("label", Block(Vector)) }
    }
    "conditional statements" - {
      "if(a) {}" in { parse("if(a) {}") shouldBe If(Select("a"), Block(Vector)) }
      "if(a) {} else {}" in { parse("if(a) {} else {}") shouldBe IfThenElse(Select("a"), Block(Vector), Block(Vector)) }
      "if(a) if(b) {} else {}" in { parse("if(a) if(b) {} else {}") shouldBe If(Select("a"), IfThenElse(Select("b"), Block(Vector), Block(Vector))) }
      "if(a) if(b) {} else {} else{}" in { parse("if(a) if(b) {} else {} else{}") shouldBe IfThenElse(Select("a"), IfThenElse(Select("b"), Block(Vector), Block(Vector)), Block(Vector)) }
    }
    "loops" - {
      "for(;;);" in { parse("for(;;);") shouldBe For(Vector, None, Vector, EmptyStatement) }
      "for (;a instanceof Object[];);" in { parse("for (;a instanceof Object[];);") shouldBe For(Vector, Some(BinaryOperations(BinaryOperator.instanceof, Vector(Select("a"), ArrayType(ClassType(Vector, None, "Object", Vector), Seq(AbstractDimension(Vector)))))), Vector, EmptyStatement) }
      "for(f(); i < 10; ++i)" in { parse("for(f(); i < 10; ++i){}") shouldBe For(Vector(MethodInvocation(Vector, "f", Vector)), BinaryOperations(BinaryOperator.<, Vector(Select("i"), IntegerLiteral("10"))), Vector(PreIncrementation(Select("i"))), Block(Vector)) }
      "for(int i = 0, j = i; i < 10; ++i, j++){}" in { parse("for(int i = 0, j=i; i < 10; ++i, j++){}") shouldBe For(Vector(LocalVariableDeclaration(Vector, false, IntegerPrimitive(Vector), Vector(InitializedVariableDeclarator("i", IntegerLiteral("0")), InitializedVariableDeclarator("j", Select("i"))))), BinaryOperations(BinaryOperator.<, Vector(Select("i"), IntegerLiteral("10"))), Vector(PreIncrementation(Select("i")), PostIncrementation(Select("j"))), Block(Vector)) }
      "for(X x :xs){}" in { parse("for(X x :xs){}") shouldBe Iteration(LocalVariableDeclaration(Vector, false, ClassType(Vector, None, "X", Vector), Vector(VariableDeclarator("x"))), Select("xs"), Block(Vector)) }
      "do ++a; while (b);" in { parse("do ++a; while (b);") shouldBe DoWhile(PreIncrementation(Select("a")), Select("b")) }
      "while (b) ++a;" in { parse("while (b) ++a;") shouldBe While(Select("b"), PreIncrementation(Select("a"))) }
    }

  }
}