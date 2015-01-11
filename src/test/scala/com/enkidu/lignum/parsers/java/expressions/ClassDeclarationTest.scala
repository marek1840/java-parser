package com.enkidu.lignum.parsers.java.expressions

import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.ast.expression.discardable.Select
import com.enkidu.lignum.parsers.ast.expression.discardable.binary.MethodInvocation
import com.enkidu.lignum.parsers.ast.expression.discardable.dimension.AbstractDimension
import com.enkidu.lignum.parsers.ast.expression.discardable.instantiation.SimpleObjectInstantiation
import com.enkidu.lignum.parsers.ast.expression.discardable.literals.{CharLiteral, IntegerLiteral}
import com.enkidu.lignum.parsers.ast.expression.types.annotations.MarkerAnnotation
import com.enkidu.lignum.parsers.ast.expression.types.coupled.ChildOfAll
import com.enkidu.lignum.parsers.ast.expression.types.primitives.{BytePrimitive, IntegerPrimitive}
import com.enkidu.lignum.parsers.ast.expression.types.references.{ArrayType, ClassType}
import com.enkidu.lignum.parsers.ast.expression.types.templates.{ArgumentTemplate, BoundedParameterTemplate, ParameterTemplate}
import com.enkidu.lignum.parsers.ast.statement._
import com.enkidu.lignum.parsers.ast.statement.constructor.{AlternateConstructorInvocation, IndirectParentConstructorInvocation, IntermidiateConstructorInvocation, ParentConstructorInvocation}
import com.enkidu.lignum.parsers.ast.statement.declaration.initializers.{InstanceInitializerDeclaration, StaticInitializerDeclaration}
import com.enkidu.lignum.parsers.ast.statement.declaration.members._
import com.enkidu.lignum.parsers.ast.statement.declaration.types.{ClassDeclaration, EmptyDeclaration, EnumDeclaration, TypeDeclaration}
import com.enkidu.lignum.parsers.ast.statement.declarator._
import com.enkidu.lignum.parsers.ast.statement.modifers._
import com.enkidu.lignum.parsers.ast.statement.parameter.{FormalParameter, InstanceReceiverParameter, NestedReceiverParameter, VariableArityParameter}
import com.enkidu.lignum.parsers.java.v8.JavaCompilationUnitParser

class ClassDeclarationTest extends ParserTest {
  def parse(string: String): TypeDeclaration = {
    implicit val parser = new JavaCompilationUnitParser(string)
    get(parser.typeDeclaration.run())
  }

  val f0 = """class A{int a;}"""
  val f1 = """class A{@A int a;}"""
  val f2 = """class A{public int a;}"""
  val f3 = """class A{protected int a;}"""
  val f4 = """class A{private  int a;}"""
  val f5 = """class A{static int a;}"""
  val f6 = """class A{final int a;}"""
  val f7 = """class A{transient int a;}"""
  val f8 = """class A{volatile int a;}"""
  val f9 = """class A{@A volatile int a;}"""
  val f10 = """class A{int a, b;}"""

  val m0 = "class A{int f(){}}"
  val m1 = "class A{public int f(){}}"
  val m2 = "class A{protected int f(){}}"
  val m3 = "class A{private  int f(){}}"
  val m4 = "class A{abstract int f(){}}"
  val m5 = "class A{static int f(){}}"
  val m6 = "class A{final int f(){}}"
  val m7 = "class A{synchronized int f(){}}"
  val m8 = "class A{native int f(){}}"
  val m9 = "class A{strictfp int f(){}}"
  val m10 = "class A{@A int f(){}}"
  val m11 = "class A{<T> int f(){}}"
  val m12 = "class A{int f()[]{}}"
  val m13 = "class A{int f() throws A, B{}}"
  val m14 = "class A{int f();}"
  val m15 = "class A{int f(int a){}}"
  val m16 = "class A{int f(int... a){}}"
  val m17 = "class A{int f(int a, int b[]){}}"
  val m18 = "class A{int f(int a, int... b[]){}}"
  val m19 = "class A{int f(@A A this){}}"
  val m20 = "class A{int f(@A A A.this){}}"

  val c1 = "class A{A(){}}"
  val c2 = "class A{private A(){}}"
  val c3 = "class A{protected A(){}}"
  val c4 = "class A{public A(){}}"
  val c5 = "class A{@A A(){}}"
  val c6 = "class A{A(int a){}}"
  val c7 = "class A{A(int a, int ... b[]){}}"
  val c8 = "class A{A(A this){}}"
  val c9 = "class A{A(A A.this){}}"
  val c10 = "class A{<T> A(){}}"
  val c11 = "class A{A() throws A{}}"
  val c12 = "class A{A(){this();}}"
  val c13 = "class A{A(){<T> this();}}"
  val c14 = "class A{A(){super();}}"
  val c15 = "class A{A(){<T> super();}}"
  val c16 = "class A{A(){A.super();}}"
  val c17 = "class A{A(){A.<T> super();}}"
  val c18 = "class A{A(){a().super();}}"
  val c19 = "class A{A(){a().<T> super();}}"
  val c20 = "class A{A(){(new A()). super();}}"
  val c21 = "class A{A(){} A(int a){}}"
  val c22 = "class A{private static final byte PAD = '=';}"

  def Class(body: MemberDeclaration*) = ClassDeclaration(Vector, Vector, "A", Vector, None, Vector, body.toVector)

  "Class Declaration parser should parse" - {
    "classes with empty body" - {
      "class A{}" in { parse("class A{}") shouldBe ClassDeclaration(Vector, Vector, "A", Vector, None, Vector, Vector) }
      "@A public class A{}" in { parse("@A public class A{}") shouldBe ClassDeclaration(Vector(MarkerAnnotation("A")), Vector(Public), "A", Vector, None, Vector, Vector) }
      "public class A{}" in { parse("public class A{}") shouldBe ClassDeclaration(Vector, Vector(Public), "A", Vector, None, Vector, Vector) }
      "protected class A{}" in { parse("protected class A{}") shouldBe ClassDeclaration(Vector, Vector(Protected), "A", Vector, None, Vector, Vector) }
      "private class A{}" in { parse("private class A{}") shouldBe ClassDeclaration(Vector, Vector(Private), "A", Vector, None, Vector, Vector) }
      "abstract class A{}" in { parse("abstract class A{}") shouldBe ClassDeclaration(Vector, Vector(Abstract), "A", Vector, None, Vector, Vector) }
      "static class A{}" in { parse("static class A{}") shouldBe ClassDeclaration(Vector, Vector(Static), "A", Vector, None, Vector, Vector) }
      "final class A{}" in { parse("final class A{}") shouldBe ClassDeclaration(Vector, Vector(Final), "A", Vector, None, Vector, Vector) }
      "strictfp class A{}" in { parse("strictfp class A{}") shouldBe ClassDeclaration(Vector, Vector(Strictfp), "A", Vector, None, Vector, Vector) }
      "class A extends B{}" in { parse("class A extends B{}") shouldBe ClassDeclaration(Vector, Vector, "A", Vector, ClassType(Vector, None, "B", Vector), Vector, Vector) }
      "class A implements B{}" in { parse("class A implements B{}") shouldBe ClassDeclaration(Vector, Vector, "A", Vector, None, Vector(ClassType(Vector, None, "B", Vector)), Vector) }
      "class A extends B implements C, D{}" in { parse("class A extends B implements C, D{}") shouldBe ClassDeclaration(Vector, Vector, "A", Vector, ClassType(Vector, None, "B", Vector), Vector(ClassType(Vector, None, "C", Vector), ClassType(Vector, None, "D", Vector)), Vector) }
      "class A<T, U extends V, @A X extends V & I1& I2> extends B{}" in { parse("class A<T, U extends V, @A X extends V & I1& I2> extends B{}") shouldBe ClassDeclaration(Vector, Vector, "A", Vector(ParameterTemplate(Vector, "T"), BoundedParameterTemplate(Vector, "U", ClassType(Vector, None, "V", Vector)), BoundedParameterTemplate(Vector(MarkerAnnotation("A")), "X", ChildOfAll(Vector(ClassType(Vector, None, "V", Vector), ClassType(Vector, None, "I1", Vector), ClassType(Vector, None, "I2", Vector))))), ClassType(Vector, None, "B", Vector), Vector, Vector) }
    }

    "classes with members" - {
      "class A{;}" in { parse("class A{;}") shouldBe Class(EmptyDeclaration) }
      "fields" - {
        f0 in { parse(f0) shouldBe Class(FieldDeclaration(Vector, Vector(), IntegerPrimitive(Vector), Vector(VariableDeclarator("a")))) }
        f1 in { parse(f1) shouldBe Class(FieldDeclaration(Vector(MarkerAnnotation("A")), Vector, IntegerPrimitive(Vector), Vector(VariableDeclarator("a")))) }
        f2 in { parse(f2) shouldBe Class(FieldDeclaration(Vector, Vector(Public), IntegerPrimitive(Vector), Vector(VariableDeclarator("a")))) }
        f3 in { parse(f3) shouldBe Class(FieldDeclaration(Vector, Vector(Protected), IntegerPrimitive(Vector), Vector(VariableDeclarator("a")))) }
        f4 in { parse(f4) shouldBe Class(FieldDeclaration(Vector, Vector(Private), IntegerPrimitive(Vector), Vector(VariableDeclarator("a")))) }
        f5 in { parse(f5) shouldBe Class(FieldDeclaration(Vector, Vector(Static), IntegerPrimitive(Vector), Vector(VariableDeclarator("a")))) }
        f6 in { parse(f6) shouldBe Class(FieldDeclaration(Vector, Vector(Final), IntegerPrimitive(Vector), Vector(VariableDeclarator("a")))) }
        f7 in { parse(f7) shouldBe Class(FieldDeclaration(Vector, Vector(Transient), IntegerPrimitive(Vector), Vector(VariableDeclarator("a")))) }
        f8 in { parse(f8) shouldBe Class(FieldDeclaration(Vector, Vector(Volatile), IntegerPrimitive(Vector), Vector(VariableDeclarator("a")))) }
        f9 in { parse(f9) shouldBe Class(FieldDeclaration(Vector(MarkerAnnotation("A")), Vector(Volatile), IntegerPrimitive(Vector), Vector(VariableDeclarator("a")))) }
        f10 in { parse(f10) shouldBe Class(FieldDeclaration(Vector, Vector(), IntegerPrimitive(Vector), Vector(VariableDeclarator("a"), VariableDeclarator("b")))) }
      }
      "methods" - {
        m0 in {
          parse(m0) shouldBe Class(MethodDeclaration(
            Vector(),
            Vector(),
            Vector(),
            IntegerPrimitive(Vector),
            MethodDeclarator(
              "f",
              Vector()),
            Vector(),
            Block(Vector)))
        }
        m1 in {
          parse(m1) shouldBe Class(MethodDeclaration(
            Vector(),
            Vector(Public),
            Vector(),
            IntegerPrimitive(Vector),
            MethodDeclarator(
              "f",
              Vector()),
            Vector(),
            Block(Vector)))
        }
        m2 in {
          parse(m2) shouldBe Class(MethodDeclaration(
            Vector(),
            Vector(Protected),
            Vector(),
            IntegerPrimitive(Vector),
            MethodDeclarator(
              "f",
              Vector()),
            Vector(),
            Block(Vector)))
        }
        m3 in {
          parse(m3) shouldBe Class(MethodDeclaration(
            Vector(),
            Vector(Private),
            Vector(),
            IntegerPrimitive(Vector),
            MethodDeclarator(
              "f",
              Vector()),
            Vector(),
            Block(Vector)))
        }
        m4 in {
          parse(m4) shouldBe Class(MethodDeclaration(
            Vector(),
            Vector(Abstract),
            Vector(),
            IntegerPrimitive(Vector),
            MethodDeclarator(
              "f",
              Vector()),
            Vector(),
            Block(Vector)))
        }
        m5 in {
          parse(m5) shouldBe Class(MethodDeclaration(
            Vector(),
            Vector(Static),
            Vector(),
            IntegerPrimitive(Vector),
            MethodDeclarator(
              "f",
              Vector()),
            Vector(),
            Block(Vector)))
        }
        m6 in {
          parse(m6) shouldBe Class(MethodDeclaration(
            Vector(),
            Vector(Final),
            Vector(),
            IntegerPrimitive(Vector),
            MethodDeclarator(
              "f",
              Vector()),
            Vector(),
            Block(Vector)))
        }
        m7 in {
          parse(m7) shouldBe Class(MethodDeclaration(
            Vector(),
            Vector(Synchronized),
            Vector(),
            IntegerPrimitive(Vector),
            MethodDeclarator(
              "f",
              Vector()),
            Vector(),
            Block(Vector)))
        }
        m8 in {
          parse(m8) shouldBe Class(MethodDeclaration(
            Vector(),
            Vector(Native),
            Vector(),
            IntegerPrimitive(Vector),
            MethodDeclarator(
              "f",
              Vector()),
            Vector(),
            Block(Vector)))
        }
        m9 in {
          parse(m9) shouldBe Class(MethodDeclaration(
            Vector(),
            Vector(Strictfp),
            Vector(),
            IntegerPrimitive(Vector),
            MethodDeclarator(
              "f",
              Vector()),
            Vector(),
            Block(Vector)))
        }
        m10 in {
          parse(m10) shouldBe Class(MethodDeclaration(
            Vector(MarkerAnnotation("A")),
            Vector(),
            Vector(),
            IntegerPrimitive(Vector),
            MethodDeclarator(
              "f",
              Vector()),
            Vector(),
            Block(Vector)))
        }
        m11 in {
          parse(m11) shouldBe Class(MethodDeclaration(
            Vector(),
            Vector(),
            Vector(ParameterTemplate(Vector, "T")),
            IntegerPrimitive(Vector),
            MethodDeclarator(
              "f",
              Vector()),
            Vector(),
            Block(Vector)))
        }
        m12 in {
          parse(m12) shouldBe Class(MethodDeclaration(
            Vector(),
            Vector(),
            Vector(),
            IntegerPrimitive(Vector),
            ArrayMethodDeclarator(
              "f",
              Vector(),
              Vector(AbstractDimension(Vector))),
            Vector(),
            Block(Vector)))
        }
        m13 in {
          parse(m13) shouldBe Class(MethodDeclaration(
            Vector(),
            Vector(),
            Vector(),
            IntegerPrimitive(Vector),
            MethodDeclarator(
              "f",
              Vector()),
            Vector(
              ClassType(Vector, None, "A", Vector),
              ClassType(Vector, None, "B", Vector)),
            Block(Vector)))
        }
        m14 in {
          parse(m14) shouldBe Class(MethodDeclaration(
            Vector(),
            Vector(),
            Vector(),
            IntegerPrimitive(Vector),
            MethodDeclarator(
              "f",
              Vector()),
            Vector(),
            EmptyStatement))
        }
        m15 in {
          parse(m15) shouldBe Class(MethodDeclaration(
            Vector(),
            Vector(),
            Vector(),
            IntegerPrimitive(Vector),
            MethodDeclarator(
              "f",
              Vector(FormalParameter(Vector, false, IntegerPrimitive(Vector), "a"))),
            Vector(),
            Block(Vector)))
        }
        m16 in {
          parse(m16) shouldBe Class(MethodDeclaration(
            Vector(),
            Vector(),
            Vector(),
            IntegerPrimitive(Vector),
            MethodDeclarator(
              "f",
              Vector(VariableArityParameter(Vector, false, IntegerPrimitive(Vector), "a"))),
            Vector(),
            Block(Vector)))
        }
        m17 in {
          parse(m17) shouldBe Class(MethodDeclaration(
            Vector(),
            Vector(),
            Vector(),
            IntegerPrimitive(Vector),
            MethodDeclarator(
              "f",
              Vector(
                FormalParameter(Vector, false, IntegerPrimitive(Vector), "a"),
                FormalParameter(Vector, false, ArrayType(IntegerPrimitive(Vector), Vector(AbstractDimension(Vector))), "b"))),
            Vector(),
            Block(Vector)))
        }
        m18 in {
          parse(m18) shouldBe Class(MethodDeclaration(
            Vector(),
            Vector(),
            Vector(),
            IntegerPrimitive(Vector),
            MethodDeclarator(
              "f",
              Vector(FormalParameter(Vector, false, IntegerPrimitive(Vector), "a"),
                VariableArityParameter(Vector, false, ArrayType(IntegerPrimitive(Vector), Vector(AbstractDimension(Vector))), "b"))),
            Vector(),
            Block(Vector)))
        }
        m19 in {
          parse(m19) shouldBe Class(MethodDeclaration(
            Vector(),
            Vector(),
            Vector(),
            IntegerPrimitive(Vector),
            MethodDeclarator(
              "f",
              Vector(InstanceReceiverParameter(Vector(MarkerAnnotation("A")), ClassType(Vector, None, "A", Vector)))),
            Vector(),
            Block(Vector)))
        }
        m20 in {
          parse(m20) shouldBe Class(MethodDeclaration(
            Vector(),
            Vector(),
            Vector(),
            IntegerPrimitive(Vector),
            MethodDeclarator(
              "f",
              Vector(NestedReceiverParameter(Vector(MarkerAnnotation("A")), ClassType(Vector, None, "A", Vector), "A"))),
            Vector(),
            Block(Vector)))
        }
      }
      "initializers" - {
        "class A{{}}" in { parse("class A{{}}") shouldBe Class(InstanceInitializerDeclaration(Block(Vector))) }
        "class A{static {}}" in { parse("class A{static {}}") shouldBe Class(StaticInitializerDeclaration(Block(Vector))) }
      }
      "constructors" - {
        c1 in {
          parse(c1) shouldBe Class(ConstructorDeclaration(
            Vector(),
            Vector(),
            Vector(),
            ConstructorDeclarator("A", Vector()),
            Vector(),
            Block(Vector)))
        }
        c2 in {
          parse(c2) shouldBe Class(ConstructorDeclaration(
            Vector(),
            Vector(Private),
            Vector(),
            ConstructorDeclarator("A", Vector()),
            Vector(),
            Block(Vector)))
        }
        c3 in {
          parse(c3) shouldBe Class(ConstructorDeclaration(
            Vector(),
            Vector(Protected),
            Vector(),
            ConstructorDeclarator("A", Vector()),
            Vector(),
            Block(Vector)))
        }
        c4 in {
          parse(c4) shouldBe Class(ConstructorDeclaration(
            Vector(),
            Vector(Public),
            Vector(),
            ConstructorDeclarator("A", Vector()),
            Vector(),
            Block(Vector)))
        }
        c5 in {
          parse(c5) shouldBe Class(ConstructorDeclaration(
            Vector(MarkerAnnotation("A")),
            Vector(),
            Vector(),
            ConstructorDeclarator("A", Vector()),
            Vector(),
            Block(Vector)))
        }
        c6 in {
          parse(c6) shouldBe Class(ConstructorDeclaration(
            Vector(),
            Vector(),
            Vector(),
            ConstructorDeclarator("A", Vector(FormalParameter(Vector, false, IntegerPrimitive(Vector), "a"))),
            Vector(),
            Block(Vector)))
        }
        c7 in {
          parse(c7) shouldBe Class(ConstructorDeclaration(
            Vector(),
            Vector(),
            Vector(),
            ConstructorDeclarator("A", Vector(
              FormalParameter(Vector, false, IntegerPrimitive(Vector), "a"),
              VariableArityParameter(Vector, false, ArrayType(IntegerPrimitive(Vector), AbstractDimension(Vector)), "b"))),
            Vector(),
            Block(Vector)))
        }
        c8 in {
          parse(c8) shouldBe Class(ConstructorDeclaration(
            Vector(),
            Vector(),
            Vector(),
            ConstructorDeclarator("A", Vector(InstanceReceiverParameter(Vector, ClassType(Vector, None, "A", Vector)))),
            Vector(),
            Block(Vector)))
        }
        c9 in {
          parse(c9) shouldBe Class(ConstructorDeclaration(
            Vector(),
            Vector(),
            Vector(),
            ConstructorDeclarator("A", Vector(NestedReceiverParameter(Vector, ClassType(Vector, None, "A", Vector), "A"))),
            Vector(),
            Block(Vector)))
        }
        c10 in {
          parse(c10) shouldBe Class(ConstructorDeclaration(
            Vector(),
            Vector(),
            Vector(ParameterTemplate(Vector, "T")),
            ConstructorDeclarator("A", Vector()),
            Vector(),
            Block(Vector)))
        }
        c11 in {
          parse(c11) shouldBe Class(ConstructorDeclaration(
            Vector(),
            Vector(),
            Vector(),
            ConstructorDeclarator("A", Vector()),
            Vector(ClassType(Vector, None, "A", Vector)),
            Block(Vector)))
        }
        c12 in {
          parse(c12) shouldBe Class(ConstructorDeclaration(
            Vector(),
            Vector(),
            Vector(),
            ConstructorDeclarator("A", Vector()),
            Vector(),
            Block(Vector(
              AlternateConstructorInvocation(Vector, Vector)))))
        }
        c13 in {
          parse(c13) shouldBe Class(ConstructorDeclaration(
            Vector(),
            Vector(),
            Vector(),
            ConstructorDeclarator("A", Vector()),
            Vector(),
            Block(Vector(
              AlternateConstructorInvocation(Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), Vector)))))
        }
        c14 in {
          parse(c14) shouldBe Class(ConstructorDeclaration(
            Vector(),
            Vector(),
            Vector(),
            ConstructorDeclarator("A", Vector()),
            Vector(),
            Block(Vector(
              ParentConstructorInvocation(Vector, Vector)))))
        }
        c15 in {
          parse(c15) shouldBe Class(ConstructorDeclaration(
            Vector(),
            Vector(),
            Vector(),
            ConstructorDeclarator("A", Vector()),
            Vector(),
            Block(Vector(
              ParentConstructorInvocation(Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), Vector)))))
        }
        c16 in {
          parse(c16) shouldBe Class(ConstructorDeclaration(
            Vector(),
            Vector(),
            Vector(),
            ConstructorDeclarator("A", Vector()),
            Vector(),
            Block(Vector(
              IndirectParentConstructorInvocation(Select("A"), Vector, Vector)))))
        }
        c17 in {
          parse(c17) shouldBe Class(ConstructorDeclaration(
            Vector(),
            Vector(),
            Vector(),
            ConstructorDeclarator("A", Vector()),
            Vector(),
            Block(Vector(
              IndirectParentConstructorInvocation(Select("A"), ArgumentTemplate(ClassType(Vector, None, "T", Vector)), Vector)))))
        }
        c18 in {
          parse(c18) shouldBe Class(ConstructorDeclaration(
            Vector(),
            Vector(),
            Vector(),
            ConstructorDeclarator("A", Vector()),
            Vector(),
            Block(Vector(
              IntermidiateConstructorInvocation(MethodInvocation(Vector, "a", Vector), Vector, Vector)))))
        }
        c19 in {
          parse(c19) shouldBe Class(ConstructorDeclaration(
            Vector(),
            Vector(),
            Vector(),
            ConstructorDeclarator("A", Vector()),
            Vector(),
            Block(Vector(
              IntermidiateConstructorInvocation(MethodInvocation(Vector, "a", Vector), ArgumentTemplate(ClassType(Vector, None, "T", Vector)), Vector)))))
        }
        c20 in {
          parse(c20) shouldBe Class(ConstructorDeclaration(
            Vector(),
            Vector(),
            Vector(),
            ConstructorDeclarator("A", Vector()),
            Vector(),
            Block(Vector(
              IntermidiateConstructorInvocation(SimpleObjectInstantiation(Vector, ClassType(Vector, None, "A", Vector), 
              Vector), Vector, Vector)))))
        }
        c21 in {
          parse(c21) shouldBe Class(
            ConstructorDeclaration(
              Vector(),
              Vector(),
              Vector(),
              ConstructorDeclarator(
                "A",
                Vector()),
              Vector(),
              Block(Vector)),
            ConstructorDeclaration(
              Vector(),
              Vector(),
              Vector(),
              ConstructorDeclarator(
                "A",
                Vector(FormalParameter(Vector, false, IntegerPrimitive(Vector), "a"))),
              Vector(),
              Block(Vector)))
        }
        c22 in {
          parse(c22) shouldBe Class(FieldDeclaration(
            Vector,
            Vector(Private, Static, Final),
            BytePrimitive(Vector),
            InitializedVariableDeclarator(
              "PAD", CharLiteral("'='"))))
        }
      }
      "class declarations" - {
        "class A{class B{}}" in {
          parse("class A{class B{}}") shouldBe Class(ClassDeclaration(
            Vector, Vector, "B", Vector, None, Vector, Vector))
        }
      }
      "interface declarations" - {
        //TODO
      }
    }

    "enums" - {
      "@A enum A{}" in {
        parse("@A enum A{}") shouldBe EnumDeclaration(
          Vector(MarkerAnnotation("A")),
          Vector,
          "A",
          Vector,
          Vector)
      }
      "public enum A{}" in {
        parse("public enum A{}") shouldBe EnumDeclaration(
          Vector,
          Vector(Public),
          "A",
          Vector,
          Vector)
      }
      "protected enum A{}" in {
        parse("protected enum A{}") shouldBe EnumDeclaration(
          Vector,
          Vector(Protected),
          "A",
          Vector,
          Vector)
      }
      "private enum A{}" in {
        parse("private enum A{}") shouldBe EnumDeclaration(
          Vector,
          Vector(Private),
          "A",
          Vector,
          Vector)
      }
      "abstract enum A{}" in {
        parse("abstract enum A{}") shouldBe EnumDeclaration(
          Vector,
          Vector(Abstract),
          "A",
          Vector,
          Vector)
      }
      "static enum A{}" in {
        parse("static enum A{}") shouldBe EnumDeclaration(
          Vector,
          Vector(Static),
          "A",
          Vector,
          Vector)
      }
      "final enum A{}" in {
        parse("final enum A{}") shouldBe EnumDeclaration(
          Vector,
          Vector(Final),
          "A",
          Vector,
          Vector)
      }
      "strictfp enum A{}" in {
        parse("strictfp enum A{}") shouldBe EnumDeclaration(
          Vector,
          Vector(Strictfp),
          "A",
          Vector,
          Vector)
      }
      "enum A implements A{}" in {
        parse("enum A implements A{}") shouldBe EnumDeclaration(
          Vector,
          Vector,
          "A",
          Vector(ClassType(Vector, None, "A", Vector)),
          Vector)
      }
      "enum A{A,B,C}" in {
        parse("enum A{A,B,C}") shouldBe EnumDeclaration(
          Vector,
          Vector,
          "A",
          Vector,
          Vector(EnumConstantDeclaration(Vector, "A", Vector), EnumConstantDeclaration(Vector, "B", Vector), EnumConstantDeclaration(Vector, "C", Vector)))
      }
      "enum A{A(1)}" in {
        parse("enum A{A(1)}") shouldBe EnumDeclaration(
          Vector,
          Vector,
          "A",
          Vector,
          Vector(EnumConstantDeclaration(Vector, "A", Vector(IntegerLiteral("1")))))
      }
      "enum A{A{}}" in {
        parse("enum A{A{}}") shouldBe EnumDeclaration(
          Vector,
          Vector,
          "A",
          Vector,
          Vector(AnonymousEnumConstantDeclaration(Vector, "A", Vector, Vector)))
      }
    }
  }
}