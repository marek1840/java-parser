package com.enkidu.lignum.parsers.java.expressions

import com.enkidu.lignum.parsers.types._
import com.enkidu.lignum.parsers.expressions._
import com.enkidu.lignum.parsers.statements._
import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.JavaParser

class ClassDeclarationTest extends ParserTest {
  def parse(string: String): TypeDeclaration = {
    implicit val parser = new JavaParser(string)
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

  def Class(body: MemberDeclaration*) = Declaration.Class(Vector, Vector, "A", Vector, None, Vector, body.toVector)

  "Class Declaration parser should parse" - {
    "classes with empty body" - {
      "class A{}" in { parse("class A{}") shouldBe Declaration.Class(Vector, Vector, "A", Vector, None, Vector, Vector) }
      "@A public class A{}" in { parse("@A public class A{}") shouldBe Declaration.Class(Vector(MarkerAnnotation("A")), Vector(Modifier.Public), "A", Vector, None, Vector, Vector) }
      "public class A{}" in { parse("public class A{}") shouldBe Declaration.Class(Vector, Vector(Modifier.Public), "A", Vector, None, Vector, Vector) }
      "protected class A{}" in { parse("protected class A{}") shouldBe Declaration.Class(Vector, Vector(Modifier.Protected), "A", Vector, None, Vector, Vector) }
      "private class A{}" in { parse("private class A{}") shouldBe Declaration.Class(Vector, Vector(Modifier.Private), "A", Vector, None, Vector, Vector) }
      "abstract class A{}" in { parse("abstract class A{}") shouldBe Declaration.Class(Vector, Vector(Modifier.Abstract), "A", Vector, None, Vector, Vector) }
      "static class A{}" in { parse("static class A{}") shouldBe Declaration.Class(Vector, Vector(Modifier.Static), "A", Vector, None, Vector, Vector) }
      "final class A{}" in { parse("final class A{}") shouldBe Declaration.Class(Vector, Vector(Modifier.Final), "A", Vector, None, Vector, Vector) }
      "strictfp class A{}" in { parse("strictfp class A{}") shouldBe Declaration.Class(Vector, Vector(Modifier.Strictfp), "A", Vector, None, Vector, Vector) }
      "class A extends B{}" in { parse("class A extends B{}") shouldBe Declaration.Class(Vector, Vector, "A", Vector, ClassType(Vector, None, "B", Vector), Vector, Vector) }
      "class A implements B{}" in { parse("class A implements B{}") shouldBe Declaration.Class(Vector, Vector, "A", Vector, None, Vector(ClassType(Vector, None, "B", Vector)), Vector) }
      "class A extends B implements C, D{}" in { parse("class A extends B implements C, D{}") shouldBe Declaration.Class(Vector, Vector, "A", Vector, ClassType(Vector, None, "B", Vector), Vector(ClassType(Vector, None, "C", Vector), ClassType(Vector, None, "D", Vector)), Vector) }
      "class A<T, U extends V, @A X extends V & I1& I2> extends B{}" in { parse("class A<T, U extends V, @A X extends V & I1& I2> extends B{}") shouldBe Declaration.Class(Vector, Vector, "A", Vector(Template.Parameter(Vector, "T"), Template.BoundedParameter(Vector, "U", ClassType(Vector, None, "V", Vector)), Template.BoundedParameter(Vector(MarkerAnnotation("A")), "X", ChildOfAll(Vector(ClassType(Vector, None, "V", Vector), ClassType(Vector, None, "I1", Vector), ClassType(Vector, None, "I2", Vector))))), ClassType(Vector, None, "B", Vector), Vector, Vector) }
    }

    "classes with members" - {
      "class A{;}" in { parse("class A{;}") shouldBe Class(Declaration.Empty) }
      "fields" - {
        f0 in { parse(f0) shouldBe Class(Declaration.Field(Vector, Vector(), PrimitiveType.Integer(Vector), Vector(Declarator.Variable("a")))) }
        f1 in { parse(f1) shouldBe Class(Declaration.Field(Vector(MarkerAnnotation("A")), Vector, PrimitiveType.Integer(Vector), Vector(Declarator.Variable("a")))) }
        f2 in { parse(f2) shouldBe Class(Declaration.Field(Vector, Vector(Modifier.Public), PrimitiveType.Integer(Vector), Vector(Declarator.Variable("a")))) }
        f3 in { parse(f3) shouldBe Class(Declaration.Field(Vector, Vector(Modifier.Protected), PrimitiveType.Integer(Vector), Vector(Declarator.Variable("a")))) }
        f4 in { parse(f4) shouldBe Class(Declaration.Field(Vector, Vector(Modifier.Private), PrimitiveType.Integer(Vector), Vector(Declarator.Variable("a")))) }
        f5 in { parse(f5) shouldBe Class(Declaration.Field(Vector, Vector(Modifier.Static), PrimitiveType.Integer(Vector), Vector(Declarator.Variable("a")))) }
        f6 in { parse(f6) shouldBe Class(Declaration.Field(Vector, Vector(Modifier.Final), PrimitiveType.Integer(Vector), Vector(Declarator.Variable("a")))) }
        f7 in { parse(f7) shouldBe Class(Declaration.Field(Vector, Vector(Modifier.Transient), PrimitiveType.Integer(Vector), Vector(Declarator.Variable("a")))) }
        f8 in { parse(f8) shouldBe Class(Declaration.Field(Vector, Vector(Modifier.Volatile), PrimitiveType.Integer(Vector), Vector(Declarator.Variable("a")))) }
        f9 in { parse(f9) shouldBe Class(Declaration.Field(Vector(MarkerAnnotation("A")), Vector(Modifier.Volatile), PrimitiveType.Integer(Vector), Vector(Declarator.Variable("a")))) }
        f10 in { parse(f10) shouldBe Class(Declaration.Field(Vector, Vector(), PrimitiveType.Integer(Vector), Vector(Declarator.Variable("a"), Declarator.Variable("b")))) }
      }
      "methods" - {
        m0 in {
          parse(m0) shouldBe Class(Declaration.Method(
            Vector(),
            Vector(),
            Vector(),
            PrimitiveType.Integer(Vector),
            Declarator.Method(
              "f",
              Vector()),
            Vector(),
            Block(Vector)))
        }
        m1 in {
          parse(m1) shouldBe Class(Declaration.Method(
            Vector(),
            Vector(Modifier.Public),
            Vector(),
            PrimitiveType.Integer(Vector),
            Declarator.Method(
              "f",
              Vector()),
            Vector(),
            Block(Vector)))
        }
        m2 in {
          parse(m2) shouldBe Class(Declaration.Method(
            Vector(),
            Vector(Modifier.Protected),
            Vector(),
            PrimitiveType.Integer(Vector),
            Declarator.Method(
              "f",
              Vector()),
            Vector(),
            Block(Vector)))
        }
        m3 in {
          parse(m3) shouldBe Class(Declaration.Method(
            Vector(),
            Vector(Modifier.Private),
            Vector(),
            PrimitiveType.Integer(Vector),
            Declarator.Method(
              "f",
              Vector()),
            Vector(),
            Block(Vector)))
        }
        m4 in {
          parse(m4) shouldBe Class(Declaration.Method(
            Vector(),
            Vector(Modifier.Abstract),
            Vector(),
            PrimitiveType.Integer(Vector),
            Declarator.Method(
              "f",
              Vector()),
            Vector(),
            Block(Vector)))
        }
        m5 in {
          parse(m5) shouldBe Class(Declaration.Method(
            Vector(),
            Vector(Modifier.Static),
            Vector(),
            PrimitiveType.Integer(Vector),
            Declarator.Method(
              "f",
              Vector()),
            Vector(),
            Block(Vector)))
        }
        m6 in {
          parse(m6) shouldBe Class(Declaration.Method(
            Vector(),
            Vector(Modifier.Final),
            Vector(),
            PrimitiveType.Integer(Vector),
            Declarator.Method(
              "f",
              Vector()),
            Vector(),
            Block(Vector)))
        }
        m7 in {
          parse(m7) shouldBe Class(Declaration.Method(
            Vector(),
            Vector(Modifier.Synchronized),
            Vector(),
            PrimitiveType.Integer(Vector),
            Declarator.Method(
              "f",
              Vector()),
            Vector(),
            Block(Vector)))
        }
        m8 in {
          parse(m8) shouldBe Class(Declaration.Method(
            Vector(),
            Vector(Modifier.Native),
            Vector(),
            PrimitiveType.Integer(Vector),
            Declarator.Method(
              "f",
              Vector()),
            Vector(),
            Block(Vector)))
        }
        m9 in {
          parse(m9) shouldBe Class(Declaration.Method(
            Vector(),
            Vector(Modifier.Strictfp),
            Vector(),
            PrimitiveType.Integer(Vector),
            Declarator.Method(
              "f",
              Vector()),
            Vector(),
            Block(Vector)))
        }
        m10 in {
          parse(m10) shouldBe Class(Declaration.Method(
            Vector(MarkerAnnotation("A")),
            Vector(),
            Vector(),
            PrimitiveType.Integer(Vector),
            Declarator.Method(
              "f",
              Vector()),
            Vector(),
            Block(Vector)))
        }
        m11 in {
          parse(m11) shouldBe Class(Declaration.Method(
            Vector(),
            Vector(),
            Vector(Template.Parameter(Vector, "T")),
            PrimitiveType.Integer(Vector),
            Declarator.Method(
              "f",
              Vector()),
            Vector(),
            Block(Vector)))
        }
        m12 in {
          parse(m12) shouldBe Class(Declaration.Method(
            Vector(),
            Vector(),
            Vector(),
            PrimitiveType.Integer(Vector),
            Declarator.ArrayMethod(
              "f",
              Vector(),
              Vector(AbstractDimension(Vector))),
            Vector(),
            Block(Vector)))
        }
        m13 in {
          parse(m13) shouldBe Class(Declaration.Method(
            Vector(),
            Vector(),
            Vector(),
            PrimitiveType.Integer(Vector),
            Declarator.Method(
              "f",
              Vector()),
            Vector(
              ClassType(Vector, None, "A", Vector),
              ClassType(Vector, None, "B", Vector)),
            Block(Vector)))
        }
        m14 in {
          parse(m14) shouldBe Class(Declaration.Method(
            Vector(),
            Vector(),
            Vector(),
            PrimitiveType.Integer(Vector),
            Declarator.Method(
              "f",
              Vector()),
            Vector(),
            EmptyStatement))
        }
        m15 in {
          parse(m15) shouldBe Class(Declaration.Method(
            Vector(),
            Vector(),
            Vector(),
            PrimitiveType.Integer(Vector),
            Declarator.Method(
              "f",
              Vector(Parameter.Formal(Vector, false, PrimitiveType.Integer(Vector), "a"))),
            Vector(),
            Block(Vector)))
        }
        m16 in {
          parse(m16) shouldBe Class(Declaration.Method(
            Vector(),
            Vector(),
            Vector(),
            PrimitiveType.Integer(Vector),
            Declarator.Method(
              "f",
              Vector(Parameter.VariableArity(Vector, false, PrimitiveType.Integer(Vector), "a"))),
            Vector(),
            Block(Vector)))
        }
        m17 in {
          parse(m17) shouldBe Class(Declaration.Method(
            Vector(),
            Vector(),
            Vector(),
            PrimitiveType.Integer(Vector),
            Declarator.Method(
              "f",
              Vector(
                Parameter.Formal(Vector, false, PrimitiveType.Integer(Vector), "a"),
                Parameter.Formal(Vector, false, ArrayType(PrimitiveType.Integer(Vector), Vector(AbstractDimension(Vector))), "b"))),
            Vector(),
            Block(Vector)))
        }
        m18 in {
          parse(m18) shouldBe Class(Declaration.Method(
            Vector(),
            Vector(),
            Vector(),
            PrimitiveType.Integer(Vector),
            Declarator.Method(
              "f",
              Vector(Parameter.Formal(Vector, false, PrimitiveType.Integer(Vector), "a"),
                Parameter.VariableArity(Vector, false, ArrayType(PrimitiveType.Integer(Vector), Vector(AbstractDimension(Vector))), "b"))),
            Vector(),
            Block(Vector)))
        }
        m19 in {
          parse(m19) shouldBe Class(Declaration.Method(
            Vector(),
            Vector(),
            Vector(),
            PrimitiveType.Integer(Vector),
            Declarator.Method(
              "f",
              Vector(Parameter.InstanceReceiver(Vector(MarkerAnnotation("A")), ClassType(Vector, None, "A", Vector)))),
            Vector(),
            Block(Vector)))
        }
        m20 in {
          parse(m20) shouldBe Class(Declaration.Method(
            Vector(),
            Vector(),
            Vector(),
            PrimitiveType.Integer(Vector),
            Declarator.Method(
              "f",
              Vector(Parameter.NestedReceiver(Vector(MarkerAnnotation("A")), ClassType(Vector, None, "A", Vector), "A"))),
            Vector(),
            Block(Vector)))
        }
      }
      "initializers" - {
        "class A{{}}" in { parse("class A{{}}") shouldBe Class(Declaration.InstanceInitializer(Block(Vector))) }
        "class A{static {}}" in { parse("class A{static {}}") shouldBe Class(Declaration.StaticInitializer(Block(Vector))) }
      }
      "constructors" - {
        c1 in {
          parse(c1) shouldBe Class(Declaration.Constructor(
            Vector(),
            Vector(),
            Vector(),
            Declarator.Constructor("A", Vector()),
            Vector(),
            Block(Vector)))
        }
        c2 in {
          parse(c2) shouldBe Class(Declaration.Constructor(
            Vector(),
            Vector(Modifier.Private),
            Vector(),
            Declarator.Constructor("A", Vector()),
            Vector(),
            Block(Vector)))
        }
        c3 in {
          parse(c3) shouldBe Class(Declaration.Constructor(
            Vector(),
            Vector(Modifier.Protected),
            Vector(),
            Declarator.Constructor("A", Vector()),
            Vector(),
            Block(Vector)))
        }
        c4 in {
          parse(c4) shouldBe Class(Declaration.Constructor(
            Vector(),
            Vector(Modifier.Public),
            Vector(),
            Declarator.Constructor("A", Vector()),
            Vector(),
            Block(Vector)))
        }
        c5 in {
          parse(c5) shouldBe Class(Declaration.Constructor(
            Vector(MarkerAnnotation("A")),
            Vector(),
            Vector(),
            Declarator.Constructor("A", Vector()),
            Vector(),
            Block(Vector)))
        }
        c6 in {
          parse(c6) shouldBe Class(Declaration.Constructor(
            Vector(),
            Vector(),
            Vector(),
            Declarator.Constructor("A", Vector(Parameter.Formal(Vector, false, PrimitiveType.Integer(Vector), "a"))),
            Vector(),
            Block(Vector)))
        }
        c7 in {
          parse(c7) shouldBe Class(Declaration.Constructor(
            Vector(),
            Vector(),
            Vector(),
            Declarator.Constructor("A", Vector(
              Parameter.Formal(Vector, false, PrimitiveType.Integer(Vector), "a"),
              Parameter.VariableArity(Vector, false, ArrayType(PrimitiveType.Integer(Vector), AbstractDimension(Vector)), "b"))),
            Vector(),
            Block(Vector)))
        }
        c8 in {
          parse(c8) shouldBe Class(Declaration.Constructor(
            Vector(),
            Vector(),
            Vector(),
            Declarator.Constructor("A", Vector(Parameter.InstanceReceiver(Vector, ClassType(Vector, None, "A", Vector)))),
            Vector(),
            Block(Vector)))
        }
        c9 in {
          parse(c9) shouldBe Class(Declaration.Constructor(
            Vector(),
            Vector(),
            Vector(),
            Declarator.Constructor("A", Vector(Parameter.NestedReceiver(Vector, ClassType(Vector, None, "A", Vector), "A"))),
            Vector(),
            Block(Vector)))
        }
        c10 in {
          parse(c10) shouldBe Class(Declaration.Constructor(
            Vector(),
            Vector(),
            Vector(Template.Parameter(Vector, "T")),
            Declarator.Constructor("A", Vector()),
            Vector(),
            Block(Vector)))
        }
        c11 in {
          parse(c11) shouldBe Class(Declaration.Constructor(
            Vector(),
            Vector(),
            Vector(),
            Declarator.Constructor("A", Vector()),
            Vector(ClassType(Vector, None, "A", Vector)),
            Block(Vector)))
        }
        c12 in {
          parse(c12) shouldBe Class(Declaration.Constructor(
            Vector(),
            Vector(),
            Vector(),
            Declarator.Constructor("A", Vector()),
            Vector(),
            Block(Vector(
              ConstructorInvocation.Alternate(Vector, Vector)))))
        }
        c13 in {
          parse(c13) shouldBe Class(Declaration.Constructor(
            Vector(),
            Vector(),
            Vector(),
            Declarator.Constructor("A", Vector()),
            Vector(),
            Block(Vector(
              ConstructorInvocation.Alternate(Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), Vector)))))
        }
        c14 in {
          parse(c14) shouldBe Class(Declaration.Constructor(
            Vector(),
            Vector(),
            Vector(),
            Declarator.Constructor("A", Vector()),
            Vector(),
            Block(Vector(
              ConstructorInvocation.Parent(Vector, Vector)))))
        }
        c15 in {
          parse(c15) shouldBe Class(Declaration.Constructor(
            Vector(),
            Vector(),
            Vector(),
            Declarator.Constructor("A", Vector()),
            Vector(),
            Block(Vector(
              ConstructorInvocation.Parent(Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), Vector)))))
        }
        c16 in {
          parse(c16) shouldBe Class(Declaration.Constructor(
            Vector(),
            Vector(),
            Vector(),
            Declarator.Constructor("A", Vector()),
            Vector(),
            Block(Vector(
              ConstructorInvocation.IndirectParent(Select("A"), Vector, Vector)))))
        }
        c17 in {
          parse(c17) shouldBe Class(Declaration.Constructor(
            Vector(),
            Vector(),
            Vector(),
            Declarator.Constructor("A", Vector()),
            Vector(),
            Block(Vector(
              ConstructorInvocation.IndirectParent(Select("A"), Template.Argument(ClassType(Vector, None, "T", Vector)), Vector)))))
        }
        c18 in {
          parse(c18) shouldBe Class(Declaration.Constructor(
            Vector(),
            Vector(),
            Vector(),
            Declarator.Constructor("A", Vector()),
            Vector(),
            Block(Vector(
              ConstructorInvocation.Intermidiate(MethodInvocation(Vector, "a", Vector), Vector, Vector)))))
        }
        c19 in {
          parse(c19) shouldBe Class(Declaration.Constructor(
            Vector(),
            Vector(),
            Vector(),
            Declarator.Constructor("A", Vector()),
            Vector(),
            Block(Vector(
              ConstructorInvocation.Intermidiate(MethodInvocation(Vector, "a", Vector), Template.Argument(ClassType(Vector, None, "T", Vector)), Vector)))))
        }
        c20 in {
          parse(c20) shouldBe Class(Declaration.Constructor(
            Vector(),
            Vector(),
            Vector(),
            Declarator.Constructor("A", Vector()),
            Vector(),
            Block(Vector(
              ConstructorInvocation.Intermidiate(Instantiation.Object(Vector, ClassType(Vector, None, "A", Vector), Vector), Vector, Vector)))))
        }
        c21 in {
          parse(c21) shouldBe Class(
            Declaration.Constructor(
              Vector(),
              Vector(),
              Vector(),
              Declarator.Constructor(
                "A",
                Vector()),
              Vector(),
              Block(Vector)),
            Declaration.Constructor(
              Vector(),
              Vector(),
              Vector(),
              Declarator.Constructor(
                "A",
                Vector(Parameter.Formal(Vector, false, PrimitiveType.Integer(Vector), "a"))),
              Vector(),
              Block(Vector)))
        }
        c22 in {
          parse(c22) shouldBe Class(Declaration.Field(
            Vector,
            Vector(Modifier.Private, Modifier.Static, Modifier.Final),
            PrimitiveType.Byte(Vector),
            Declarator.InitializedVariable(
              "PAD", PrimitiveLiteral("'='"))))
        }
      }
      "class declarations" - {
        "class A{class B{}}" in {
          parse("class A{class B{}}") shouldBe Class(Declaration.Class(
            Vector, Vector, "B", Vector, None, Vector, Vector))
        }
      }
      "interface declarations" - {
        //TODO
      }
    }

    "enums" - {
      "@A enum A{}" in {
        parse("@A enum A{}") shouldBe Declaration.Enum(
          Vector(MarkerAnnotation("A")),
          Vector,
          "A",
          Vector,
          Vector)
      }
      "public enum A{}" in {
        parse("public enum A{}") shouldBe Declaration.Enum(
          Vector,
          Vector(Modifier.Public),
          "A",
          Vector,
          Vector)
      }
      "protected enum A{}" in {
        parse("protected enum A{}") shouldBe Declaration.Enum(
          Vector,
          Vector(Modifier.Protected),
          "A",
          Vector,
          Vector)
      }
      "private enum A{}" in {
        parse("private enum A{}") shouldBe Declaration.Enum(
          Vector,
          Vector(Modifier.Private),
          "A",
          Vector,
          Vector)
      }
      "abstract enum A{}" in {
        parse("abstract enum A{}") shouldBe Declaration.Enum(
          Vector,
          Vector(Modifier.Abstract),
          "A",
          Vector,
          Vector)
      }
      "static enum A{}" in {
        parse("static enum A{}") shouldBe Declaration.Enum(
          Vector,
          Vector(Modifier.Static),
          "A",
          Vector,
          Vector)
      }
      "final enum A{}" in {
        parse("final enum A{}") shouldBe Declaration.Enum(
          Vector,
          Vector(Modifier.Final),
          "A",
          Vector,
          Vector)
      }
      "strictfp enum A{}" in {
        parse("strictfp enum A{}") shouldBe Declaration.Enum(
          Vector,
          Vector(Modifier.Strictfp),
          "A",
          Vector,
          Vector)
      }
      "enum A implements A{}" in {
        parse("enum A implements A{}") shouldBe Declaration.Enum(
          Vector,
          Vector,
          "A",
          Vector(ClassType(Vector, None, "A", Vector)),
          Vector)
      }
      "enum A{A,B,C}" in {
        parse("enum A{A,B,C}") shouldBe Declaration.Enum(
          Vector,
          Vector,
          "A",
          Vector,
          Vector(Declaration.EnumConstant(Vector, "A", Vector), Declaration.EnumConstant(Vector, "B", Vector), Declaration.EnumConstant(Vector, "C", Vector)))
      }
      "enum A{A(1)}" in {
        parse("enum A{A(1)}") shouldBe Declaration.Enum(
          Vector,
          Vector,
          "A",
          Vector,
          Vector(Declaration.EnumConstant(Vector, "A", Vector(PrimitiveLiteral("1")))))
      }
      "enum A{A{}}" in {
        parse("enum A{A{}}") shouldBe Declaration.Enum(
          Vector,
          Vector,
          "A",
          Vector,
          Vector(Declaration.AnonymousEnumConstant(Vector, "A", Vector, Vector)))
      }
    }
  }
}