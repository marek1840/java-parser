package com.enkidu.lignum.parsers.java.expressions

import com.enkidu.lignum.parsers.types._
import com.enkidu.lignum.parsers.expressions._
import com.enkidu.lignum.parsers.statements._
import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.JavaParser

class PrimaryTest extends ParserTest {
  def parse(string: String): Expression = {
    implicit val parser = new JavaParser(string)
    get(parser.expression.run())
  }

  "Primary parser should parse" - {
    "this" in (parse("this") shouldBe ThisReference)
    "(this)" in (parse("(this)") shouldBe ThisReference)
    "((a))" in { parse("((a))") shouldBe Select("a") }
    "longish.qualifier.this" in (parse("longish.qualifier.this") shouldBe QualifiedThisReference(Vector("longish", "qualifier")))
    "literals" - {
      "1" in (parse("1") shouldBe PrimitiveLiteral("1"))
      "test.class" in (parse("test.class") shouldBe ClassLiteral(ClassType(Vector, None, "test", Vector)))
      "\"aa\"" in (parse("\"aa\"") shouldBe PrimitiveLiteral("\"aa\""))
    }
    "object creation" - {
      "new Class()" in { parse("new Class()") shouldBe Instantiation.Object(Vector, ClassType(Vector(), None, "Class", Vector), Vector()) }
      "new <T> Class()" in { parse("new <T> Class()") shouldBe Instantiation.Object(Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), ClassType(Vector(), None, "Class", Vector), Vector()) }
      "new <T> @A Class()" in { parse("new <T> @A Class()") shouldBe Instantiation.Object(Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), ClassType(Vector(MarkerAnnotation("A")), None, "Class", Vector), Vector()) }
      "new <T> @A Class<U,V>()" in { parse("new <T> @A Class<U,V>()") shouldBe Instantiation.Object(Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), ClassType(Vector(MarkerAnnotation("A")), None, "Class", Vector(Template.Argument(ClassType(Vector, None, "U", Vector)), Template.Argument(ClassType(Vector, None, "V", Vector)))), Vector()) }
      "new <T> @A Class<U,V>(arg1, arg2)" in { parse("new <T> @A Class<U,V>(arg1, arg2)") shouldBe Instantiation.Object(Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), ClassType(Vector(MarkerAnnotation("A")), None, "Class", Vector(Template.Argument(ClassType(Vector, None, "U", Vector)), Template.Argument(ClassType(Vector, None, "V", Vector)))), Vector(Select("arg1"), Select("arg2"))) }
      "new <T> @A Class<>(arg1, arg2)" in { parse("new <T> @A Class<>(arg1, arg2)") shouldBe Instantiation.Object(Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), ClassType(Vector(MarkerAnnotation("A")), None, "Class", Vector()), Vector(Select("arg1"), Select("arg2"))) }
      "expr.name. new Class<T>()" in { parse("expr.name. new Class<T>()") shouldBe Instantiation.NestedObject(Select(Vector("expr", "name")), Vector, ClassType(Vector(), None, "Class", Vector(Template.Argument(ClassType(Vector, None, "T", Vector)))), Vector) }
      "expr.name.new <T> @A Class<U,V>(arg1, arg2)" in { parse("expr.name.new <T> @A Class<U,V>(arg1, arg2)") shouldBe Instantiation.NestedObject(Select(Vector("expr", "name")), Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), ClassType(Vector(MarkerAnnotation("A")), None, "Class", Vector(Template.Argument(ClassType(Vector, None, "U", Vector)), Template.Argument(ClassType(Vector, None, "V", Vector)))), Vector(Select("arg1"), Select("arg2"))) }
      "invoke.new <T> @A Class<U,V>(arg1, arg2)" in { parse("invoke.new <T> @A Class<U,V>(arg1, arg2)") shouldBe Instantiation.NestedObject(Select(Vector("invoke")), Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), ClassType(Vector(MarkerAnnotation("A")), None, "Class", Vector(Template.Argument(ClassType(Vector, None, "U", Vector)), Template.Argument(ClassType(Vector, None, "V", Vector)))), Vector(Select("arg1"), Select("arg2"))) }
    }
    "anonymous object creation" - {
      "new Class(){;}" in { parse("new Class(){;}") shouldBe Instantiation.AnonymousObject(Vector, ClassType(Vector(), None, "Class", Vector), Vector(), Vector(Declaration.Empty)) }
      "new <T>Class(){;}" in { parse("new <T>Class(){;}") shouldBe Instantiation.AnonymousObject(Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), ClassType(Vector(), None, "Class", Vector), Vector(), Vector(Declaration.Empty)) }
      "new <T> @A Class(){;}" in { parse("new <T> @A Class(){;}") shouldBe Instantiation.AnonymousObject(Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), ClassType(Vector(MarkerAnnotation("A")), None, "Class", Vector), Vector(), Vector(Declaration.Empty)) }
      "new <T> @A Class<U,V>(){;}" in { parse("new <T> @A Class<U,V>(){;}") shouldBe Instantiation.AnonymousObject(Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), ClassType(Vector(MarkerAnnotation("A")), None, "Class", Vector(Template.Argument(ClassType(Vector, None, "U", Vector)), Template.Argument(ClassType(Vector, None, "V", Vector)))), Vector(), Vector(Declaration.Empty)) }
      "new <T> @A Class<U,V>(arg1, arg2){;}" in { parse("new <T> @A Class<U,V>(arg1, arg2){;}") shouldBe Instantiation.AnonymousObject(Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), ClassType(Vector(MarkerAnnotation("A")), None, "Class", Vector(Template.Argument(ClassType(Vector, None, "U", Vector)), Template.Argument(ClassType(Vector, None, "V", Vector)))), Vector(Select("arg1"), Select("arg2")), Vector(Declaration.Empty)) }
      "new <T> @A Class<>(arg1, arg2){;}" in { parse("new <T> @A Class<>(arg1, arg2){;}") shouldBe Instantiation.AnonymousObject(Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), ClassType(Vector(MarkerAnnotation("A")), None, "Class", Vector()), Vector(Select("arg1"), Select("arg2")), Vector(Declaration.Empty)) }
      "expr.name. new Class<T>(){;}" in { parse("expr.name. new Class<T>(){;}") shouldBe Instantiation.NestedAnonymousObject(Select(Vector("expr", "name")), Vector, ClassType(Vector(), None, "Class", Vector(Template.Argument(ClassType(Vector, None, "T", Vector)))), Vector, Vector(Declaration.Empty)) }
      "expr.name.new <T> @A Class<U,V>(arg1, arg2){;}" in { parse("expr.name.new <T> @A Class<U,V>(arg1, arg2){;}") shouldBe Instantiation.NestedAnonymousObject(Select(Vector("expr", "name")), Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), ClassType(Vector(MarkerAnnotation("A")), None, "Class", Vector(Template.Argument(ClassType(Vector, None, "U", Vector)), Template.Argument(ClassType(Vector, None, "V", Vector)))), Vector(Select("arg1"), Select("arg2")), Vector(Declaration.Empty)) }
      "invoke.new <T> @A Class<U,V>(arg1, arg2){;}" in { parse("invoke.new <T> @A Class<U,V>(arg1, arg2){;}") shouldBe Instantiation.NestedAnonymousObject(Select(Vector("invoke")), Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), ClassType(Vector(MarkerAnnotation("A")), None, "Class", Vector(Template.Argument(ClassType(Vector, None, "U", Vector)), Template.Argument(ClassType(Vector, None, "V", Vector)))), Vector(Select("arg1"), Select("arg2")), Vector(Declaration.Empty)) }
    }
    "field access" - {
      "super.field" in { parse("super.field") shouldBe FieldAccess(ParentReference, "field") }
      "Class.Name.super.field" in { parse("Class.Name.super.field") shouldBe FieldAccess(QualifiedParentReference(Vector("Class", "Name")), "field") }
      "f().field" in { parse("f().field") shouldBe FieldAccess(MethodInvocation(Vector, "f", Vector), "field") }
      "f[0].field" in { parse("f[0].field") shouldBe FieldAccess(Extraction(Select("f"), PrimitiveLiteral("0")), "field") }
      "f.f1.f2" in { parse("f.f1.f2") shouldBe Select(Vector("f", "f1", "f2")) }
    }
    "array access" - {
      "f().field[1]" in { parse("f().field[1]") shouldBe Extraction(FieldAccess(MethodInvocation(Vector, "f", Vector), "field"), PrimitiveLiteral("1")) }
      "f.field[1]" in { parse("f.field[1]") shouldBe Extraction(Select(Vector("f", "field")), PrimitiveLiteral("1")) }
      "f.f1.f2[1]" in { parse("f.f1.f2[1]") shouldBe Extraction(Select(Vector("f", "f1", "f2")), PrimitiveLiteral("1")) }
    }
    "array creation" - {
      "new int [1]" in { parse("new int [1]") shouldBe Instantiation.Array(PrimitiveType.Integer(Vector), Vector(InitializedDimension(Vector, PrimitiveLiteral("1")))) }
      "new int [1][1]" in { parse("new int [1][1]") shouldBe Instantiation.Array(PrimitiveType.Integer(Vector), Vector(InitializedDimension(Vector, PrimitiveLiteral("1")), InitializedDimension(Vector, PrimitiveLiteral("1")))) }
      "new int @A [1]@A[1][][]@A[]" in { parse("new int @A [1]@A[1][][]@A[]") shouldBe Instantiation.Array(PrimitiveType.Integer(Vector), Vector(InitializedDimension(Vector(MarkerAnnotation("A")), PrimitiveLiteral("1")), InitializedDimension(Vector(MarkerAnnotation("A")), PrimitiveLiteral("1")), AbstractDimension(Vector), AbstractDimension(Vector), AbstractDimension(Vector(MarkerAnnotation("A"))))) }
      "new Class @A [1]@A[1][][]@A[]" in { parse("new Class @A [1]@A[1][][]@A[]") shouldBe Instantiation.Array(ClassType(Vector, None, "Class", Vector), Vector(InitializedDimension(Vector(MarkerAnnotation("A")), PrimitiveLiteral("1")), InitializedDimension(Vector(MarkerAnnotation("A")), PrimitiveLiteral("1")), AbstractDimension(Vector), AbstractDimension(Vector), AbstractDimension(Vector(MarkerAnnotation("A"))))) }
      "new Class<T> @A [1]@A[1][][]@A[]" in { parse("new Class<T> @A [1]@A[1][][]@A[]") shouldBe Instantiation.Array(ClassType(Vector, None, "Class", Vector(Template.Argument(ClassType(Vector, None, "T", Vector)))), Vector(InitializedDimension(Vector(MarkerAnnotation("A")), PrimitiveLiteral("1")), InitializedDimension(Vector(MarkerAnnotation("A")), PrimitiveLiteral("1")), AbstractDimension(Vector), AbstractDimension(Vector), AbstractDimension(Vector(MarkerAnnotation("A"))))) }
      "new Class.@A Class @A [1]@A[1][][]@A[]" in { parse("new Class.@A Class @A [1]@A[1][][]@A[]") shouldBe Instantiation.Array(ClassType(Vector(MarkerAnnotation("A")), ClassType(Vector(), None, "Class", Vector()), "Class", Vector), Vector(InitializedDimension(Vector(MarkerAnnotation("A")), PrimitiveLiteral("1")), InitializedDimension(Vector(MarkerAnnotation("A")), PrimitiveLiteral("1")), AbstractDimension(Vector), AbstractDimension(Vector), AbstractDimension(Vector(MarkerAnnotation("A"))))) }

      "new int [][]{{1,2,3}, {1,2,3}}" in { parse("new int [][]{{1,2,3}, {1,2,3}}") shouldBe Instantiation.InitializedArray(ArrayType(PrimitiveType.Integer(Vector()), Vector(AbstractDimension(Vector), AbstractDimension(Vector))), ArrayInitializer(Vector(ArrayInitializer(Vector(PrimitiveLiteral("1"), PrimitiveLiteral("2"), PrimitiveLiteral("3"))), ArrayInitializer(Vector(PrimitiveLiteral("1"), PrimitiveLiteral("2"), PrimitiveLiteral("3")))))) }
      "new Class[]@A[] {{new Class(), new Class()}}" in { parse("new Class[]@A[] {new Class(), new Class()}") shouldBe Instantiation.InitializedArray(ArrayType(ClassType(Vector(), None, "Class", Vector()), Vector(AbstractDimension(Vector), AbstractDimension(Vector(MarkerAnnotation("A"))))), ArrayInitializer(Vector(Instantiation.Object(Vector, ClassType(Vector(), None, "Class", Vector), Vector()), Instantiation.Object(Vector, ClassType(Vector(), None, "Class", Vector), Vector())))) }
    }
    "method invocation" - {
      "f()" in { parse("f()") shouldBe MethodInvocation(Vector, "f", Vector) }
      "f(a)" in { parse("f(a)") shouldBe MethodInvocation(Vector, "f", Vector(Select("a"))) }
      "f(a1, a2)" in { parse("f(a1, a2)") shouldBe MethodInvocation(Vector, "f", Vector(Select(Vector("a1")), Select("a2"))) }
      "Class.Name.f(a,a)" in { parse("Class.Name.f(a,a)") shouldBe QualifiedMethodInvocation(Select(Vector("Class", "Name")), Vector, "f", Vector(Select(Vector("a")), Select(Vector("a")))) }
      "Class.Name.<T> f(a,a)" in { parse("Class.Name.<T> f(a,a)") shouldBe QualifiedMethodInvocation(Select(Vector("Class", "Name")), Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), "f", Vector(Select(Vector("a")), Select(Vector("a")))) }
      "Class.Name . <T> f(a,a)" in { parse("Class.Name . <T> f(a,a)") shouldBe QualifiedMethodInvocation(Select(Vector("Class", "Name")), Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), "f", Vector(Select(Vector("a")), Select(Vector("a")))) }
      "expr.name.f(a,a)" in { parse("expr.name.f(a,a)") shouldBe QualifiedMethodInvocation(Select(Vector("expr", "name")), Vector, "f", Vector(Select(Vector("a")), Select(Vector("a")))) }
      "expr.name.<T> f(a,a)" in { parse("expr.name.<T> f(a,a)") shouldBe QualifiedMethodInvocation(Select(Vector("expr", "name")), Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), "f", Vector(Select(Vector("a")), Select(Vector("a")))) }
      "a.f()" in { parse("a.f()") shouldBe QualifiedMethodInvocation(Select("a"), Vector, "f", Vector) }
      "a[0].f()" in { parse("a[0].f()") shouldBe QualifiedMethodInvocation(Extraction(Select("a"), PrimitiveLiteral("0")), Vector, "f", Vector) }
      "a().f()" in { parse("a().f()") shouldBe QualifiedMethodInvocation(MethodInvocation(Vector, "a", Vector), Vector, "f", Vector) }
      "a[0].<T> f()" in { parse("a[0].<T> f()") shouldBe QualifiedMethodInvocation(Extraction(Select("a"), PrimitiveLiteral("0")), Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), "f", Vector) }
      "a.<T> f()" in { parse("a.<T> f()") shouldBe QualifiedMethodInvocation(Select("a"), Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), "f", Vector) }
      "a().<T> f()" in { parse("a().<T> f()") shouldBe QualifiedMethodInvocation(MethodInvocation(Vector, "a", Vector), Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), "f", Vector) }
      "super.f()" in { parse("super.f()") shouldBe QualifiedMethodInvocation(ParentReference, Vector, "f", Vector) }
      "Class.super.f()" in { parse("Class.super.f()") shouldBe QualifiedMethodInvocation(QualifiedParentReference("Class"), Vector, "f", Vector) }
      "super.<T> f()" in { parse("super.<T> f()") shouldBe QualifiedMethodInvocation(ParentReference, Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), "f", Vector) }
      "Class.super.<T> f()" in { parse("Class.super.<T> f()") shouldBe QualifiedMethodInvocation(QualifiedParentReference("Class"), Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), "f", Vector) }
      "f((a).f())" in { parse("f((a).f())") shouldBe MethodInvocation(Vector, "f", QualifiedMethodInvocation(Select("a"), Vector, "f", Vector)) }
    }
    "method reference" - {
      "expr.name :: f" in { parse("expr.name :: f") shouldBe MethodReference(Select(Vector("expr", "name")), Vector, "f") }
      "Class:: f" in { parse("Class:: f") shouldBe MethodReference(Select("Class"), Vector, "f") }
      "f[1] ::f" in { parse("f[1] ::f") shouldBe MethodReference(Extraction(Select("f"), PrimitiveLiteral("1")), Vector, "f") }
      "super::f" in { parse("super::f") shouldBe MethodReference(ParentReference, Vector, "f") }
      "Class.super ::f" in { parse("Class.super:: f") shouldBe MethodReference(QualifiedParentReference("Class"), Vector, "f") }

      "expr.name ::<T> f" in { parse("expr.name ::<T> f") shouldBe MethodReference(Select(Vector("expr", "name")), Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), "f") }
      "Class:: <T>f" in { parse("Class:: <T>f") shouldBe MethodReference(Select("Class"), Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), "f") }
      "f[1] ::<T>f" in { parse("f[1] ::<T>f") shouldBe MethodReference(Extraction(Select("f"), PrimitiveLiteral("1")), Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), "f") }
      "super::<T>f" in { parse("super::<T>f") shouldBe MethodReference(ParentReference, Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), "f") }
      "Class.super::<T>f" in { parse("Class.super::<T>f") shouldBe MethodReference(QualifiedParentReference("Class"), Vector(Template.Argument(ClassType(Vector, None, "T", Vector))), "f") }
      "Class::new" in { parse("Class::new") shouldBe ConstructorReference(ClassType(Vector, None, "Class", Vector), Vector) }
      "Class::<T>new" in { parse("Class::<T>new") shouldBe ConstructorReference(ClassType(Vector, None, "Class", Vector), Vector(Template.Argument(ClassType(Vector, None, "T", Vector)))) }
      "int@A[] :: new" in { parse("int@A[] :: new") shouldBe ArrayConstructorReference(ArrayType(PrimitiveType.Integer(Vector), Vector(AbstractDimension(Vector(MarkerAnnotation("A")))))) }
    }
  }
}