package com.enkidu.lignum.parsers.java.expressions

import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.ast.expression._
import com.enkidu.lignum.parsers.ast.expression.discardable._
import com.enkidu.lignum.parsers.ast.expression.discardable.binary._
import com.enkidu.lignum.parsers.ast.expression.discardable.dimension.{AbstractDimension, InitializedDimension}
import com.enkidu.lignum.parsers.ast.expression.discardable.instantiation._
import com.enkidu.lignum.parsers.ast.expression.discardable.literals.{ClassLiteral, IntegerLiteral, StringLiteral}
import com.enkidu.lignum.parsers.ast.expression.types.annotations.MarkerAnnotation
import com.enkidu.lignum.parsers.ast.expression.types.primitives.IntegerPrimitive
import com.enkidu.lignum.parsers.ast.expression.types.references.{ArrayType, ClassType}
import com.enkidu.lignum.parsers.ast.expression.types.templates.ArgumentTemplate
import com.enkidu.lignum.parsers.ast.statement.declaration.types.EmptyDeclaration
import com.enkidu.lignum.parsers.java.v8.JavaCompilationUnitParser

class PrimaryTest extends ParserTest {
  def parse(string: String): Expression = {
    implicit val parser = new JavaCompilationUnitParser(string)
    get(parser.expression.run())
  }

  "Primary parser should parse" - {
    "this" in (parse("this") shouldBe ThisReference)
    "(this)" in (parse("(this)") shouldBe ThisReference)
    "((a))" in { parse("((a))") shouldBe Select("a") }
    "longish.qualifier.this" in (parse("longish.qualifier.this") shouldBe QualifiedThisReference(Vector("longish", "qualifier")))
    "literals" - {
      "1" in (parse("1") shouldBe IntegerLiteral("1"))
      "test.class" in (parse("test.class") shouldBe ClassLiteral(ClassType(Vector, None, "test", Vector)))
      "\"aa\"" in (parse("\"aa\"") shouldBe StringLiteral("\"aa\""))
    }
    "object creation" - {
      "new Class()" in { parse("new Class()") shouldBe SimpleObjectInstantiation(Vector, ClassType(Vector(), None,
        "Class",
        Vector),
        Vector()) }
      "new <T> Class()" in { parse("new <T> Class()") shouldBe SimpleObjectInstantiation(Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), ClassType(Vector(), None, "Class", Vector), Vector()) }
      "new <T> @A Class()" in { parse("new <T> @A Class()") shouldBe SimpleObjectInstantiation(Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), ClassType(Vector(MarkerAnnotation("A")), None, "Class", Vector), Vector()) }
      "new <T> @A Class<U,V>()" in { parse("new <T> @A Class<U,V>()") shouldBe SimpleObjectInstantiation(Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), ClassType(Vector(MarkerAnnotation("A")), None, "Class", Vector(ArgumentTemplate(ClassType(Vector, None, "U", Vector)), ArgumentTemplate(ClassType(Vector, None, "V", Vector)))), Vector()) }
      "new <T> @A Class<U,V>(arg1, arg2)" in { parse("new <T> @A Class<U,V>(arg1, arg2)") shouldBe SimpleObjectInstantiation(Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), ClassType(Vector(MarkerAnnotation("A")), None, "Class", Vector(ArgumentTemplate(ClassType(Vector, None, "U", Vector)), ArgumentTemplate(ClassType(Vector, None, "V", Vector)))), Vector(Select("arg1"), Select("arg2"))) }
      "new <T> @A Class<>(arg1, arg2)" in { parse("new <T> @A Class<>(arg1, arg2)") shouldBe SimpleObjectInstantiation(Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), ClassType(Vector(MarkerAnnotation("A")), None, "Class", Vector()), Vector(Select("arg1"), Select("arg2"))) }
      "expr.name. new Class<T>()" in { parse("expr.name. new Class<T>()") shouldBe NestedObjectInstantiation(Select(Vector("expr", "name")), Vector, ClassType(Vector(), None, "Class", Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector)))), Vector) }
      "expr.name.new <T> @A Class<U,V>(arg1, arg2)" in { parse("expr.name.new <T> @A Class<U,V>(arg1, arg2)") shouldBe NestedObjectInstantiation(Select(Vector("expr", "name")), Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), ClassType(Vector(MarkerAnnotation("A")), None, "Class", Vector(ArgumentTemplate(ClassType(Vector, None, "U", Vector)), ArgumentTemplate(ClassType(Vector, None, "V", Vector)))), Vector(Select("arg1"), Select("arg2"))) }
      "invoke.new <T> @A Class<U,V>(arg1, arg2)" in { parse("invoke.new <T> @A Class<U,V>(arg1, arg2)") shouldBe NestedObjectInstantiation(Select(Vector("invoke")), Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), ClassType(Vector(MarkerAnnotation("A")), None, "Class", Vector(ArgumentTemplate(ClassType(Vector, None, "U", Vector)), ArgumentTemplate(ClassType(Vector, None, "V", Vector)))), Vector(Select("arg1"), Select("arg2"))) }
    }
    "anonymous object creation" - {
      "new Class(){;}" in { parse("new Class(){;}") shouldBe AnonymousObjectInstantiation(Vector, ClassType(Vector(), None, "Class", Vector), Vector(), Vector(EmptyDeclaration)) }
      "new <T>Class(){;}" in { parse("new <T>Class(){;}") shouldBe AnonymousObjectInstantiation(Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), ClassType(Vector(), None, "Class", Vector), Vector(), Vector(EmptyDeclaration)) }
      "new <T> @A Class(){;}" in { parse("new <T> @A Class(){;}") shouldBe AnonymousObjectInstantiation(Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), ClassType(Vector(MarkerAnnotation("A")), None, "Class", Vector), Vector(), Vector(EmptyDeclaration)) }
      "new <T> @A Class<U,V>(){;}" in { parse("new <T> @A Class<U,V>(){;}") shouldBe AnonymousObjectInstantiation(Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), ClassType(Vector(MarkerAnnotation("A")), None, "Class", Vector(ArgumentTemplate(ClassType(Vector, None, "U", Vector)), ArgumentTemplate(ClassType(Vector, None, "V", Vector)))), Vector(), Vector(EmptyDeclaration)) }
      "new <T> @A Class<U,V>(arg1, arg2){;}" in { parse("new <T> @A Class<U,V>(arg1, arg2){;}") shouldBe AnonymousObjectInstantiation(Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), ClassType(Vector(MarkerAnnotation("A")), None, "Class", Vector(ArgumentTemplate(ClassType(Vector, None, "U", Vector)), ArgumentTemplate(ClassType(Vector, None, "V", Vector)))), Vector(Select("arg1"), Select("arg2")), Vector(EmptyDeclaration)) }
      "new <T> @A Class<>(arg1, arg2){;}" in { parse("new <T> @A Class<>(arg1, arg2){;}") shouldBe AnonymousObjectInstantiation(Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), ClassType(Vector(MarkerAnnotation("A")), None, "Class", Vector()), Vector(Select("arg1"), Select("arg2")), Vector(EmptyDeclaration)) }
      "expr.name. new Class<T>(){;}" in { parse("expr.name. new Class<T>(){;}") shouldBe NestedAnonymousObjectInstantiation(Select(Vector("expr", "name")), Vector, ClassType(Vector(), None, "Class", Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector)))), Vector, Vector(EmptyDeclaration)) }
      "expr.name.new <T> @A Class<U,V>(arg1, arg2){;}" in { parse("expr.name.new <T> @A Class<U,V>(arg1, arg2){;}") shouldBe NestedAnonymousObjectInstantiation(Select(Vector("expr", "name")), Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), ClassType(Vector(MarkerAnnotation("A")), None, "Class", Vector(ArgumentTemplate(ClassType(Vector, None, "U", Vector)), ArgumentTemplate(ClassType(Vector, None, "V", Vector)))), Vector(Select("arg1"), Select("arg2")), Vector(EmptyDeclaration)) }
      "invoke.new <T> @A Class<U,V>(arg1, arg2){;}" in { parse("invoke.new <T> @A Class<U,V>(arg1, arg2){;}") shouldBe NestedAnonymousObjectInstantiation(Select(Vector("invoke")), Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), ClassType(Vector(MarkerAnnotation("A")), None, "Class", Vector(ArgumentTemplate(ClassType(Vector, None, "U", Vector)), ArgumentTemplate(ClassType(Vector, None, "V", Vector)))), Vector(Select("arg1"), Select("arg2")), Vector(EmptyDeclaration)) }
    }
    "field access" - {
      "super.field" in { parse("super.field") shouldBe FieldAccess(ParentReference, "field") }
      "Class.Name.super.field" in { parse("Class.Name.super.field") shouldBe FieldAccess(QualifiedParentReference(Vector("Class", "Name")), "field") }
      "f().field" in { parse("f().field") shouldBe FieldAccess(MethodInvocation(Vector, "f", Vector), "field") }
      "f[0].field" in { parse("f[0].field") shouldBe FieldAccess(Extraction(Select("f"), IntegerLiteral("0")), 
        "field") }
      "f.f1.f2" in { parse("f.f1.f2") shouldBe Select(Vector("f", "f1", "f2")) }
    }
    "array access" - {
      "f().field[1]" in { parse("f().field[1]") shouldBe Extraction(FieldAccess(MethodInvocation(Vector, "f", Vector), "field"), IntegerLiteral("1")) }
      "f.field[1]" in { parse("f.field[1]") shouldBe Extraction(Select(Vector("f", "field")), IntegerLiteral("1")) }
      "f.f1.f2[1]" in { parse("f.f1.f2[1]") shouldBe Extraction(Select(Vector("f", "f1", "f2")), IntegerLiteral("1")) }
    }
    "array creation" - {
      "new int [1]" in { parse("new int [1]") shouldBe EmptyArrayInstantiation(IntegerPrimitive(Vector), Vector(InitializedDimension(Vector, IntegerLiteral("1")))) }
      "new int [1][1]" in { parse("new int [1][1]") shouldBe EmptyArrayInstantiation(IntegerPrimitive(Vector), Vector(InitializedDimension(Vector, IntegerLiteral("1")), InitializedDimension(Vector, IntegerLiteral("1")))) }
      "new int @A [1]@A[1][][]@A[]" in { parse("new int @A [1]@A[1][][]@A[]") shouldBe EmptyArrayInstantiation(IntegerPrimitive(Vector), Vector(InitializedDimension(Vector(MarkerAnnotation("A")), IntegerLiteral("1")), InitializedDimension(Vector(MarkerAnnotation("A")), IntegerLiteral("1")), AbstractDimension(Vector), AbstractDimension(Vector), AbstractDimension(Vector(MarkerAnnotation("A"))))) }
      "new Class @A [1]@A[1][][]@A[]" in { parse("new Class @A [1]@A[1][][]@A[]") shouldBe EmptyArrayInstantiation(ClassType(Vector, None, "Class", Vector), Vector(InitializedDimension(Vector(MarkerAnnotation("A")), IntegerLiteral("1")), InitializedDimension(Vector(MarkerAnnotation("A")), IntegerLiteral("1")), AbstractDimension(Vector), AbstractDimension(Vector), AbstractDimension(Vector(MarkerAnnotation("A"))))) }
      "new Class<T> @A [1]@A[1][][]@A[]" in { parse("new Class<T> @A [1]@A[1][][]@A[]") shouldBe EmptyArrayInstantiation(ClassType(Vector, None, "Class", Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector)))), Vector(InitializedDimension(Vector(MarkerAnnotation("A")), IntegerLiteral("1")), InitializedDimension(Vector(MarkerAnnotation("A")), IntegerLiteral("1")), AbstractDimension(Vector), AbstractDimension(Vector), AbstractDimension(Vector(MarkerAnnotation("A"))))) }
      "new Class.@A Class @A [1]@A[1][][]@A[]" in { parse("new Class.@A Class @A [1]@A[1][][]@A[]") shouldBe EmptyArrayInstantiation(ClassType(Vector(MarkerAnnotation("A")), ClassType(Vector(), None, "Class", Vector()), "Class", Vector), Vector(InitializedDimension(Vector(MarkerAnnotation("A")), IntegerLiteral("1")), InitializedDimension(Vector(MarkerAnnotation("A")), IntegerLiteral("1")), AbstractDimension(Vector), AbstractDimension(Vector), AbstractDimension(Vector(MarkerAnnotation("A"))))) }

      "new int [][]{{1,2,3}, {1,2,3}}" in { parse("new int [][]{{1,2,3}, {1,2,3}}") shouldBe InitializedArrayInstantiation(ArrayType(IntegerPrimitive(Vector()), Vector(AbstractDimension(Vector), AbstractDimension(Vector))), ArrayInitializer(Vector(ArrayInitializer(Vector(IntegerLiteral("1"), IntegerLiteral("2"), IntegerLiteral("3"))), ArrayInitializer(Vector(IntegerLiteral("1"), IntegerLiteral("2"), IntegerLiteral("3")))))) }
      "new Class[]@A[] {{new Class(), new Class()}}" in { parse("new Class[]@A[] {new Class(), new Class()}") shouldBe InitializedArrayInstantiation(ArrayType(ClassType(Vector(), None, "Class", Vector()), Vector(AbstractDimension(Vector), AbstractDimension(Vector(MarkerAnnotation("A"))))), ArrayInitializer(Vector(SimpleObjectInstantiation(Vector, ClassType(Vector(), None, "Class", Vector), Vector()), SimpleObjectInstantiation(Vector, ClassType(Vector(), None, "Class", Vector), Vector())))) }
    }
    "method invocation" - {
      "f()" in { parse("f()") shouldBe MethodInvocation(Vector, "f", Vector) }
      "f(a)" in { parse("f(a)") shouldBe MethodInvocation(Vector, "f", Vector(Select("a"))) }
      "f(a1, a2)" in { parse("f(a1, a2)") shouldBe MethodInvocation(Vector, "f", Vector(Select(Vector("a1")), Select("a2"))) }
      "Class.Name.f(a,a)" in { parse("Class.Name.f(a,a)") shouldBe QualifiedMethodInvocation(Select(Vector("Class", "Name")), Vector, "f", Vector(Select(Vector("a")), Select(Vector("a")))) }
      "Class.Name.<T> f(a,a)" in { parse("Class.Name.<T> f(a,a)") shouldBe QualifiedMethodInvocation(Select(Vector("Class", "Name")), Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), "f", Vector(Select(Vector("a")), Select(Vector("a")))) }
      "Class.Name . <T> f(a,a)" in { parse("Class.Name . <T> f(a,a)") shouldBe QualifiedMethodInvocation(Select(Vector("Class", "Name")), Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), "f", Vector(Select(Vector("a")), Select(Vector("a")))) }
      "expr.name.f(a,a)" in { parse("expr.name.f(a,a)") shouldBe QualifiedMethodInvocation(Select(Vector("expr", "name")), Vector, "f", Vector(Select(Vector("a")), Select(Vector("a")))) }
      "expr.name.<T> f(a,a)" in { parse("expr.name.<T> f(a,a)") shouldBe QualifiedMethodInvocation(Select(Vector("expr", "name")), Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), "f", Vector(Select(Vector("a")), Select(Vector("a")))) }
      "a.f()" in { parse("a.f()") shouldBe QualifiedMethodInvocation(Select("a"), Vector, "f", Vector) }
      "a[0].f()" in { parse("a[0].f()") shouldBe QualifiedMethodInvocation(Extraction(Select("a"), IntegerLiteral("0")), Vector, "f", Vector) }
      "a().f()" in { parse("a().f()") shouldBe QualifiedMethodInvocation(MethodInvocation(Vector, "a", Vector), Vector, "f", Vector) }
      "a[0].<T> f()" in { parse("a[0].<T> f()") shouldBe QualifiedMethodInvocation(Extraction(Select("a"), IntegerLiteral("0")), Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), "f", Vector) }
      "a.<T> f()" in { parse("a.<T> f()") shouldBe QualifiedMethodInvocation(Select("a"), Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), "f", Vector) }
      "a().<T> f()" in { parse("a().<T> f()") shouldBe QualifiedMethodInvocation(MethodInvocation(Vector, "a", Vector), Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), "f", Vector) }
      "super.f()" in { parse("super.f()") shouldBe QualifiedMethodInvocation(ParentReference, Vector, "f", Vector) }
      "Class.super.f()" in { parse("Class.super.f()") shouldBe QualifiedMethodInvocation(QualifiedParentReference("Class"), Vector, "f", Vector) }
      "super.<T> f()" in { parse("super.<T> f()") shouldBe QualifiedMethodInvocation(ParentReference, Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), "f", Vector) }
      "Class.super.<T> f()" in { parse("Class.super.<T> f()") shouldBe QualifiedMethodInvocation(QualifiedParentReference("Class"), Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), "f", Vector) }
      "f((a).f())" in { parse("f((a).f())") shouldBe MethodInvocation(Vector, "f", QualifiedMethodInvocation(Select("a"), Vector, "f", Vector)) }
    }
    "method reference" - {
      "expr.name :: f" in { parse("expr.name :: f") shouldBe MethodReference(Select(Vector("expr", "name")), Vector, "f") }
      "Class:: f" in { parse("Class:: f") shouldBe MethodReference(Select("Class"), Vector, "f") }
      "f[1] ::f" in { parse("f[1] ::f") shouldBe MethodReference(Extraction(Select("f"), IntegerLiteral("1")), Vector, "f") }
      "super::f" in { parse("super::f") shouldBe MethodReference(ParentReference, Vector, "f") }
      "Class.super ::f" in { parse("Class.super:: f") shouldBe MethodReference(QualifiedParentReference("Class"), Vector, "f") }

      "expr.name ::<T> f" in { parse("expr.name ::<T> f") shouldBe MethodReference(Select(Vector("expr", "name")), Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), "f") }
      "Class:: <T>f" in { parse("Class:: <T>f") shouldBe MethodReference(Select("Class"), Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), "f") }
      "f[1] ::<T>f" in { parse("f[1] ::<T>f") shouldBe MethodReference(Extraction(Select("f"), IntegerLiteral("1")), Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), "f") }
      "super::<T>f" in { parse("super::<T>f") shouldBe MethodReference(ParentReference, Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), "f") }
      "Class.super::<T>f" in { parse("Class.super::<T>f") shouldBe MethodReference(QualifiedParentReference("Class"), Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector))), "f") }
      "Class::new" in { parse("Class::new") shouldBe ConstructorReference(ClassType(Vector, None, "Class", Vector), Vector) }
      "Class::<T>new" in { parse("Class::<T>new") shouldBe ConstructorReference(ClassType(Vector, None, "Class", Vector), Vector(ArgumentTemplate(ClassType(Vector, None, "T", Vector)))) }
      "int@A[] :: new" in { parse("int@A[] :: new") shouldBe ArrayConstructorReference(ArrayType(IntegerPrimitive(Vector), Vector(AbstractDimension(Vector(MarkerAnnotation("A")))))) }
    }
  }
}