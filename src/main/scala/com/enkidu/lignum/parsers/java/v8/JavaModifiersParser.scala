package com.enkidu.lignum.parsers.java.v8

import com.enkidu.lignum.parsers.ast.expression.types.annotations.Annotation
import com.enkidu.lignum.parsers.ast.statement.modifers._
import org.parboiled2._
import shapeless.HNil

abstract class JavaModifiersParser extends JavaExpressionParser {
  def annotation: Rule1[Annotation]

  protected def classModifiers: Rule2[Seq[Annotation], Seq[Modifier]] = rule {
    zeroOrMore(annotation | classModifier) ~> { (seq: Seq[Any]) =>
      var as = Vector[Annotation]()
      var ms = Vector[Modifier]()

      for (i <- seq) {
        i match {
          case annotation: Annotation => as = as :+ annotation
          case modifier: Modifier => ms = ms :+ modifier
          case _ =>
        }
      }
      as :: ms :: HNil
    }
  }

  protected def constuctorModifiers: Rule2[Seq[Annotation], Seq[Modifier]] = rule {
    push((Vector(), Vector())) ~ zeroOrMore {
      annotation ~> ((t: (Seq[Annotation], Seq[Modifier]), a: Annotation) => (t._1 :+ a, t._2)) |
        constructorModifier ~> ((t: (Seq[Annotation], Seq[Modifier]), m: Modifier) => (t._1, t._2 :+ m))
    } ~> ((t: (Seq[Annotation], Seq[Modifier])) => t._1 :: t._2 :: HNil)
  }

  protected def methodModifiers: Rule2[Seq[Annotation], Seq[Modifier]] = rule {
    push((Vector(), Vector())) ~ zeroOrMore {
      annotation ~> ((t: (Seq[Annotation], Seq[Modifier]), a: Annotation) => (t._1 :+ a, t._2)) |
        methodModifier ~> ((t: (Seq[Annotation], Seq[Modifier]), m: Modifier) => (t._1, t._2 :+ m))
    } ~> ((t: (Seq[Annotation], Seq[Modifier])) => t._1 :: t._2 :: HNil)
  }

  protected def fieldModifiers: Rule2[Seq[Annotation], Seq[Modifier]] = rule {
    push((Vector(), Vector())) ~ zeroOrMore {
      annotation ~> ((t: (Seq[Annotation], Seq[Modifier]), a: Annotation) => (t._1 :+ a, t._2)) |
        fieldModifier ~> ((t: (Seq[Annotation], Seq[Modifier]), m: Modifier) => (t._1, t._2 :+ m))
    } ~> ((t: (Seq[Annotation], Seq[Modifier])) => t._1 :: t._2 :: HNil)
  }

  protected def variableModifiers: Rule2[Seq[Annotation], Boolean] = rule {
    annotations ~ optional(`final` ~ push(true)) ~ annotations ~> {
      (as1: Seq[Annotation], f: Option[Boolean], as2: Seq[Annotation]) =>
        (as1 ++ as2) :: f.getOrElse(false) :: HNil
    }
  }

  protected def interfaceModifiers: Rule2[Seq[Annotation], Seq[Modifier]] = rule {
    push((Vector(), Vector())) ~ zeroOrMore {
      annotation ~> ((t: (Seq[Annotation], Seq[Modifier]), a: Annotation) => (t._1 :+ a, t._2)) |
        interfaceModifier ~> ((t: (Seq[Annotation], Seq[Modifier]), m: Modifier) => (t._1, t._2 :+ m))
    } ~> ((t: (Seq[Annotation], Seq[Modifier])) => t._1 :: t._2 :: HNil)
  }


  protected def constantModifiers: Rule2[Seq[Annotation], Seq[Modifier]] = rule {
    push((Vector(), Vector())) ~ zeroOrMore {
      annotation ~> ((t: (Seq[Annotation], Seq[Modifier]), a: Annotation) => (t._1 :+ a, t._2)) |
        constantModifier ~> ((t: (Seq[Annotation], Seq[Modifier]), m: Modifier) => (t._1, t._2 :+ m))
    } ~> ((t: (Seq[Annotation], Seq[Modifier])) => t._1 :: t._2 :: HNil)
  }


  protected def interfaceMethodModifiers: Rule2[Seq[Annotation], Seq[Modifier]] = rule {
    push((Vector(), Vector())) ~ zeroOrMore {
      annotation ~> ((t: (Seq[Annotation], Seq[Modifier]), a: Annotation) => (t._1 :+ a, t._2)) |
        interfaceMethodModifier ~> ((t: (Seq[Annotation], Seq[Modifier]), m: Modifier) => (t._1, t._2 :+ m))
    } ~> ((t: (Seq[Annotation], Seq[Modifier])) => t._1 :: t._2 :: HNil)
  }


  protected def annotationElementModifiers: Rule2[Seq[Annotation], Seq[Modifier]] = rule {
    push((Vector(), Vector())) ~ zeroOrMore {
      annotation ~> ((t: (Seq[Annotation], Seq[Modifier]), a: Annotation) => (t._1 :+ a, t._2)) |
        annotationElementModifier ~> ((t: (Seq[Annotation], Seq[Modifier]), m: Modifier) => (t._1, t._2 :+ m))
    } ~> ((t: (Seq[Annotation], Seq[Modifier])) => t._1 :: t._2 :: HNil)
  }

  protected def classModifier: Rule1[Modifier] = rule {
    `public` ~ push(Public) | `protected` ~ push(Protected) |
      `private` ~ push(Private) | `abstract` ~ push(Abstract) |
      `static` ~ push(Static) | `final` ~ push(Final) |
      `strictfp` ~ push(Strictfp)
  }

  protected def constructorModifier: Rule1[Modifier] = rule {
    `public` ~ push(Public) |
      `protected` ~ push(Protected) |
      `private` ~ push(Private)
  }


  protected def methodModifier: Rule1[Modifier] = rule {
    `public` ~ push(Public) | `protected` ~ push(Protected) |
      `private` ~ push(Private) | `abstract` ~ push(Abstract) |
      `static` ~ push(Static) | `final` ~ push(Final) |
      `synchronized` ~ push(Synchronized) | `native` ~ push(Native) |
      `strictfp` ~ push(Strictfp)
  }

  protected def fieldModifier: Rule1[Modifier] = rule {
    `public` ~ push(Public) | `protected` ~ push(Protected) |
      `private` ~ push(Private) | `static` ~ push(Static) |
      `final` ~ push(Final) | `transient` ~ push(Transient) |
      `volatile` ~ push(Volatile)
  }

  protected def interfaceMethodModifier: Rule1[Modifier] = rule {
    `public` ~ push(Public) | `static` ~ push(Static) |
      `abstract` ~ push(Abstract) | `default` ~ push(Default) |
      `strictfp` ~ push(Strictfp)
  }

  protected def annotationElementModifier: Rule1[Modifier] = rule {
    `public` ~ push(Public) | `abstract` ~ push(Abstract)
  }

  protected def constantModifier: Rule1[Modifier] = rule {
    `public` ~ push(Public) | `static` ~ push(Static) | `final` ~ push(Final)
  }

  protected def interfaceModifier: Rule1[Modifier] = rule {
    `public` ~ push(Public) | `protected` ~ push(Protected) |
      `private` ~ push(Private) | `abstract` ~ push(Abstract) |
      `static` ~ push(Static) | `strictfp` ~ push(Strictfp)
  }

}
