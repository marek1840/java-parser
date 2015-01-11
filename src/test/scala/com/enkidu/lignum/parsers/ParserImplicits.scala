package com.enkidu.lignum.parsers

import com.enkidu.lignum.parsers.ast.expression.discardable.literals._
import org.scalatest.Matchers

import scala.language.implicitConversions


trait ParserImplicits extends Matchers{
  implicit def toSeq[A](a: A): Seq[A] = a +: Vector
  implicit def toSome[A](a:A):Option[A] = Some(a)

  implicit def int2Literal(int: Int): IntegerLiteral = IntegerLiteral(int.toString)
  implicit def string2Literal(string: String): StringLiteral = StringLiteral(string)

  val Vector = collection.immutable.Vector()
  def Vector[A](a:A*) = a.toVector
}