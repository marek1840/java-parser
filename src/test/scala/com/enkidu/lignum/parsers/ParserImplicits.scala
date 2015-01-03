package com.enkidu.lignum.parsers

import scala.language.implicitConversions

import org.scalatest.Matchers

import com.enkidu.lignum.parsers.expressions.PrimitiveLiteral

trait ParserImplicits extends Matchers{
  implicit def toSeq[A](a: A): Seq[A] = a +: Vector
  implicit def toSome[A](a:A):Option[A] = Some(a)

  implicit def int2Literal(int: Int): PrimitiveLiteral = PrimitiveLiteral(int.toString())
  implicit def string2Literal(string: String): PrimitiveLiteral = PrimitiveLiteral(string)

  val Vector = collection.immutable.Vector()
  def Vector[A](a:A*) = a.toVector
}