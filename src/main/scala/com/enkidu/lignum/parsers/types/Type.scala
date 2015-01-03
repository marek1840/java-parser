package com.enkidu.lignum.parsers.types

import com.enkidu.lignum.parsers.expressions.{ Expression, Dimension }

trait Type extends Expression

case class ChildOfAll(types: Seq[Type]) extends Type
case class ChildOfAny(types: Seq[Type]) extends Type

object Type {
  case object Null extends Type
  case object Unit extends Type
  case object Nothing extends Type
}

sealed trait PrimitiveType extends Type

object PrimitiveType {
  case class Byte(annotations: Seq[Annotation]) extends PrimitiveType
  case class Short(annotations: Seq[Annotation]) extends PrimitiveType
  case class Integer(annotations: Seq[Annotation]) extends PrimitiveType
  case class Long(annotations: Seq[Annotation]) extends PrimitiveType
  case class Char(annotations: Seq[Annotation]) extends PrimitiveType
  case class Double(annotations: Seq[Annotation]) extends PrimitiveType
  case class Float(annotations: Seq[Annotation]) extends PrimitiveType
  case class Boolean(annotations: Seq[Annotation]) extends PrimitiveType
  case class Void(annotations: Seq[Annotation]) extends PrimitiveType
}

sealed trait ReferenceType extends Type

case class ClassType(annotations: Seq[Annotation], parent: Option[ClassType],
                     name: String, types: Seq[TemplateArgument]) extends ReferenceType
case class ArrayType(`type`: Type, dimensions: Seq[Dimension]) extends ReferenceType

sealed trait Annotation extends Type

case class NormalAnnotation(name: Seq[String], elements: Seq[(String, Expression)]) extends Annotation
case class SingleElementAnnotation(name: Seq[String], value: Expression) extends Annotation
case class MarkerAnnotation(name: Seq[String]) extends Annotation