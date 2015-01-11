package com.enkidu.lignum.parsers.ast.expression.operators

sealed trait UnaryOperator

object UnaryOperator {

  case object + extends UnaryOperator

  case object - extends UnaryOperator

  case object ~ extends UnaryOperator

  case object not extends UnaryOperator

}