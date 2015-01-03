package com.enkidu.lignum.parsers.expressions

sealed trait BinaryOperator

object BinaryOperator {
  case object or extends BinaryOperator
  case object and extends BinaryOperator
  case object instanceof extends BinaryOperator

  case object + extends BinaryOperator
  case object - extends BinaryOperator
  case object * extends BinaryOperator
  case object / extends BinaryOperator
  case object ** extends BinaryOperator //power
  case object ^ extends BinaryOperator //xor
  case object & extends BinaryOperator
  case object | extends BinaryOperator
  case object << extends BinaryOperator
  case object <<< extends BinaryOperator
  case object >> extends BinaryOperator
  case object >>> extends BinaryOperator
  case object mod extends BinaryOperator
  case object div extends BinaryOperator

  case object < extends BinaryOperator
  case object <= extends BinaryOperator
  case object > extends BinaryOperator
  case object >= extends BinaryOperator
  case object == extends BinaryOperator
  case object != extends BinaryOperator
  case object in extends BinaryOperator
  case object notIn extends BinaryOperator
  case object is extends BinaryOperator
  case object isNot extends BinaryOperator

}