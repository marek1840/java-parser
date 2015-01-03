package com.enkidu.lignum.parsers.expressions

sealed trait UnaryOperator

object UnaryOperator{
   case object + extends UnaryOperator
   case object - extends UnaryOperator
   case object ~ extends UnaryOperator

   case object not extends UnaryOperator
}