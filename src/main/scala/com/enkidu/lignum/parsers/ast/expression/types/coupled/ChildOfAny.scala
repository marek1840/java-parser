package com.enkidu.lignum.parsers.ast.expression.types.coupled

import com.enkidu.lignum.parsers.ast.expression.types.Type

case class ChildOfAny(types: Seq[Type]) extends CoupledType