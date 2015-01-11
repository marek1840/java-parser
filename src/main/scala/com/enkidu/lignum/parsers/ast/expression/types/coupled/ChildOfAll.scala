package com.enkidu.lignum.parsers.ast.expression.types.coupled

import com.enkidu.lignum.parsers.ast.expression.types.Type

case class ChildOfAll(types: Seq[Type]) extends CoupledType
