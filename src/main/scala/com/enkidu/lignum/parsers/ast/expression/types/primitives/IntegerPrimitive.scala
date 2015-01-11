package com.enkidu.lignum.parsers.ast.expression.types.primitives

import com.enkidu.lignum.parsers.ast.expression.types.annotations.Annotation

case class IntegerPrimitive(annotations: Seq[Annotation]) extends PrimitiveType
