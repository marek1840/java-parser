package com.enkidu.lignum.parsers.ast.expression.types.primitives

import com.enkidu.lignum.parsers.ast.expression.types.annotations.Annotation

case class VoidPrimitive(annotations: Seq[Annotation]) extends PrimitiveType
