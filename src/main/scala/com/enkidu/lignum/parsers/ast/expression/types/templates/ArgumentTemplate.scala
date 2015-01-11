package com.enkidu.lignum.parsers.ast.expression.types.templates

import com.enkidu.lignum.parsers.ast.expression.types.Type

case class ArgumentTemplate(`type`: Type) extends TemplateArgument {
  override def dispatch(visitor: Visitor): Unit = {
    `type`.dispatch(visitor)
    apply(visitor)
  }
}
