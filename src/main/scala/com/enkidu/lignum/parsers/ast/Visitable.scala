package com.enkidu.lignum.parsers.ast


protected[ast] trait Visitable {
  protected type Visitor = PartialFunction[Visitable, Unit]

  def dispatch(visitor: Visitor) = apply(visitor)

  protected def apply(visitor: Visitor) = if (visitor isDefinedAt this) visitor(this)

  protected implicit class VisitableSeq[A <: Visitable](seq: Seq[A]) {
    def dispatch(visitor: Visitor) = seq.foreach(_.dispatch(visitor))
  }

  protected implicit class VisitableOption[A <: Visitable](opt: Option[A]) {
    def dispatch(visitor: Visitor) = opt.foreach(_.dispatch(visitor))
  }

}
