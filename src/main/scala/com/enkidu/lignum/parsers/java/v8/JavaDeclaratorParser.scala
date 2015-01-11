package com.enkidu.lignum.parsers.java.v8

import com.enkidu.lignum.parsers.ast.expression.Expression
import com.enkidu.lignum.parsers.ast.expression.discardable.dimension.Dimension
import com.enkidu.lignum.parsers.ast.statement.declarator._
import org.parboiled2._

abstract class JavaDeclaratorParser extends JavaParameterParser{
  protected def constructorDeclarator: Rule1[ConstructorDeclarator] = rule {
    identifier ~ `(` ~ formalParameters ~ `)` ~> ConstructorDeclarator
  }

  protected def methodDeclarator: Rule1[FunctionDeclarator] = rule {
    identifier ~ `(` ~ formalParameters ~ `)` ~ {
      dims ~> ArrayMethodDeclarator |
        MATCH ~> MethodDeclarator
    }
  }

  protected def variableDeclaratorId: Rule2[String, Seq[Dimension]] = rule {
    identifier ~ optionalDims
  }

  protected def variableDeclarators: Rule1[Seq[Declarator]] = rule {
    zeroOrMore {
      variableDeclaratorId ~ optional(`=` ~ (expression | arrayInitializer)) ~> {
        (n: String, ds: Seq[Dimension], i: Option[Expression]) =>
          if (ds.size == 0 && i.isDefined) InitializedVariableDeclarator(n, i.get)
          else if (i.isDefined) InitializedArrayDeclarator(n, ds, i.get)
          else if (ds.size == 0) VariableDeclarator(n)
          else ArrayDeclarator(n, ds)
      }
    } separatedBy comma
  }
}
