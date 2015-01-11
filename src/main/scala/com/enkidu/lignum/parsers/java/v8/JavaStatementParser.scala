package com.enkidu.lignum.parsers.java.v8

import com.enkidu.lignum.parsers.ast.expression._
import com.enkidu.lignum.parsers.ast.expression.discardable.unary._
import com.enkidu.lignum.parsers.ast.expression.types._
import com.enkidu.lignum.parsers.ast.expression.types.annotations.Annotation
import com.enkidu.lignum.parsers.ast.expression.types.coupled.ChildOfAny
import com.enkidu.lignum.parsers.ast.expression.types.references._
import com.enkidu.lignum.parsers.ast.statement._
import com.enkidu.lignum.parsers.ast.statement.conditional._
import com.enkidu.lignum.parsers.ast.statement.declaration._
import com.enkidu.lignum.parsers.ast.statement.declarator._
import com.enkidu.lignum.parsers.ast.statement.flow._
import com.enkidu.lignum.parsers.ast.statement.interruptable._
import com.enkidu.lignum.parsers.ast.statement.loop._
import com.enkidu.lignum.parsers.ast.statement.switch._
import org.parboiled2.{Rule1, Rule2}

abstract class JavaStatementParser extends JavaDeclaratorParser {
  protected def variableDeclaration: Rule1[LocalVariableDeclaration]
  protected def classDeclaration: Rule1[Statement]


  protected def block: Rule1[Block] = rule {
    `{` ~ zeroOrMore(blockStatement) ~ `}` ~> Block
  }

  protected def blockStatements: Rule1[Seq[Statement]] = rule {
    oneOrMore(blockStatement)
  }

  def blockStatement: Rule1[Statement] = rule {
    variableDeclaration ~ semicolon |
      classDeclaration | statement
  }

  private def statement: Rule1[Statement] = rule {
    whileLoop | forLoop | labeledStatement | conditionalStatement | statementWithoutTrailingSubstatement
  }

  private def statementWithoutTrailingSubstatement: Rule1[Statement] = rule {
    block | emptyStatement | discardableExpressionStatement ~ semicolon |
      assertStatement | breakStatement | continueStatement | returnStatement |
      throwStatement | doWhileLoop | tryStatement | switchStatement |
      synchronizedBlock
  }

  protected def emptyStatement: Rule1[Statement] = rule {
    semicolon ~ push(EmptyStatement)
  }

  private def labeledStatement: Rule1[LabeledStatement] = rule {
    identifier ~ colon ~ statement ~> LabeledStatement
  }

  private def discardableExpressionStatement: Rule1[Statement] = rule {
    assignment |
      reference ~ (`++` ~> PostIncrementation | `--` ~> PostDecrementation) |
      primary ~ optional(`++` ~> PostIncrementation | `--` ~> PostDecrementation) |
      `++` ~ unary ~> PreIncrementation | `--` ~ unary ~> PreDecrementation
  }

  private def assertStatement: Rule1[Assertion] = rule {
    `assert` ~ expression ~ optional(colon ~ expression) ~> Assertion
  }

  private def breakStatement: Rule1[Statement] = rule {
    (`break` ~ identifier ~> TargetedBreak | `break` ~ push(Break)) ~ semicolon
  }

  private def continueStatement: Rule1[Statement] = rule {
    (`continue` ~ identifier ~> TargetedContinue | `continue` ~ push(Continue)) ~ semicolon
  }

  private def returnStatement: Rule1[Statement] = rule {
    (`return` ~ expression ~> Return | `return` ~ push(EmptyReturn)) ~ semicolon
  }

  private def throwStatement: Rule1[FlowStatement] = rule {
    `throw` ~ expression ~ semicolon ~> Throw
  }

  private def conditionalStatement: Rule1[ConditionalStatement] = rule {
    `if` ~ `(` ~ expression ~ `)` ~ statement ~ {
      `else` ~ statement ~> IfThenElse |
        MATCH ~> If
    }
  }

  private def tryStatement: Rule1[TryStatement] = rule {
    `try` ~ {
      resources ~ block ~ optional(catches) ~ optional(finallyBlock) ~> {
        (rs: Seq[LocalVariableDeclaration], tb: Block, cs: Option[Seq[CatchClause]], fb: Option[Block]) =>
          TryWithResources(rs, tb, cs.getOrElse(Vector()), fb)
      } |
        block ~ {
          finallyBlock ~> TryFinally |
            catches ~ {
              finallyBlock ~> TryCatchFinally |
                MATCH ~> TryCatch
            }
        }
    }
  }

  private def switchStatement = rule {
    `switch` ~ `(` ~ expression ~ `)` ~ `{` ~ {
      zeroOrMore(oneOrMore(switchLabel) ~ oneOrMore(blockStatement) ~> SwitchCases) ~
        zeroOrMore(switchLabel) ~> ((es: Seq[Expression]) => if (es.size == 0) None else Some(EmptySwitchCases(es)))
    } ~ `}` ~> {
      (e: Expression, cs1: Seq[SwitchRule], c2: Option[SwitchRule]) =>
        if (c2.isDefined) SwitchStatement(e, cs1 :+ c2.get) else SwitchStatement(e, cs1)
    }
  }

  private def switchLabel: Rule1[Expression] = rule {
    (`case` ~ (expression | reference) | `default` ~ push(DefaultSwitch)) ~ colon
  }

  private def synchronizedBlock: Rule1[SynchronizedBlock] = rule {
    `synchronized` ~ optional(`(` ~ expression ~ `)`) ~ block ~> SynchronizedBlock
  }

  private def catches: Rule1[Seq[CatchClause]] = rule {
    zeroOrMore {
      `catch` ~ `(` ~ catchFormalParameter ~ `)` ~ block ~> CatchClause
    }
  }

  private def finallyBlock: Rule1[Block] = rule {
    `finally` ~ block
  }

  private def catchFormalParameter: Rule1[LocalVariableDeclaration] = rule {
    variableModifiers ~ ((zeroOrMore(classType) separatedBy `|`) ~> (
      (seq: Seq[ClassType]) => if (seq.size == 1) seq(0) else ChildOfAny(seq))) ~ identifier ~> {
      (as: Seq[Annotation], _: Boolean, t: Type, n: String) =>
        LocalVariableDeclaration(as, isFinal = true, t, Vector(VariableDeclarator(n)))
    }
  }

  private def resources: Rule1[Seq[LocalVariableDeclaration]] = rule {
    `(` ~ (oneOrMore(resource) separatedBy semicolon) ~ optional(semicolon) ~ `)`
  }

  private def resource: Rule1[LocalVariableDeclaration] = rule {
    variableModifiers ~ classType ~ identifier ~ `=` ~ expression ~> {
      (as: Seq[Annotation], _: Boolean, t: Type, n: String, e: Expression) =>
        LocalVariableDeclaration(as, isFinal = true, t, Vector(InitializedVariableDeclarator(n, e)))
    }
  }

  private def doWhileLoop: Rule1[Loop] = rule {
    `do` ~ statement ~ `while` ~ `(` ~ expression ~ `)` ~ semicolon ~> DoWhile
  }

  private def whileLoop: Rule1[While] = rule {
    `while` ~ `(` ~ expression ~ `)` ~ statement ~> While
  }

  private def forLoop: Rule1[Loop] = rule {
    `for` ~ `(` ~ variableDeclaration ~ colon ~ expression ~ `)` ~ statement ~> Iteration |
      `for` ~ `(` ~ forInit ~ semicolon ~ optional(expression) ~ semicolon ~ forUpdate ~ `)` ~ statement ~> For
  }

  private def forInit: Rule1[Seq[Statement]] = rule {
    (oneOrMore(discardableExpressionStatement) separatedBy comma) |
      variableDeclaration ~> ((d: LocalVariableDeclaration) => Vector(d)) |
      push(Vector())
  }

  private def forUpdate: Rule1[Seq[Statement]] = rule {
    zeroOrMore(discardableExpressionStatement) separatedBy comma
  }
}