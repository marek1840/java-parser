package com.enkidu.lignum.parsers.java.v8

import com.enkidu.lignum.parsers.ast.expression.discardable.binary._
import com.enkidu.lignum.parsers.ast.expression.discardable.binary.assignment.{Binding, AugmentedBinding, Assignment}
import com.enkidu.lignum.parsers.ast.expression.discardable.dimension.{AbstractDimension, InitializedDimension, Dimension}
import com.enkidu.lignum.parsers.ast.expression.discardable.instantiation._
import com.enkidu.lignum.parsers.ast.expression.discardable._
import com.enkidu.lignum.parsers.ast.expression.discardable.literals._
import com.enkidu.lignum.parsers.ast.expression.discardable.unary.{PostDecrementation, PostIncrementation, PreDecrementation, PreIncrementation}
import com.enkidu.lignum.parsers.ast.expression.operators.{UnaryOperator, BinaryOperator}
import com.enkidu.lignum.parsers.ast.expression.types.annotations._
import com.enkidu.lignum.parsers.ast.expression.types.coupled.ChildOfAll
import com.enkidu.lignum.parsers.ast.expression.types.references._
import com.enkidu.lignum.parsers.ast.expression.types.templates._
import com.enkidu.lignum.parsers.ast.statement.declaration.members._
import com.enkidu.lignum.parsers.ast.statement.flow._
import com.enkidu.lignum.parsers.ast.statement.parameter._
import org.parboiled2.{ CharPredicate, ParserInput, Rule1, Rule2, RuleN }

import shapeless.HNil

import com.enkidu.lignum.parsers.ast.expression.types._
import com.enkidu.lignum.parsers.ast.expression._
import com.enkidu.lignum.parsers.ast.statement._

abstract class JavaExpressionParser extends JavaTypeParser {
  protected def classBody: Rule1[Seq[MemberDeclaration]]
  protected def formalParameters: Rule1[Seq[Parameter]]
  protected def block: Rule1[Block]

  def expression: Rule1[Expression] = rule {
    lambda | assignment | conditionalExpression
  }

  private def lambda: Rule1[Lambda] = rule {
    lambdaParameters ~ `->` ~ lambdaBody ~> Lambda
  }

  private def lambdaBody: Rule1[Statement] = rule {
    expression ~> ImplicitReturn | block
  }

  private def lambdaParameters: Rule1[Seq[Parameter]] = rule {
    identifier ~> ((e: String) => Vector(InferredParameter(e))) |
      `(` ~ (oneOrMore(identifier ~> InferredParameter) separatedBy comma) ~ `)` |
      `(` ~ optionalFormalParameters ~ `)`
  }

  private def optionalFormalParameters: Rule1[Seq[Parameter]] = rule {
    formalParameters | push(Vector())
  }

  protected def referenceNoMethodAccess: Rule1[Select] = rule {
    identifier ~ zeroOrMore(dot ~ identifier ~ !`(`) ~> ((i: String, is: Seq[String]) => Select(i +: is))
  }

  protected def primary: Rule1[DiscardableExpression] = rule {
    primaryBase ~ zeroOrMore {
      dot ~ {
        optionalTypeArguments ~ identifier ~ arguments ~> {
          (e: Expression, t: Seq[TemplateArgument], i: String, as: Seq[Expression]) =>
            QualifiedMethodInvocation(e, t, i, as)
        } | identifier ~> ((e: Expression, i: String) => FieldAccess(e, i)) |
          `new` ~ optionalTypeArguments ~ classTypeWithDaiamond ~ arguments ~ optional(classBody) ~> {
            (e: Expression, ts: Seq[TemplateArgument], t: Type, args: Seq[Expression], b: Option[Seq[MemberDeclaration]]) =>
              if (b.isDefined) NestedAnonymousObjectInstantiation(e, ts, t, args, b.get)
              else NestedObjectInstantiation(e, ts, t, args)
          }
      } | {
        `[` ~ expression ~ `]` ~> ((e1: Expression, e2: Expression) => Extraction(e1, e2)) |
          `::` ~ optionalTypeArguments ~ identifier ~> ((e1: Expression, ts: Seq[TemplateArgument], n: String) => MethodReference(e1, ts, n))
      }
    }
  }

  private def primaryBase: Rule1[Expression] = rule {
    `this` ~ push(ThisReference) |
      literal |
      classLiteral |
      `(` ~ expression ~ `)` |
      `super` ~ push(ParentReference) ~ {
        dot ~ {
          optionalTypeArguments ~ identifier ~ arguments ~> QualifiedMethodInvocation |
            identifier ~> FieldAccess
        } |
          `::` ~ optionalTypeArguments ~ identifier ~> MethodReference
      } |
      `new` ~ (classCreator | arrayCreator) |
      qualifiedIdentifier ~ {
        `[` ~ expression ~ `]` ~> ((s: Seq[String], e: Expression) => Extraction(Select(s), e)) |
          arguments ~> ((s: Seq[String], as: Seq[Expression]) =>
            if (s.size == 1) MethodInvocation(Vector(), s(0), as)
            else QualifiedMethodInvocation(Select(s.dropRight(1)), Vector(), s.last, as)) |
          dot ~ {
            `this` ~> QualifiedThisReference |
              `super` ~> QualifiedParentReference ~ {
                dot ~ {
                  optionalTypeArguments ~ identifier ~ arguments ~> QualifiedMethodInvocation |
                    identifier ~> FieldAccess
                } |
                  `::` ~ optionalTypeArguments ~ identifier ~> MethodReference
              }
          } |
          run { (s: Seq[String]) => Select(s) } ~ dot ~ {
            `new` ~ optionalTypeArguments ~ classTypeWithDaiamond ~ arguments ~ optional(classBody) ~> {
              (e: Expression, ts: Seq[TemplateArgument], t: Type, args: Seq[Expression], b: Option[Seq[MemberDeclaration]]) =>
                if (b.isDefined) NestedAnonymousObjectInstantiation(e, ts, t, args, b.get)
                else NestedObjectInstantiation(e, ts, t, args)
            } |
              typeArguments ~ identifier ~ arguments ~> QualifiedMethodInvocation
          } |
          `::` ~ optionalTypeArguments ~ identifier ~> MethodReference
      } |
      referenceType ~ `::` ~ optionalTypeArguments ~ identifier ~> MethodReference |
      classType ~ `::` ~ optionalTypeArguments ~ `new` ~> ConstructorReference |
      arrayType ~ `::` ~ `new` ~> ArrayConstructorReference
  }

  protected def classLiteral: Rule1[Literal] = rule {
    `type` ~ dot ~ `class` ~> ClassLiteral
  }

  protected def arguments: Rule1[Seq[Expression]] = rule { `(` ~ (zeroOrMore(expression) separatedBy comma) ~ `)` }

  private def classCreator: Rule1[ObjectInstantiation] = rule {
    optionalTypeArguments ~ classTypeWithDaiamond ~ arguments ~ {
      classBody ~> AnonymousObjectInstantiation |
        MATCH ~> SimpleObjectInstantiation
    }
  }

  private def classTypeWithDaiamond: Rule1[ClassType] = rule {
    annotations ~ push(None) ~ identifier ~ typeArgumentsOrDiamond ~> ClassType ~ {
      zeroOrMore(dot ~ annotations ~ identifier ~ typeArgumentsOrDiamond ~> {
        (p: ClassType, as: Seq[Annotation], id: String, args: Seq[TemplateArgument]) => ClassType(as, Some(p), id, args)
      })
    }
  }

  private def typeArgumentsOrDiamond: Rule1[Seq[TemplateArgument]] = rule {
    optionalTypeArguments | (`<` ~ `>` ~ !dot ~ push(Vector()))
  }

  private def arrayCreator: Rule1[ArrayInstantiation] = rule {
    `type` ~ dimExprs ~> { (t: Type, ss: Seq[Dimension]) => EmptyArrayInstantiation(t, ss) } |
      arrayType ~ arrayInitializer ~> InitializedArrayInstantiation
  }

  private def dimExprs: Rule1[Seq[Dimension]] = rule {
    oneOrMore(annotations ~ `[` ~ expression ~ `]` ~> InitializedDimension) ~
      zeroOrMore(annotations ~ `[` ~ `]` ~> AbstractDimension) ~> {
        (d1: Seq[InitializedDimension], d2: Seq[AbstractDimension]) => d1 ++: d2
      }
  }

  private def assignmentExpression: Rule1[Expression] = rule { conditionalExpression | assignment }

  protected def assignment: Rule1[Assignment] = rule {
    (primary | reference) ~ {
      `=` ~ expression ~> Binding | assignmentOps ~ expression ~> AugmentedBinding
    }
  }

  protected def conditionalExpression: Rule1[Expression] = rule {
    or ~ optional(`?` ~ expression ~ colon ~ conditionalExpression ~> TernaryConditional)
  }

  private def or: Rule1[Expression] = rule { binaryOperation(orOp, and) }
  private val and = () => rule { binaryOperation(andOp, binOr) }
  private val binOr = () => rule { binaryOperation(binOrOp, xor) }
  private val xor = () => rule { binaryOperation(xorOp, binAnd) }
  private val binAnd = () => rule { binaryOperation(binAndOp, equality) }
  private val equality = () => rule { binaryOperation(eqOps, () => relational) }

  private def relational: Rule1[Expression] = rule {
    shift() ~> ((expr: Expression) => (Vector[BinaryOperator](), Vector(expr))) ~ {
      zeroOrMore {
        (relOps() ~ shift() | `instanceof` ~ push(BinaryOperator.instanceof) ~ referenceType) ~> (
          (acc: (Vector[BinaryOperator], Vector[Expression]), op: BinaryOperator, expr: Expression) =>
            (acc._1 :+ op, acc._2 :+ expr))
      } ~> ((acc: (Vector[BinaryOperator], Vector[Expression])) =>
        if (acc._2.size == 1) acc._2(0) else BinaryOperations(acc._1, acc._2))
    }
  }

  private val shift = () => rule { binaryOperation(shiftOps, additive) }
  private val additive = () => rule { binaryOperation(addOps, multiplicative) }
  private val multiplicative = () => rule { binaryOperation(mulOps, () => unary) }

  protected def unary: Rule1[Expression] = rule {
    `++` ~ unary ~> PreIncrementation | `--` ~ unary ~> PreDecrementation |
      oneOrMore(unaryOps) ~ unary ~> (UnaryOperations((_: Seq[UnaryOperator]), (_: Expression))) |
      cast | postfix
  }

  private def postfix: Rule1[Expression] = rule {
    (primary | reference) ~ optional {
      `++` ~> PostIncrementation | `--` ~> PostDecrementation
    }
  }

  protected def reference: Rule1[Expression] = rule {
    qualifiedIdentifier ~> ((name: Seq[String]) => Select(name))
  }

  private def cast: Rule1[Cast] = rule {
    {
      `(` ~ primitiveType ~ `)` ~ unary |
        `(` ~ (referenceType ~ zeroOrMore(additionalBound)) ~> {
          (t: Type, ts: Seq[ClassType]) => if (ts.size == 0) t else ChildOfAll(t +: ts)
        } ~ `)` ~ (lambda | postfix)
    } ~> Cast
  }

  protected def arrayInitializer: Rule1[ArrayInitializer] = rule {
    `{` ~ (zeroOrMore(expression | arrayInitializer) separatedBy comma) ~ optional(comma) ~ `}` ~> ArrayInitializer
  }

  private def binaryOperation(operator: () => Rule1[BinaryOperator], next: () => Rule1[Expression]): Rule1[Expression] = rule {
    next() ~> ((expr: Expression) => (Vector[BinaryOperator](), Vector(expr))) ~ {
      zeroOrMore {
        operator() ~ next() ~> ((acc: (Vector[BinaryOperator], Vector[Expression]), op: BinaryOperator, expr: Expression) =>
          (acc._1 :+ op, acc._2 :+ expr))
      }
    } ~> ((acc: (Vector[BinaryOperator], Vector[Expression])) =>
      if (acc._2.size == 1) acc._2(0) else BinaryOperations(acc._1, acc._2))
  }

  private def assignmentOps: Rule1[BinaryOperator] = rule {
    `*=` ~ push(BinaryOperator.*) | `/=` ~ push(BinaryOperator./) |
      `%=` ~ push(BinaryOperator.mod) | `+=` ~ push(BinaryOperator.+) |
      `-= ` ~ push(BinaryOperator.-) | `<<=` ~ push(BinaryOperator.<<) |
      `>>= ` ~ push(BinaryOperator.>>) | `>>>=` ~ push(BinaryOperator.>>>) |
      `&=` ~ push(BinaryOperator.&) | `^=` ~ push(BinaryOperator.^) |
      `|=` ~ push(BinaryOperator.|)
  }

  private def unaryOps: Rule1[UnaryOperator] = rule {
    plus ~ push(UnaryOperator.+) | minus ~ push(UnaryOperator.-) |
      tilde ~ push(UnaryOperator.~) | exclamation ~ push(UnaryOperator.not)
  }

  private val relOps = () => rule {
    `<=` ~ push(BinaryOperator.<=) | `<` ~ push(BinaryOperator.<) |
      `>=` ~ push(BinaryOperator.>=) | `>` ~ push(BinaryOperator.>)
  }

  private val mulOps = () => rule { `*` ~ push(BinaryOperator.*) | `/` ~ push(BinaryOperator./) | `%` ~ push(BinaryOperator.mod) }
  private val addOps = () => rule { `plus` ~ push(BinaryOperator.+) | `minus` ~ push(BinaryOperator.-) }
  private val shiftOps = () => rule { `>>>` ~ push(BinaryOperator.>>>) | `>>` ~ push(BinaryOperator.>>) | `<<` ~ push(BinaryOperator.<<) }
  private val eqOps = () => rule { `==` ~ push(BinaryOperator.==) | `!=` ~ push(BinaryOperator.!=) }
  private val binAndOp = () => rule { `&` ~ push(BinaryOperator.&) }
  private val xorOp = () => rule { `^` ~ push(BinaryOperator.^) }
  private val binOrOp = () => rule { `|` ~ push(BinaryOperator.|) }
  private val andOp = () => rule { `&&` ~ push(BinaryOperator.and) }
  private val orOp = () => rule { `||` ~ push(BinaryOperator.or) }
}