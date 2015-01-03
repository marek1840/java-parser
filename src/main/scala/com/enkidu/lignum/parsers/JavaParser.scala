package com.enkidu.lignum.parsers

import org.parboiled2.{ CharPredicate, ParserInput, Rule1, Rule2, RuleN }

import shapeless.HNil

import com.enkidu.lignum.parsers.types._
import com.enkidu.lignum.parsers.expressions._
import com.enkidu.lignum.parsers.statements._

class JavaParser(val input: ParserInput) extends AbstractParser {

  ////////////////////
// Lexical Structure

def identifier = rule {
    !keyword ~ capture(identifierStart ~ zeroOrMore(identifierChar)) ~ whitespace
  }

  def qualifiedIdentifier: Rule1[Seq[String]] = rule { oneOrMore(identifier) separatedBy `dot` }

  protected def keyword = rule {
    ("abstract" | "continue" | "for" | "new" | "switch" | "assert" | "default" | "if" | "package" | "synchronized" |
      "boolean" | "double" | "do" | "goto" | "private" | "this" | "break" | "implements" | "protected" | "throws" |
      "throw" | "byte" | "else" | "import" | "public" | "case" | "enum" | "instanceof" | "return" | "transient" |
      "catch" | "extends" | "interface" | "int" | "short" | "try" | "char" | "finally" | "final" | "static" | "void" |
      "class" | "long" | "strictfp" | "volatile" | "const" | "float" | "native" | "super" | "while" | "long") ~ !(identifierChar) ~ whitespace
  }

  def literal: Rule1[Literal] = rule {
    { floatLiteral | integerLiteral | characterLiteral | stringLiteral | nullLiteral | booleanLiteral  } ~ whitespace
  }

  protected def integerLiteral: Rule1[PrimitiveLiteral] = rule {
    capture((hexNumber | binaryNumber | octalNumber | decimalNumber) ~ optionalIntegerSuffix) ~> PrimitiveLiteral
  }

  protected def floatLiteral: Rule1[PrimitiveLiteral] = rule { (hexFloat | decimalFloat) ~> PrimitiveLiteral }

  protected def booleanLiteral: Rule1[PrimitiveLiteral] = rule { capture(`true` | `false`) ~> PrimitiveLiteral }

  protected def characterLiteral: Rule1[PrimitiveLiteral] = rule {
    capture("'" ~ (escapeSequence | unicodeEscape | noneOf("'\\" + 10.toChar + 13.toChar)) ~ "'") ~> PrimitiveLiteral
  }

  protected def stringLiteral: Rule1[PrimitiveLiteral] = rule {
    capture('"' ~ zeroOrMore(escapeSequence | noneOf("\\\"\n\r")) ~ '"') ~> PrimitiveLiteral
  }

  protected def nullLiteral: Rule1[PrimitiveLiteral] = rule { capture(`null`) ~> PrimitiveLiteral }

  private def unicodeEscape = rule {
    "\\" ~ oneOrMore("u") ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit
  }


  private def optionalIntegerSuffix = rule(optional("l" | "L"))
  private def optionalFloatSuffix = rule(optional("f" | "F" | "d" | "D"))
  private def exponent = rule(("e" | "E") ~ signedInteger)
  private def signedInteger = rule { optional("+" | "-") ~ zeroOrMore(decimalDigit) }

  private def decimalFloat: Rule1[String] = rule {
    capture {
      decimalNumber ~ "." ~ zeroOrMore(decimalDigit) ~ optional(exponent) ~ optionalFloatSuffix |
        decimalNumber ~ exponent ~ optionalFloatSuffix |
        decimalNumber ~ optional(exponent) ~ ("f" | "F" | "d" | "D") |
        "." ~ oneOrMore(decimalDigit) ~ optional(exponent) ~ optionalFloatSuffix
    }
  }

  private def hexFloat: Rule1[String] = rule {
    capture(("0x" | "0X") ~ {
      (zeroOrMore(hexDigit) ~ "." ~ oneOrMore(hexDigit)) | (oneOrMore(hexDigit) ~ optional("."))
    } ~ ("p" | "P") ~ signedInteger ~ optionalFloatSuffix)
  }

  ///////////////////////////
  // Types, Values, Variables

  def `type`: Rule1[Type] = rule {
    (primitiveType | classType) ~ optionalDims ~> {
      (t: Type, ds: Seq[Dimension]) => if (ds.size == 0) t else ArrayType(t, ds)
    }
  }

  private def primitiveType: Rule1[PrimitiveType] = rule {
    annotations ~ {
      `byte` ~> PrimitiveType.Byte |
        `short` ~> PrimitiveType.Short |
        `float` ~> PrimitiveType.Float |
        `int` ~> PrimitiveType.Integer |
        `long` ~> PrimitiveType.Long |
        `char` ~> PrimitiveType.Char |
        `double` ~> PrimitiveType.Double |
        `boolean` ~> PrimitiveType.Boolean |
        `void` ~> PrimitiveType.Void
    }
  }

  private def referenceType: Rule1[ReferenceType] = rule { arrayType | classType }

  private def classType: Rule1[ClassType] = rule {
    annotations ~ push(None) ~ identifier ~ optionalTypeArguments ~> ClassType ~ {
      zeroOrMore(dot ~ annotations ~ identifier ~ optionalTypeArguments ~> {
        (p: ClassType, as: Seq[Annotation], id: String, args: Seq[TemplateArgument]) => ClassType(as, Some(p), id, args)
      })
    }
  }

  private def arrayType: Rule1[ArrayType] = rule { (primitiveType | classType) ~ dims ~> ArrayType }

  private def dims: Rule1[Seq[Dimension]] = rule {
    oneOrMore(annotations ~ `[` ~ `]` ~> AbstractDimension)
  }

  private def optionalDims: Rule1[Seq[Dimension]] = rule {
    zeroOrMore(annotations ~ `[` ~ `]` ~> AbstractDimension)
  }

  private def typeParameter: Rule1[TemplateParameter] = rule {
    annotations ~ identifier ~ {
      typeBound ~> Template.BoundedParameter |
        MATCH ~> Template.Parameter
    }
  }

  private def typeBound: Rule1[Type] = rule {
    `extends` ~ classType ~ zeroOrMore(additionalBound) ~> ((t: ClassType, is: Seq[ClassType]) =>
      if (is.size == 0) t else ChildOfAll(t +: is))
  }

  private def additionalBound: Rule1[ClassType] = rule { `&` ~ classType }

  private def optionalTypeArguments: Rule1[Seq[TemplateArgument]] = rule {
    `<` ~ (zeroOrMore(typeArgument) separatedBy (comma)) ~ `>` | push(Vector())
  }

  private def typeArguments: Rule1[Seq[TemplateArgument]] = rule {
    `<` ~ (zeroOrMore(typeArgument) separatedBy (comma)) ~ `>`
  }

  private def typeArgument: Rule1[TemplateArgument] = rule {
    referenceType ~> Template.Argument |
      annotations ~ {
        `?` ~ `extends` ~ referenceType ~> Template.AnySubClass |
          `?` ~ `super` ~ referenceType ~> Template.AnyBaseClass |
          `?` ~> Template.Any
      }
  }

  ///////////
  // Packages

  def compilationUnit: Rule1[Declaration.CompilationUnit] = rule {
    whitespace ~ packageDeclaration ~ importDeclarations ~ typeDeclarations ~ EOI ~> Declaration.CompilationUnit
  }

  private def typeDeclarations: Rule1[Seq[TypeDeclaration]] = rule {
    zeroOrMore(typeDeclaration)
  }

  def typeDeclaration: Rule1[TypeDeclaration] = rule {
    classDeclaration |
      enumDeclaration |
      interfaceDeclaration |
      annotationDeclaration |
      emptyDeclaration
  }

  private def packageDeclaration: Rule1[PackageDeclaration] = rule {
    annotations ~ `package` ~ (oneOrMore(identifier) separatedBy dot) ~ semicolon ~> Declaration.Package |
      push(Declaration.UnnamedPackage)
  }

  def importDeclarations: Rule1[Seq[ImportDeclaration]] = rule {
    zeroOrMore {
      `import` ~ {
        `static` ~ {
          qualifiedIdentifier ~ {
            dot ~ `*` ~> Declaration.StaticLazyImport |
              MATCH ~> Declaration.StaticImport
          }
        } |
          qualifiedIdentifier ~ {
            dot ~ `*` ~> Declaration.LazyImport |
              MATCH ~> Declaration.SingleImport
          }
      } ~ semicolon
    }
  }

  //////////
  // Classes

  def classDeclaration: Rule1[TypeDeclaration] = rule {
    classModifiers ~ `class` ~ identifier ~ optionalTypeParameters ~
      superClass ~ superInterfaces ~ classBody ~> Declaration.Class
  }

  def enumDeclaration: Rule1[TypeDeclaration] = rule {
    classModifiers ~ `enum` ~ identifier ~ superInterfaces ~ enumBody ~> Declaration.Enum
  }

  private def classModifiers: Rule2[Seq[Annotation], Seq[Modifier]] = rule {
    zeroOrMore(annotation | classModifier) ~> { (seq: Seq[Any]) =>
      var as = Vector[Annotation]()
      var ms = Vector[Modifier]()

      for (i <- seq) {
        if (i.isInstanceOf[Annotation]) as = as :+ i.asInstanceOf[Annotation]
        else if (i.isInstanceOf[Modifier]) ms = ms :+ i.asInstanceOf[Modifier]
      }
      as :: ms :: HNil
    }
  }

  private def classModifier: Rule1[Modifier] = rule {
    `public` ~ push(Modifier.Public) | `protected` ~ push(Modifier.Protected) |
      `private` ~ push(Modifier.Private) | `abstract` ~ push(Modifier.Abstract) |
      `static` ~ push(Modifier.Static) | `final` ~ push(Modifier.Final) |
      `strictfp` ~ push(Modifier.Strictfp)
  }

  private def optionalTypeParameters: Rule1[Seq[TemplateParameter]] = rule {
    `<` ~ (oneOrMore(typeParameter) separatedBy comma) ~ `>` | push(Vector())
  }

  private def superClass: Rule1[Option[ClassType]] = rule { optional(`extends` ~ classType) }

  private def superInterfaces: Rule1[Seq[ClassType]] = rule {
    `implements` ~ (zeroOrMore(classType) separatedBy comma) | push(Vector())
  }

  private def classBody: Rule1[Seq[MemberDeclaration]] = rule { `{` ~ classMembers ~ `}` }

  private def classMembers: Rule1[Seq[MemberDeclaration]] = rule {
    zeroOrMore(classMemberDeclaration | instanceInitializer | staticInitializer | constructorDeclaration)
  }

  def classMemberDeclaration: Rule1[MemberDeclaration] = rule {
    fieldDeclaration | methodDeclaration | typeDeclaration
  }

  private def emptyDeclaration: Rule1[TypeDeclaration] = rule { semicolon ~ push(Declaration.Empty) }

  private def fieldDeclaration: Rule1[Declaration.Field] = rule {
    fieldModifiers ~ `type` ~ variableDeclarators ~ semicolon ~> Declaration.Field
  }

  private def variableDeclaration: Rule1[Declaration.LocalVariable] = rule {
    variableModifiers ~ `type` ~ variableDeclarators ~> Declaration.LocalVariable
  }

  private def variableDeclarators: Rule1[Seq[Declarator]] = rule {
    zeroOrMore {
      variableDeclaratorId ~ optional(`=` ~ (expression | arrayInitializer)) ~> {
        (n: String, ds: Seq[Dimension], i: Option[Expression]) =>
          if (ds.size == 0 && i.isDefined) Declarator.InitializedVariable(n, i.get)
          else if (i.isDefined) Declarator.InitializedArray(n, ds, i.get)
          else if (ds.size == 0) Declarator.Variable(n)
          else Declarator.Array(n, ds)
      }
    } separatedBy comma
  }

  private def variableDeclaratorId: Rule2[String, Seq[Dimension]] = rule {
    identifier ~ optionalDims
  }

  private def methodDeclaration: Rule1[Declaration.Method] = rule {
    methodModifiers ~ methodHeader ~ methodBody ~> Declaration.Method
  }

  private def methodBody: Rule1[Statement] = rule { block | emptyStatement }

  private def methodHeader = rule { optionalTypeParameters ~ `type` ~ methodDeclarator ~ thrown }

  private def thrown: Rule1[Seq[ClassType]] = rule {
    `throws` ~ (oneOrMore(classType) separatedBy comma) | push(Vector())
  }

  private def methodDeclarator: Rule1[MethodDeclarator] = rule {
    identifier ~ `(` ~ formalParameters ~ `)` ~ {
      dims ~> Declarator.ArrayMethod |
        MATCH ~> Declarator.Method
    }
  }

  private def formalParameters: Rule1[Seq[FormalParameter]] = rule {
    { receiverParameter | formalParameter } ~ zeroOrMore(comma ~ formalParameter) ~ optional(comma ~ varParameter) ~> {
      (p: FormalParameter, ps: Seq[FormalParameter], v: Option[FormalParameter]) =>
        if (v.isDefined) p +: (ps :+ v.get)
        else p +: ps
    } | varParameter ~> ((p: FormalParameter) => Vector(p)) |
      MATCH ~ push(Vector())
  }

  private def formalParameter: Rule1[FormalParameter] = rule {
    variableModifiers ~ `type` ~ variableDeclaratorId ~> {
      (as: Seq[Annotation], f: Boolean, t: Type, i: String, ds: Seq[Dimension]) =>
        if (ds.size == 0) Parameter.Formal(as, f, t, i)
        else Parameter.Formal(as, f, ArrayType(t, ds), i)
    }
  }

  private def receiverParameter: Rule1[FormalParameter] = rule {
    annotations ~ `type` ~ {
      identifier ~ dot ~> Parameter.NestedReceiver |
        MATCH ~> Parameter.InstanceReceiver
    } ~ `this`
  }

  private def varParameter: Rule1[FormalParameter] = rule {
    variableModifiers ~ `type` ~ `...` ~ variableDeclaratorId ~> {
      (as: Seq[Annotation], f: Boolean, t: Type, i: String, ds: Seq[Dimension]) =>
        if (ds.size == 0) Parameter.VariableArity(as, f, t, i)
        else Parameter.VariableArity(as, f, ArrayType(t, ds), i)
    }
  }

  private def instanceInitializer: Rule1[MemberDeclaration] = rule { block ~> Declaration.InstanceInitializer }

  private def staticInitializer: Rule1[MemberDeclaration] = rule { `static` ~ block ~> Declaration.StaticInitializer }

  private def constructorDeclaration: Rule1[Declaration.Constructor] = rule {
    constuctorModifiers ~ optionalTypeParameters ~ constructorDeclarator ~ thrown ~ constructorBody ~> Declaration.Constructor
  }

  private def constructorDeclarator: Rule1[Declarator.Constructor] = rule {
    identifier ~ `(` ~ formalParameters ~ `)` ~> Declarator.Constructor
  }

  private def constructorBody: Rule1[Block] = rule {
    `{` ~ optional(explicitConstructorInvocation) ~ zeroOrMore(blockStatement) ~ `}` ~> {
      (s: Option[Statement], ss: Seq[Statement]) => if (s.isDefined) Block(s.get +: ss) else Block(ss)
    }
  }

  private def explicitConstructorInvocation: Rule1[Statement] = rule {
    {
      optionalTypeArguments ~ {
        `this` ~ arguments ~> ConstructorInvocation.Alternate |
          `super` ~ arguments ~> ConstructorInvocation.Parent
      } |
        referenceNoMethodAccess ~ dot ~ optionalTypeArguments ~ `super` ~ arguments ~> ConstructorInvocation.IndirectParent |
        primary ~ dot ~ optionalTypeArguments ~ `super` ~ arguments ~> ConstructorInvocation.Intermidiate
    } ~ semicolon
  }
  private def fieldModifiers: Rule2[Seq[Annotation], Seq[Modifier]] = rule {
    push((Vector(), Vector())) ~ zeroOrMore {
      annotation ~> ((t: (Seq[Annotation], Seq[Modifier]), a: Annotation) => (t._1 :+ a, t._2)) |
        fieldModifier ~> ((t: (Seq[Annotation], Seq[Modifier]), m: Modifier) => (t._1, t._2 :+ m))
    } ~> ((t: (Seq[Annotation], Seq[Modifier])) => t._1 :: t._2 :: HNil)
  }

  private def fieldModifier: Rule1[Modifier] = rule {
    `public` ~ push(Modifier.Public) | `protected` ~ push(Modifier.Protected) |
      `private` ~ push(Modifier.Private) | `static` ~ push(Modifier.Static) |
      `final` ~ push(Modifier.Final) | `transient` ~ push(Modifier.Transient) |
      `volatile` ~ push(Modifier.Volatile)
  }
  private def constuctorModifiers: Rule2[Seq[Annotation], Seq[Modifier]] = rule {
    push((Vector(), Vector())) ~ zeroOrMore {
      annotation ~> ((t: (Seq[Annotation], Seq[Modifier]), a: Annotation) => (t._1 :+ a, t._2)) |
        constructorModifier ~> ((t: (Seq[Annotation], Seq[Modifier]), m: Modifier) => (t._1, t._2 :+ m))
    } ~> ((t: (Seq[Annotation], Seq[Modifier])) => t._1 :: t._2 :: HNil)
  }

  def enumBody: Rule1[Seq[MemberDeclaration]] = rule {
    `{` ~ (zeroOrMore(enumConstant) separatedBy comma) ~ optional(comma) ~ optional(semicolon ~ classMembers) ~ `}` ~> {
      (cs: Seq[MemberDeclaration], ms: Option[Seq[MemberDeclaration]]) => cs ++: ms.getOrElse(Vector())
    }
  }

  private def enumConstant: Rule1[MemberDeclaration] = rule {
    annotations ~ identifier ~ optional(arguments) ~ optional(classBody) ~> {
      (as: Seq[Annotation], n: String, args: Option[Seq[Expression]], b: Option[Seq[MemberDeclaration]]) =>
        if (b.isDefined) Declaration.AnonymousEnumConstant(as, n, args.getOrElse(Vector()), b.get)
        else Declaration.EnumConstant(as, n, args.getOrElse(Vector()))
    }
  }

  private def constructorModifier: Rule1[Modifier] = rule {
    `public` ~ push(Modifier.Public) |
      `protected` ~ push(Modifier.Protected) |
      `private` ~ push(Modifier.Private)
  }

  private def methodModifiers: Rule2[Seq[Annotation], Seq[Modifier]] = rule {
    push((Vector(), Vector())) ~ zeroOrMore {
      annotation ~> ((t: (Seq[Annotation], Seq[Modifier]), a: Annotation) => (t._1 :+ a, t._2)) |
        methodModifier ~> ((t: (Seq[Annotation], Seq[Modifier]), m: Modifier) => (t._1, t._2 :+ m))
    } ~> ((t: (Seq[Annotation], Seq[Modifier])) => t._1 :: t._2 :: HNil)
  }

  private def methodModifier: Rule1[Modifier] = rule {
    `public` ~ push(Modifier.Public) | `protected` ~ push(Modifier.Protected) |
      `private` ~ push(Modifier.Private) | `abstract` ~ push(Modifier.Abstract) |
      `static` ~ push(Modifier.Static) | `final` ~ push(Modifier.Final) |
      `synchronized` ~ push(Modifier.Synchronized) | `native` ~ push(Modifier.Native) |
      `strictfp` ~ push(Modifier.Strictfp)
  }

  private def variableModifiers: Rule2[Seq[Annotation], Boolean] = rule {
    annotations ~ optional(`final` ~ push(true)) ~ annotations ~> {
      (as1: Seq[Annotation], f: Option[Boolean], as2: Seq[Annotation]) =>
        (as1 ++ as2) :: f.getOrElse(false) :: HNil
    }
  }

  /////////////
  // Interfaces

  def interfaceDeclaration: Rule1[TypeDeclaration] = rule {
    interfaceModifiers ~ `interface` ~ identifier ~ optionalTypeParameters ~
      extendedInterfaces ~ `{` ~ interfaceBody ~ `}` ~> Declaration.Interface
  }
  def annotationDeclaration: Rule1[TypeDeclaration] = rule {
    interfaceModifiers ~ `@` ~ `interface` ~ identifier ~ `{` ~
      zeroOrMore(annotationMemberDeclaration) ~ `}` ~> Declaration.Annotation
  }

  private def extendedInterfaces: Rule1[Seq[ClassType]] = rule {
    `extends` ~ (oneOrMore(classType) separatedBy comma) | push(Vector())
  }
  private def interfaceBody: Rule1[Seq[MemberDeclaration]] = rule { zeroOrMore(interfaceMemberDeclaration) }

  def interfaceMemberDeclaration: Rule1[MemberDeclaration] = rule {
    constantDeclaration | interfaceMethodDeclaration | typeDeclaration
  }

  private def constantDeclaration: Rule1[MemberDeclaration] = rule {
    constantModifiers ~ `type` ~ variableDeclarators ~ semicolon ~> Declaration.Constant
  }

  private def interfaceMethodDeclaration: Rule1[MemberDeclaration] = rule {
    interfaceMethodModifiers ~ methodHeader ~ methodBody ~> Declaration.InterfaceMethod
  }

  private def annotations: Rule1[Seq[Annotation]] = rule { zeroOrMore(annotation) }

  def annotation: Rule1[Annotation] = rule {
    `@` ~ qualifiedIdentifier ~ {
      `(` ~ (zeroOrMore(elementValuePair) separatedBy `comma`) ~ `)` ~> NormalAnnotation |
        `(` ~ elementValue ~ `)` ~> SingleElementAnnotation |
        MATCH ~> MarkerAnnotation
    }
  }

  private def elementValuePair: Rule1[(String, Expression)] = rule {
    identifier ~ `=` ~ elementValue ~> ((id: String, expr: Expression) => (id, expr))
  }

  private def elementValue: Rule1[Expression] = rule {
    elementValueArrayInitializer | annotation | conditionalExpression
  }

  private def elementValueArrayInitializer: Rule1[Expression] = rule {
    `{` ~ `}` ~ push(ArrayInitializer(Vector())) |
      `{` ~ oneOrMore(elementValue).separatedBy(`comma`) ~ optional(comma) ~ `}` ~> ArrayInitializer
  }

  def annotationMemberDeclaration: Rule1[MemberDeclaration] = rule {
    annotationElementDeclaration | constantDeclaration | typeDeclaration
  }

  private def annotationElementDeclaration: Rule1[MemberDeclaration] = rule {
    annotationElementModifiers ~ `type` ~ identifier ~ `(` ~ `)` ~ optionalDims ~ {
      `default` ~ elementValue ~> { (as: Seq[Annotation], ms: Seq[Modifier], t: Type, n: String, dims: Seq[Dimension], e: Expression) =>
        if (dims.size == 0) Declaration.AnnotationDefaultElement(as, ms, t, n, e)
        else Declaration.AnnotationDefaultElement(as, ms, ArrayType(t, dims), n, e)
      } |
        MATCH ~> { (as: Seq[Annotation], ms: Seq[Modifier], t: Type, n: String, dims: Seq[Dimension]) =>
          if (dims.size == 0) Declaration.AnnotationElement(as, ms, t, n)
          else Declaration.AnnotationElement(as, ms, ArrayType(t, dims), n)
        }
    } ~ semicolon
  }

  private def interfaceModifiers: Rule2[Seq[Annotation], Seq[Modifier]] = rule {
    push((Vector(), Vector())) ~ zeroOrMore {
      annotation ~> ((t: (Seq[Annotation], Seq[Modifier]), a: Annotation) => (t._1 :+ a, t._2)) |
        interfaceModifier ~> ((t: (Seq[Annotation], Seq[Modifier]), m: Modifier) => (t._1, t._2 :+ m))
    } ~> ((t: (Seq[Annotation], Seq[Modifier])) => t._1 :: t._2 :: HNil)
  }

  private def interfaceModifier: Rule1[Modifier] = rule {
    `public` ~ push(Modifier.Public) | `protected` ~ push(Modifier.Protected) |
      `private` ~ push(Modifier.Private) | `abstract` ~ push(Modifier.Abstract) |
      `static` ~ push(Modifier.Static) | `strictfp` ~ push(Modifier.Strictfp)
  }

  private def constantModifiers: Rule2[Seq[Annotation], Seq[Modifier]] = rule {
    push((Vector(), Vector())) ~ zeroOrMore {
      annotation ~> ((t: (Seq[Annotation], Seq[Modifier]), a: Annotation) => (t._1 :+ a, t._2)) |
        constantModifier ~> ((t: (Seq[Annotation], Seq[Modifier]), m: Modifier) => (t._1, t._2 :+ m))
    } ~> ((t: (Seq[Annotation], Seq[Modifier])) => t._1 :: t._2 :: HNil)
  }

  private def constantModifier: Rule1[Modifier] = rule {
    `public` ~ push(Modifier.Public) | `static` ~ push(Modifier.Static) | `final` ~ push(Modifier.Final)
  }

  private def interfaceMethodModifiers: Rule2[Seq[Annotation], Seq[Modifier]] = rule {
    push((Vector(), Vector())) ~ zeroOrMore {
      annotation ~> ((t: (Seq[Annotation], Seq[Modifier]), a: Annotation) => (t._1 :+ a, t._2)) |
        interfaceMethodModifier ~> ((t: (Seq[Annotation], Seq[Modifier]), m: Modifier) => (t._1, t._2 :+ m))
    } ~> ((t: (Seq[Annotation], Seq[Modifier])) => t._1 :: t._2 :: HNil)
  }

  private def interfaceMethodModifier: Rule1[Modifier] = rule {
    `public` ~ push(Modifier.Public) | `static` ~ push(Modifier.Static) |
      `abstract` ~ push(Modifier.Abstract) | `default` ~ push(Modifier.Default) |
      `strictfp` ~ push(Modifier.Strictfp)
  }

  private def annotationElementModifiers: Rule2[Seq[Annotation], Seq[Modifier]] = rule {
    push((Vector(), Vector())) ~ zeroOrMore {
      annotation ~> ((t: (Seq[Annotation], Seq[Modifier]), a: Annotation) => (t._1 :+ a, t._2)) |
        annotationElementModifier ~> ((t: (Seq[Annotation], Seq[Modifier]), m: Modifier) => (t._1, t._2 :+ m))
    } ~> ((t: (Seq[Annotation], Seq[Modifier])) => t._1 :: t._2 :: HNil)
  }

  private def annotationElementModifier: Rule1[Modifier] = rule {
    `public` ~ push(Modifier.Public) | `abstract` ~ push(Modifier.Abstract)
  }
  /////////
  // Arrays

  private def arrayInitializer: Rule1[ArrayInitializer] = rule {
    `{` ~ (zeroOrMore(expression | arrayInitializer) separatedBy comma) ~ optional(comma) ~ `}` ~> ArrayInitializer
  }

  ////////////////////////
  // Blocks and Statements

  private def block: Rule1[Block] = rule { `{` ~ zeroOrMore(blockStatement) ~ `}` ~> Block }

  private def blockStatements: Rule1[Seq[Statement]] = rule { oneOrMore(blockStatement) }

  def blockStatement: Rule1[Statement] = rule {
    variableDeclaration ~ semicolon | classDeclaration | statement
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

  private def emptyStatement: Rule1[Statement] = rule { semicolon ~ push(EmptyStatement) }

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
    (`break` ~ identifier ~> FlowStatement.TargetedBreak | `break` ~ push(FlowStatement.Break)) ~ semicolon
  }

  private def continueStatement: Rule1[Statement] = rule {
    (`continue` ~ identifier ~> FlowStatement.TargetedContinue | `continue` ~ push(FlowStatement.Continue)) ~ semicolon
  }

  private def returnStatement: Rule1[Statement] = rule {
    (`return` ~ expression ~> FlowStatement.Return | `return` ~ push(FlowStatement.EmptyReturn)) ~ semicolon
  }

  private def throwStatement: Rule1[FlowStatement] = rule {
    `throw` ~ expression ~ semicolon ~> FlowStatement.Throw
  }

  private def conditionalStatement: Rule1[ConditionalStatement] = rule {
    `if` ~ `(` ~ expression ~ `)` ~ statement ~ {
      `else` ~ statement ~> Conditional.IfThenElse |
        MATCH ~> Conditional.If
    }
  }

  private def tryStatement: Rule1[TryStatement] = rule {
    `try` ~ {
      resources ~ block ~ optional(catches) ~ optional(finallyBlock) ~> {
        (rs: Seq[Declaration.LocalVariable], tb: Block, cs: Option[Seq[CatchClause]], fb: Option[Block]) =>
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
      zeroOrMore(oneOrMore(switchLabel) ~ oneOrMore(blockStatement) ~> Switch.Cases) ~
        zeroOrMore(switchLabel) ~> ((es: Seq[Expression]) => if (es.size == 0) None else Some(Switch.EmptyCases(es)))
    } ~ `}` ~> {
      (e: Expression, cs1: Seq[SwitchRule], c2: Option[SwitchRule]) =>
        if (c2.isDefined) SwitchStatement(e, cs1 :+ c2.get) else SwitchStatement(e, cs1)
    }
  }

  private def switchLabel: Rule1[Expression] = rule {
    (`case` ~ (expression | reference) | `default` ~ push(Switch.Default)) ~ colon
  }

  private def synchronizedBlock: Rule1[SynchronizedBlock] = rule {
    `synchronized` ~ optional(`(` ~ expression ~ `)`) ~ block ~> SynchronizedBlock
  }

  private def catches: Rule1[Seq[CatchClause]] = rule {
    zeroOrMore { `catch` ~ `(` ~ catchFormalParameter ~ `)` ~ block ~> CatchClause }
  }

  private def finallyBlock: Rule1[Block] = rule { `finally` ~ block }

  private def catchFormalParameter: Rule1[Declaration.LocalVariable] = rule {
    variableModifiers ~ ((zeroOrMore(classType) separatedBy `|`) ~> (
      (seq: Seq[ClassType]) => if (seq.size == 1) seq(0) else ChildOfAny(seq))) ~ identifier ~> {
        (as: Seq[Annotation], _: Boolean, t: Type, n: String) =>
          Declaration.LocalVariable(as, true, t, Vector(Declarator.Variable(n)))
      }
  }

  private def resources: Rule1[Seq[Declaration.LocalVariable]] = rule {
    `(` ~ (oneOrMore(resource) separatedBy semicolon) ~ optional(semicolon) ~ `)`
  }

  private def resource: Rule1[Declaration.LocalVariable] = rule {
    variableModifiers ~ classType ~ identifier ~ `=` ~ expression ~> {
      (as: Seq[Annotation], _: Boolean, t: Type, n: String, e: Expression) =>
        Declaration.LocalVariable(as, true, t, Vector(Declarator.InitializedVariable(n, e)))
    }
  }

  private def doWhileLoop: Rule1[Loop] = rule {
    `do` ~ statement ~ `while` ~ `(` ~ expression ~ `)` ~ semicolon ~> Loop.DoWhile
  }

  private def whileLoop: Rule1[Loop.While] = rule {
    `while` ~ `(` ~ expression ~ `)` ~ statement ~> Loop.While
  }

  private def forLoop: Rule1[Loop] = rule {
    `for` ~ `(` ~ variableDeclaration ~ colon ~ expression ~ `)` ~ statement ~> Loop.Iteration |
      `for` ~ `(` ~ forInit ~ semicolon ~ optional(expression) ~ semicolon ~ forUpdate ~ `)` ~ statement ~> Loop.For
  }

  private def forInit: Rule1[Seq[Statement]] = rule {
    (oneOrMore(discardableExpressionStatement) separatedBy comma) |
      variableDeclaration ~> ((d: Declaration.LocalVariable) => Vector(d)) |
      push(Vector())
  }

  private def forUpdate: Rule1[Seq[Statement]] = rule {
    zeroOrMore(discardableExpressionStatement) separatedBy comma
  }

  //////////////
  // Expressions
  def expression: Rule1[Expression] = rule {
    lambda | assignment | conditionalExpression
  }

  private def lambda: Rule1[Lambda] = rule {
    lambdaParameters ~ `->` ~ lambdaBody ~> Lambda
  }

  private def lambdaBody: Rule1[Statement] = rule {
    expression ~> FlowStatement.ImplicitReturn | block
  }

  private def lambdaParameters: Rule1[Seq[Parameter]] = rule {
    identifier ~> ((e: String) => Vector(Parameter.Inferred(e))) |
      `(` ~ (oneOrMore(identifier ~> Parameter.Inferred) separatedBy comma) ~ `)` |
      `(` ~ optionalFormalParameters ~ `)`
  }

  private def optionalFormalParameters: Rule1[Seq[FormalParameter]] = rule {
    formalParameters | push(Vector())
  }

  private def referenceNoMethodAccess: Rule1[Select] = rule {
    identifier ~ zeroOrMore(dot ~ identifier ~ !(`(`)) ~> ((i: String, is: Seq[String]) => Select(i +: is))
  }

  private def primary: Rule1[DiscardableExpression] = rule {
    primaryBase ~ zeroOrMore {
      dot ~ {
        optionalTypeArguments ~ identifier ~ arguments ~> {
          (e: Expression, t: Seq[TemplateArgument], i: String, as: Seq[Expression]) =>
            QualifiedMethodInvocation(e, t, i, as)
        } | identifier ~> ((e: Expression, i: String) => FieldAccess(e, i)) |
          `new` ~ optionalTypeArguments ~ classTypeWithDaiamond ~ arguments ~ optional(classBody) ~> {
            (e: Expression, ts: Seq[TemplateArgument], t: Type, args: Seq[Expression], b: Option[Seq[MemberDeclaration]]) =>
              if (b.isDefined) Instantiation.NestedAnonymousObject(e, ts, t, args, b.get)
              else Instantiation.NestedObject(e, ts, t, args)
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
                if (b.isDefined) Instantiation.NestedAnonymousObject(e, ts, t, args, b.get)
                else Instantiation.NestedObject(e, ts, t, args)
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

  private def arguments: Rule1[Seq[Expression]] = rule { `(` ~ (zeroOrMore(expression) separatedBy comma) ~ `)` }

  private def classCreator: Rule1[ObjectInstantiation] = rule {
    optionalTypeArguments ~ classTypeWithDaiamond ~ arguments ~ {
      classBody ~> Instantiation.AnonymousObject |
        MATCH ~> Instantiation.Object
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
    `type` ~ dimExprs ~> { (t: Type, ss: Seq[Dimension]) => Instantiation.Array(t, ss) } |
      arrayType ~ arrayInitializer ~> Instantiation.InitializedArray
  }

  private def dimExprs: Rule1[Seq[Dimension]] = rule {
    oneOrMore(annotations ~ `[` ~ expression ~ `]` ~> InitializedDimension) ~
      zeroOrMore(annotations ~ `[` ~ `]` ~> AbstractDimension) ~> {
        (d1: Seq[InitializedDimension], d2: Seq[AbstractDimension]) => d1 ++: d2
      }
  }

  private def assignmentExpression: Rule1[Expression] = rule { conditionalExpression | assignment }

  private def assignment: Rule1[Assignment] = rule {
    (primary | reference) ~ {
      `=` ~ expression ~> Binding | assignmentOps ~ expression ~> AugmentedBinding
    }
  }

  private def conditionalExpression: Rule1[Expression] = rule {
    or ~ optional(`?` ~ expression ~ colon ~ conditionalExpression ~> TernaryConditional)
  }

  private def or: Rule1[Expression] = rule { binaryOperation(orOp, and) }
  private val and = () => rule { binaryOperation(andOp, binOr) }
  private val binOr = () => rule { binaryOperation(binOrOp, xor) }
  private val xor = () => rule { binaryOperation(xorOp, binAnd) }
  private val binAnd = () => rule { binaryOperation(binAndOp, equality) }
  private val equality = () => rule { binaryOperation(eqOps, () => relational) }

  private def relational: Rule1[Expression] = rule {
    (shift()) ~> ((expr: Expression) => (Vector[BinaryOperator](), Vector(expr))) ~ {
      zeroOrMore {
        ((relOps()) ~ (shift()) | `instanceof` ~ push(BinaryOperator.instanceof) ~ referenceType) ~> (
          (acc: (Vector[BinaryOperator], Vector[Expression]), op: BinaryOperator, expr: Expression) =>
            (acc._1 :+ op, acc._2 :+ expr))
      } ~> ((acc: (Vector[BinaryOperator], Vector[Expression])) =>
        if (acc._2.size == 1) acc._2(0) else BinaryOperationChain(acc._1, acc._2))
    }
  }

  private val shift = () => rule { binaryOperation(shiftOps, additive) }
  private val additive = () => rule { binaryOperation(addOps, multiplicative) }
  private val multiplicative = () => rule { binaryOperation(mulOps, () => unary) }

  private def unary: Rule1[Expression] = rule {
    `++` ~ unary ~> PreIncrementation | `--` ~ unary ~> PreDecrementation |
      oneOrMore(unaryOps) ~ unary ~> (UnaryOperationChain((_: Seq[UnaryOperator]), (_: Expression))) |
      cast | postfix
  }

  private def postfix: Rule1[Expression] = rule {
    (primary | reference) ~ optional {
      `++` ~> PostIncrementation | `--` ~> PostDecrementation
    }
  }

  private def reference: Rule1[Expression] = rule {
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

  private def binaryOperation(operator: () => Rule1[BinaryOperator], next: () => Rule1[Expression]): Rule1[Expression] = rule {
    next() ~> ((expr: Expression) => (Vector[BinaryOperator](), Vector(expr))) ~ {
      zeroOrMore {
        operator() ~ next() ~> ((acc: (Vector[BinaryOperator], Vector[Expression]), op: BinaryOperator, expr: Expression) =>
          (acc._1 :+ op, acc._2 :+ expr))
      }
    } ~> ((acc: (Vector[BinaryOperator], Vector[Expression])) =>
      if (acc._2.size == 1) acc._2(0) else BinaryOperationChain(acc._1, acc._2))
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