package com.enkidu.lignum.parsers.java.v8

import org.parboiled2.{ CharPredicate, Rule1 }
import com.enkidu.lignum.parsers.commons.AbstractParser
import org.parboiled2.ParseError
import org.parboiled2.ParserInput

abstract class JavaIdentifierParser extends AbstractParser  {
  def identifier = rule {
    !keyword ~ capture(identifierStart ~ zeroOrMore(identifierChar)) ~ whitespace
  }

  def qualifiedIdentifier: Rule1[Seq[String]] = rule { oneOrMore(identifier) separatedBy `dot` }

  protected def keyword = rule {
    ("abstract" | "continue" | "for" | "new" | "switch" | "assert" | "default" | "if" | "package" | "synchronized" |
      "boolean" | "double" | "do" | "goto" | "private" | "this" | "break" | "implements" | "protected" | "throws" |
      "throw" | "byte" | "else" | "imports" | "public" | "case" | "enum" | "instanceof" | "return" | "transient" |
      "catch" | "extends" | "interface" | "int" | "short" | "try" | "char" | "finally" | "final" | "static" | "void" |
      "class" | "long" | "strictfp" | "volatile" | "const" | "float" | "native" | "super" | "while" | "long") ~ !(identifierChar) ~ whitespace
  }
}
