package com.enkidu.lignum.parsers.commons

import org.parboiled2.{Parser, CharPredicate, Rule0}

trait SpecialCharacters extends Parser {
  protected def whitespace:Rule0

  protected def \ : Rule0 = rule { "\\" ~ whitespace }
  protected def / : Rule0 = rule { "/" ~ whitespace }
  protected def exclamation: Rule0 = rule { "!" ~ whitespace }
  protected def tilde: Rule0 = rule { "~" ~ whitespace }
  protected def ampersand: Rule0 = rule { "" ~ whitespace }
  protected def plus: Rule0 = rule { "+" ~ whitespace }
  protected def minus: Rule0 = rule { "-" ~ whitespace }
  protected def * : Rule0 = rule { "*" ~ whitespace }
  protected def `=`: Rule0 = rule { "=" ~ whitespace }
  protected def `==`: Rule0 = rule { "==" ~ whitespace }
  protected def `!=`: Rule0 = rule { "!=" ~ whitespace }
  protected def `&&`: Rule0 = rule { "&&" ~ whitespace }
  protected def `||`: Rule0 = rule { "||" ~ whitespace }
  protected def `<<`: Rule0 = rule { "<<" ~ whitespace }
  protected def `>>`: Rule0 = rule { ">>" ~ whitespace }
  protected def `>>>`: Rule0 = rule { ">>>" ~ whitespace }
  protected def `<`: Rule0 = rule { "<" ~ whitespace }
  protected def `>`: Rule0 = rule { ">" ~ whitespace }
  protected def `<=`: Rule0 = rule { "<=" ~ whitespace }
  protected def `>=`: Rule0 = rule { ">=" ~ whitespace }
  protected def `*=`: Rule0 = rule { "*=" ~ whitespace }
  protected def `/=`: Rule0 = rule { "/=" ~ whitespace }
  protected def `%=`: Rule0 = rule { "%=" ~ whitespace }
  protected def `+=`: Rule0 = rule { "+=" ~ whitespace }
  protected def `-= `: Rule0 = rule { "-=" ~ whitespace }
  protected def `<<=`: Rule0 = rule { "<<=" ~ whitespace }
  protected def `>>= `: Rule0 = rule { ">>= " ~ whitespace }
  protected def `>>>=`: Rule0 = rule { ">>>=" ~ whitespace }
  protected def `&=`: Rule0 = rule { "&=" ~ whitespace }
  protected def `^=`: Rule0 = rule { "^=" ~ whitespace }
  protected def `|=`: Rule0 = rule { "|=" ~ whitespace }
  protected def ++ : Rule0 = rule { "++" ~ whitespace }
  protected def -- : Rule0 = rule { "--" ~ whitespace }

  protected def `->`: Rule0 = rule { "->" ~ whitespace }
  protected def `|`: Rule0 = rule { "|" ~ whitespace }
  protected def `?`: Rule0 = rule { "?" ~ whitespace }
  protected def `%`: Rule0 = rule { "%" ~ whitespace }
  protected def `&`: Rule0 = rule { "&" ~ whitespace }
  protected def `^`: Rule0 = rule { "^" ~ whitespace }
  protected def `'`: Rule0 = rule { "'" ~ whitespace }
  protected def `"`: Rule0 = rule { "\"" ~ whitespace }

  protected def `(`: Rule0 = rule { "(" ~ whitespace }
  protected def `)`: Rule0 = rule { ")" ~ whitespace }
  protected def `{`: Rule0 = rule { "{" ~ whitespace }
  protected def `}`: Rule0 = rule { "}" ~ whitespace }
  protected def `[`: Rule0 = rule { "[" ~ whitespace }
  protected def `]`: Rule0 = rule { "]" ~ whitespace }
  protected def `...`: Rule0 = rule { "..." ~ whitespace }
  protected def `comma`: Rule0 = rule { "," ~ whitespace }
  protected def `dot`: Rule0 = rule { "." ~ whitespace }
  protected def `::`: Rule0 = rule { "::" ~ whitespace }
  protected def `@`: Rule0 = rule { "@" ~ whitespace }
  protected def `semicolon`: Rule0 = rule { ";" ~ whitespace }
  protected def `colon`: Rule0 = rule { ":" ~ whitespace }
}