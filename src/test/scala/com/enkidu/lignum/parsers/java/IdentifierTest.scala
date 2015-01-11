package com.enkidu.lignum.parsers.java

import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.java.v8.JavaCompilationUnitParser

class IdentifierTest extends ParserTest {
  def parse(string: String): Seq[String] = {
    implicit val parser = new JavaCompilationUnitParser(string)
    get(parser.qualifiedIdentifier.run())
  }

  "Identifier parser should" - {
    "parse" - {
      "a" in (parse("a") shouldBe Vector("a"))
      "a10" in (parse("a10") shouldBe Vector("a10"))
      "$a" in (parse("$a") shouldBe Vector("$a"))
      "$10" in (parse("$10") shouldBe Vector("$10"))
      "_aaa" in (parse("_aaa") shouldBe Vector("_aaa"))
      "qualified identifier" - {
        "Config$Keys.ANONYMOUS_USER_ID" in { parse("Config$Keys.ANONYMOUS_USER_ID") shouldBe Vector("Config$Keys", "ANONYMOUS_USER_ID") }
        "a.b.c.d" in (parse("a.b.c.d") shouldBe Vector("a", "b", "c", "d"))
        "a . b  . c\t.d     " in (parse("a . b  . c\t.d     ") shouldBe Vector("a", "b", "c", "d"))
      }
      "not java keywords" - {
        "do_id" in { parse("do_id") shouldBe Vector("do_id") }
        "do$id" in { parse("do$id") shouldBe Vector("do$id") }
        "longish" in (parse("longish") shouldBe Vector("longish"))
        "trueHash" in { parse("trueHash") shouldBe Vector("trueHash") }
      }
    }
    "not parse" - {
      "1asf" in (a[Exception] should be thrownBy parse("1asf"))
      "34531" in (a[Exception] should be thrownBy parse("34531"))
      "" in (a[Exception] should be thrownBy parse(""))
      ".b.c.d" in (a[Exception] should be thrownBy parse(".b.c.d"))
      "keywords" - {
        "abstract" in (a[Exception] should be thrownBy parse("abstract"))
        "continue" in (a[Exception] should be thrownBy parse("continue"))
        "for" in (a[Exception] should be thrownBy parse("for"))
        "new" in (a[Exception] should be thrownBy parse("new"))
        "switch" in (a[Exception] should be thrownBy parse("switch"))
        "assert" in (a[Exception] should be thrownBy parse("assert"))
        "default" in (a[Exception] should be thrownBy parse("default"))
        "if" in (a[Exception] should be thrownBy parse("if"))
        "package" in (a[Exception] should be thrownBy parse("package"))
        "synchronized" in (a[Exception] should be thrownBy parse("synchronized"))
        "boolean" in (a[Exception] should be thrownBy parse("boolean"))
        "do" in (a[Exception] should be thrownBy parse("do"))
        "goto" in (a[Exception] should be thrownBy parse("goto"))
        "private" in (a[Exception] should be thrownBy parse("private"))
        "this" in (a[Exception] should be thrownBy parse("this"))
        "break" in (a[Exception] should be thrownBy parse("break"))
        "double" in (a[Exception] should be thrownBy parse("double"))
        "implements" in (a[Exception] should be thrownBy parse("implements"))
        "protected" in (a[Exception] should be thrownBy parse("protected"))
        "throw" in (a[Exception] should be thrownBy parse("throw"))
        "byte" in (a[Exception] should be thrownBy parse("byte"))
        "else" in (a[Exception] should be thrownBy parse("else"))
        "imports" in (a[Exception] should be thrownBy parse("imports"))
        "public" in (a[Exception] should be thrownBy parse("public"))
        "throws" in (a[Exception] should be thrownBy parse("throws"))
        "case" in (a[Exception] should be thrownBy parse("case"))
        "enum" in (a[Exception] should be thrownBy parse("enum"))
        "instanceof" in (a[Exception] should be thrownBy parse("instanceof"))
        "return" in (a[Exception] should be thrownBy parse("return"))
        "transient" in (a[Exception] should be thrownBy parse("transient"))
        "catch" in (a[Exception] should be thrownBy parse("catch"))
        "extends" in (a[Exception] should be thrownBy parse("extends"))
        "int" in (a[Exception] should be thrownBy parse("int"))
        "short" in (a[Exception] should be thrownBy parse("short"))
        "try" in (a[Exception] should be thrownBy parse("try"))
        "char" in (a[Exception] should be thrownBy parse("char"))
        "final" in (a[Exception] should be thrownBy parse("final"))
        "interface" in (a[Exception] should be thrownBy parse("interface"))
        "static" in (a[Exception] should be thrownBy parse("static"))
        "void" in (a[Exception] should be thrownBy parse("void"))
        "class" in (a[Exception] should be thrownBy parse("class"))
        "finally" in (a[Exception] should be thrownBy parse("finally"))
        "long" in (a[Exception] should be thrownBy parse("long"))
        "strictfp" in (a[Exception] should be thrownBy parse("strictfp"))
        "volatile" in (a[Exception] should be thrownBy parse("volatile"))
        "const" in (a[Exception] should be thrownBy parse("const"))
        "float" in (a[Exception] should be thrownBy parse("float"))
        "native" in (a[Exception] should be thrownBy parse("native"))
        "super" in (a[Exception] should be thrownBy parse("super"))
        "while" in (a[Exception] should be thrownBy parse("while"))
      }
    }
  }
}