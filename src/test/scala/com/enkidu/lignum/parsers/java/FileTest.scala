package com.enkidu.lignum.parsers.java

import com.enkidu.lignum.parsers.types._
import com.enkidu.lignum.parsers.expressions._
import com.enkidu.lignum.parsers.statements._
import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.JavaParser
import java.io.File

class FileTest extends ParserTest {
  def parse(string: String): Declaration.CompilationUnit = {
    implicit val parser = new JavaParser(string)
    get(parser.compilationUnit.run())
  }

  val files = Seq("D:\\_tmp\\").map(new File(_)).filter(_.exists())
  val enabled = false

  if (enabled)
    "Parser should parse .java files in" - {
      files.flatMap { f =>
        s"$f" - {
          traverseDirectory(f)(isJavaSource).foreach { t =>
            //          (t._1.toString()) in { noException should be thrownBy (parse(t._2)) }
            (t._1.toString()) in { (try { parse(t._2) } catch { case _: Throwable => parse(t._2) shouldBe 1 }) }
          }
        }
      }
    }

}