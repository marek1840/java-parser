package com.enkidu.lignum.parsers.java

import java.io.File

import com.enkidu.lignum.parsers.ParserTest
import com.enkidu.lignum.parsers.ast.statement.declaration.CompilationUnitDeclaration
import com.enkidu.lignum.parsers.java.v8.JavaCompilationUnitParser

class FileTest extends ParserTest {
  def parse(string: String): CompilationUnitDeclaration = {
    implicit val parser = new JavaCompilationUnitParser(string)
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
            t._1.toString in {
              try parse(t._2) catch {
                case _: Throwable => parse(t._2) shouldBe 1
              }
            }
          }
        }
      }
    }

}