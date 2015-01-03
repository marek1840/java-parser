package com.enkidu.lignum.parsers;

import _root_.java.io.File
import _root_.java.nio.charset.CodingErrorAction

import scala.collection.immutable.Stream.consWrapper
import scala.io.Codec
import scala.io.Source
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.parboiled2.ParseError
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.prop.Checkers
import org.scalatest.prop.PropertyChecks

abstract class ParserTest extends FreeSpec with PropertyChecks with Matchers with Checkers with ParserImplicits {

  protected def get[A](a: Try[A])(implicit parser: AbstractParser): A = a match {
    case Success(res)               => res
    case Failure(error: ParseError) => throw new Exception(parser.formatError(error, showTraces = true))
    case r                          => r.get
  }

  protected def traverseDirectory(file: File)(predicate: File => Boolean)(implicit codec: Codec): Stream[(File, String)] = {
    def recursiveListFiles(file: File): Stream[File] = {
      val these = file.listFiles.toStream
      these #::: these.filter(_.isDirectory).flatMap(recursiveListFiles)
    }
    codec.onMalformedInput(CodingErrorAction.IGNORE)
    codec.onUnmappableCharacter(CodingErrorAction.IGNORE)
    recursiveListFiles(file).filter(_.isFile).filter(predicate).map { f => (f, Source.fromFile(f)(codec).mkString) }
  }

  protected def isJavaSource(file: File): Boolean = file.getName().endsWith(".java")
}
