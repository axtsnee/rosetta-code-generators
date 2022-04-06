package com.regnosys.rosetta.generator.scalawrapper

import java.nio.file.{Files, Path}
import java.util.Collections
import scala.jdk.CollectionConverters._
import scala.util.Using
import com.regnosys.rosetta.generator.scalawrapper.JavaWrapperGenerator.afterGenerate
import com.regnosys.rosetta.generators.test.TestHelper

object ParserHelper {
  private lazy val testHelper = new TestHelper(JavaWrapperGenerator)
  private implicit val autodelete: Using.Releasable[Path] = p => Files.delete(p)

  val preamble =
    """namespace test:
      |version "test"
      |
      |""".stripMargin

  def parse(snippets: String*): Map[String, CharSequence] = {
    val rosetta = preamble + snippets.mkString("\n")
    Using.resource(Files.createTempFile("cdm-temp-", ".rosetta")) { p =>
      Files.writeString(p, rosetta)
      Option(testHelper.parse(p.toUri.toURL)) match {
        case None =>
          Map.empty.withDefault(k => s"Rosetta parser failed, $k not generated.")
        case Some(model) =>
          afterGenerate(Collections.singleton(model)).asScala.toMap
      }
    }
  }
}
