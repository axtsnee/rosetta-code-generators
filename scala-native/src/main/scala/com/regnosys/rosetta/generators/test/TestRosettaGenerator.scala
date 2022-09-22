package com.regnosys.rosetta.generators.test

import java.io.{File, FileInputStream}
import java.nio.file.{Files, Paths}
import java.nio.charset.Charset
import scala.jdk.CollectionConverters._
import com.google.common.io.Resources
import com.google.inject.Injector
import com.regnosys.rosetta.generator.external.AbstractExternalGenerator
import com.regnosys.rosetta.generator.scalanative.ScalaNativeGenerator
import com.regnosys.rosetta.rosetta.RosettaModel
import org.apache.commons.io.IOUtils
import org.eclipse.emf.common.util.URI
import org.eclipse.xtext.testing.util.ParseHelper

object TestRosettaGenerator {
  def main(args: Array[String]): Unit = {
    require(args.nonEmpty)
    runGenerator(ScalaNativeGenerator, args.head, args.tail.headOption)
  }

  def runGenerator(
      generator: AbstractExternalGenerator,
      rosettaDir: String,
      outputDir: Option[String]
  ): Unit = {
    val injector: Injector = new RosettaServerSetup(new ExternalGeneratorProvider(generator)).createInjectorDoSetup()
    val parseHelper = injector.getInstance(classOf[ParseHelper[RosettaModel]])
    val basicModel = parseHelper.parse(basicTypes)
    val resourceSet = basicModel.eResource().getResourceSet
    val _ = parseHelper.parse(annotations, resourceSet)
    val rosettaFiles = getRecursiveListOfRosettaFiles(new File(rosettaDir))
    val models = rosettaFiles.map(f => parseHelper.parse(new FileInputStream(f), toUri(f), null, resourceSet))
    val result = generator.afterGenerate(models.asJava).asScala
    result.foreach {
      case (filename, contents) =>
        val p = Paths.get(outputDir.getOrElse("."), filename)
        Files.createDirectories(p.getParent)
        Files.writeString(p, contents)
    }
  }

  private def annotations: String =
    IOUtils.toString(Resources.getResource("model/annotations.rosetta"), Charset.defaultCharset())

  private def basicTypes: String =
    IOUtils.toString(Resources.getResource("model/basictypes.rosetta"), Charset.defaultCharset())

  private def toUri(f: File): URI = URI.createFileURI(f.getAbsolutePath)

  private def getRecursiveListOfRosettaFiles(dir: File): Vector[File] = {
    val topLevel = dir.listFiles.toVector
    val all = topLevel ++ topLevel.filter(_.isDirectory).flatMap(getRecursiveListOfRosettaFiles)
    all.filter(_.getName.endsWith(".rosetta"))
  }
}
