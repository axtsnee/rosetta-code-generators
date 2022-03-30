package com.regnosys.rosetta.generator.scalawrapper

import com.regnosys.rosetta.generator.scalawrapper.AbstractCdmGenerator.deriveAnyPackageName

import scala.jdk.CollectionConverters._
import com.regnosys.rosetta.rosetta.{RosettaEnumeration, RosettaMetaType, RosettaModel, RosettaNamed, RosettaRootElement}
import com.regnosys.rosetta.rosetta.simple.{Data, Function}

abstract class AbstractCdmGenerator[T <: RosettaRootElement with RosettaNamed](
    elements: Iterable[T],
    nsToPkgs: Map[String, Iterable[String]]
) {
  def dependencies: T => Iterable[RosettaRootElement with RosettaNamed]

  def derivePackageName: T => String

  def translate: T => String

  lazy val generateFile: String = {
    val generatedCode =
      elements.groupBy(derivePackageName)
        .map(generatePackage.tupled)
        .mkString("\n")
    elements
      .headOption
      .map(e => generateFileHeader(e.getModel.getVersion) + generatedCode)
      .getOrElse("")
  }

  private val generatePackage: (String, Iterable[T]) => String = {
    case (pkg, es) =>
      val importStatements =
        generateDeps(es)
          .map(i => s"  import $i")
          .mkString("\n")
      val code = es.map(translate).mkString
      s"""package $pkg {
         |$importStatements
         |$code
         |}
         |""".stripMargin
  }

  private def generateDeps(ts: Iterable[T]): Set[String] = {
    val pkgDeps =
      ts.map(_.getModel)
        .toSet
        .flatMap((m: RosettaModel) => m.getImports.asScala)
        .map(_.getImportedNamespace.replace(".*", ""))
        .flatMap(nsToPkgs.withDefaultValue(Nil)) //FIXME
    pkgDeps.map(_ + "._") ++ ts.foldLeft(Set.empty[String]) {
      case (acc, e) =>
        val specificImports =
          dependencies(e)
            .filterNot(_.getModel == e.getModel) // same pkg - no need to import
            .map(ancestor => (deriveAnyPackageName(ancestor), ancestor.getName))
            .filterNot(tuple => pkgDeps.contains(tuple._1)) // already covered
            .map(tuple => tuple._1 + '.' + tuple._2)
        acc ++ specificImports
    }
  }

  private def generateFileHeader(version: String): String = {
    s"""/**
       |  * This file is auto-generated from the ISDA Common Domain Model, do not edit.
       |  * Version: $version
       |  */
       |import java.time._
       |import scala.math.BigDecimal
       |
       |""".stripMargin
  }
}
object AbstractCdmGenerator {
  val basePkg: String = "org.isda.scala"

  def deriveAnyPackageName(r: RosettaRootElement): String = r match {
    case e: RosettaEnumeration => CdmEnumerationGenerator.derivePackageName(e)
    case e: Function => CdmFunctionGenerator.derivePackageName(e)
    case e: RosettaMetaType => CdmMetaTypeGenerator.derivePackageName(e)
    case e: Data => CdmTypeGenerator.derivePackageName(e)
  }
}
