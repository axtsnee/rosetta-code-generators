package com.regnosys.rosetta.generator.scalawrapper

import com.regnosys.rosetta.generator.scalawrapper.AbstractCdmGenerator.deriveAnyPackageName

import com.regnosys.rosetta.rosetta.{RosettaEnumeration, RosettaMetaType, RosettaNamed, RosettaRootElement}
import com.regnosys.rosetta.rosetta.simple.{Data, Function}

abstract class AbstractCdmGenerator[T <: RosettaRootElement with RosettaNamed](
    elements: Iterable[T]
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
    ts.foldLeft(Set.empty[String]) {
      case (acc, e) =>
        acc ++ dependencies(e)
          .filterNot(_.getModel == e.getModel) // same pkg - no need to import
          .map(dep => s"${deriveAnyPackageName(dep)}.${dep.getName}")
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
