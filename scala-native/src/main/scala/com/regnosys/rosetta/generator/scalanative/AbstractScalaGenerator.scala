package com.regnosys.rosetta.generator.scalanative

import AbstractScalaGenerator.deriveAnyPackageName
import com.regnosys.rosetta.rosetta.{RosettaEnumeration, RosettaMetaType, RosettaNamed, RosettaRootElement}
import com.regnosys.rosetta.rosetta.simple.{Data, Function}

abstract class AbstractScalaGenerator[T <: RosettaRootElement with RosettaNamed](
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
      val prettyImports =
        if (importStatements.isEmpty) "" else importStatements + "\n\n"
      val code = es.map(translate).mkString("\n")
      s"""package $pkg {
         |$prettyImports$code
         |}
         |""".stripMargin
  }

  private def generateDeps(ts: Iterable[T]): Set[String] = {
    ts.foldLeft(Set.empty[String]) {
      case (acc, e) =>
        acc ++ dependencies(e)
          .filterNot(d => deriveAnyPackageName(d) == deriveAnyPackageName(e)) // same pkg - no need to import
          .map(dep => s"${deriveAnyPackageName(dep)}.${dep.getName}")
    }
  }

  private def generateFileHeader(version: String): String = {
    s"""/**
       |  * This file is auto-generated from the ISDA Common Domain Model, do not edit.
       |  * Version: $version
       |  */
       |import java.time._
       |
       |import scala.jdk.CollectionConverters._
       |import scala.math.BigDecimal
       |import scala.util.{Try, Success, Failure}
       |
       |import com.google.inject.Injector
       |import com.regnosys.rosetta.common.hashing.ReferenceResolverProcessStep
       |import com.regnosys.rosetta.common.validation.RosettaTypeValidator
       |import com.regnosys.rosetta.common.validation.ValidationReport
       |import org.isda.cdm.processor.CdmReferenceConfig
       |import ${AbstractScalaGenerator.basePackage}._
       |import ${AbstractScalaGenerator.basePackage}.Utils._
       |
       |""".stripMargin
  }
}
object AbstractScalaGenerator {
  val basePackage = "org.isda.cdm.scalanative"

  def deriveParentPackage(e: RosettaRootElement): String = {
    val ns = e.getModel.getName
    val subpkg = ns.substring(ns.indexOf('.') + 1)
    s"$basePackage.$subpkg"
  }

  def deriveAnyPackageName(r: RosettaRootElement): String = r match {
    case e: RosettaEnumeration => ScalaEnumerationGenerator.derivePackageName(e)
    case e: Function => ScalaFunctionGenerator.derivePackageName(e)
    case e: RosettaMetaType => ScalaMetaTypeGenerator.derivePackageName(e)
    case e: Data => ScalaTypeGenerator.derivePackageName(e)
  }
}
