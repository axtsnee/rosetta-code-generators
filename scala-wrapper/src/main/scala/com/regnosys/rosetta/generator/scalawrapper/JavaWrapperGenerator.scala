package com.regnosys.rosetta.generator.scalawrapper

import java.util.{Collections, Collection => JCollection, List => JList, Map => JMap}

import scala.jdk.CollectionConverters._

import com.regnosys.rosetta.generator.external.AbstractExternalGenerator
import com.regnosys.rosetta.generator.java.RosettaJavaPackages
import com.regnosys.rosetta.rosetta.{RosettaEnumeration, RosettaMetaType, RosettaModel, RosettaRootElement, RosettaType}
import com.regnosys.rosetta.rosetta.simple.{Data, Function}

object JavaWrapperGenerator extends AbstractExternalGenerator("ScalaWrapper") {
  val typesFilename = "CdmTypes.scala"
  val metaTypesFilename = "CdmMetaTypes.scala"
  val enumsFilename = "CdmEnums.scala"
  val functionsFilename = "CdmFunctions.scala"

  override def generate(packages: RosettaJavaPackages, elements: JList[RosettaRootElement], version: String): JMap[String, _ <: CharSequence] = Collections.emptyMap()

  override def afterGenerate(models: JCollection[_ <: RosettaModel]): JMap[String, _ <: CharSequence] = {
    val rootElements = models.asScala.flatMap(_.getElements.asScala)
    val enclosingElements = extractEnclosingElements(rootElements)
    val extendingEnums = extractExtendingEnums(rootElements)
    val base = rootElements.groupMapReduce(elementToFileName)(translate(enclosingElements, extendingEnums))(_ ++ _)
    val version = models.asScala.head.getVersion
    val result = base
      .updatedWith(typesFilename)(_.map(generateFileHeaderForTypes(version) + _))
      .updatedWith(metaTypesFilename)(_.map(generateFileHeaderForMetatypes(version) + _))
      .updatedWith(enumsFilename)(_.map(generateFileHeaderForEnums(version) + _))
      .updatedWith(functionsFilename)(_.map(generateFileHeaderForFunctions(version) + _))
    result.toMap.asJava
  }

  private val elementToFileName: RosettaRootElement => String = {
    case _: Data => typesFilename
    case _: RosettaMetaType => metaTypesFilename
    case _: RosettaEnumeration => enumsFilename
    case _: Function => functionsFilename
    case _ => "Unknown.txt"
  }

  private def extractEnclosingElements(rootElements: Iterable[RosettaRootElement]): Map[RosettaType, List[RosettaType]] =
    rootElements.foldLeft(Map.empty[RosettaType, List[RosettaType]].withDefaultValue(Nil)) {
      case (acc, e: Data) if CdmTypeGenerator.shouldBeSumType(e) =>
        e.getAttributes.asScala.foldLeft(acc)((acc, attr) => {
          acc.updatedWith(attr.getType) {
            case Some(list) => Some(e :: list)
            case None       => Some(List(e))
          }
        })
      case (acc, _) => acc
    }

  private def extractExtendingEnums(rootElements: Iterable[RosettaRootElement]): Map[RosettaType, Set[RosettaType]] =
    rootElements.foldLeft(Map.empty[RosettaType, Set[RosettaType]].withDefaultValue(Set.empty)) {
      case (acc, e: RosettaEnumeration) =>
        getAllAncestors(e).foldLeft(acc)((acc, ancestor) => {
          acc.updatedWith(ancestor) {
            case Some(descendants) => Some(descendants + e)
            case None => Some(Set(e))
          }
        })
      case (acc, _) => acc
    }

  private def translate(enclosingElements: Map[RosettaType, List[RosettaType]], extendingEnums: Map[RosettaType, Set[RosettaType]]): RosettaRootElement => String = {
      case e: Data => CdmTypeGenerator.generate(enclosingElements(e))(e)
      case e: RosettaMetaType => CdmMetaTypeGenerator.generate(e)
      case e: RosettaEnumeration => CdmEnumerationGenerator.generate(extendingEnums(e))(e)
      case e: Function => CdmFunctionGenerator.generate(e)
      case e => s"$e\n"
    }

  private def getAllAncestors(e: RosettaEnumeration): List[RosettaEnumeration] =
    Option(e.getSuperType) match {
      case Some(superType) => superType :: getAllAncestors(superType)
      case None => Nil
    }

  private def generateFileHeaderForEnums(version: String): String =
    generateFileHeader(version, "org.isda.cdm.scala")

  private def generateFileHeaderForFunctions(version: String): String =
    generateFileHeader(version, "org.isda.cdm.scala.functions", "org.isda.cdm.scala._")

  private def generateFileHeaderForMetatypes(version: String): String =
    generateFileHeader(version, "org.isda.cdm.scala")

  private def generateFileHeaderForTypes(version: String): String =
    generateFileHeader(version, "org.isda.cdm.scala")

  private def generateFileHeader(version: String, pkg: String, imports: String*): String =
    s"""/**
       |  * This file is auto-generated from the ISDA Common Domain Model, do not edit.
       |  * Version: $version
       |  */
       |
       |package $pkg
       |
       |import java.time._
       |import scala.math.BigDecimal
       |${imports.map("import " + _).mkString("\n")}
       |""".stripMargin
}
