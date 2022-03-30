package com.regnosys.rosetta.generator.scalawrapper

import scala.jdk.CollectionConverters._
import com.regnosys.rosetta.rosetta.{RosettaEnumValue, RosettaEnumeration, RosettaType}
import GeneratorFunctions._

case class CdmEnumerationGenerator(
    analysis: RootElementAnalyzer
) extends AbstractCdmGenerator(analysis.enums, analysis.nsToPkgs) {
  override val dependencies: RosettaEnumeration => Iterable[RosettaEnumeration] =
    analysis.enums.foldLeft(Map.empty[RosettaEnumeration, Set[RosettaEnumeration]]) {
      case (acc, e: RosettaEnumeration) =>
        getAllAncestors(e).foldLeft(acc)((acc, ancestor) => {
          acc.updatedWith(ancestor) {
            case Some(descendants) => Some(descendants + e)
            case None => Some(Set(e))
          }
        })
    }.withDefaultValue(Set.empty)

  override val derivePackageName: RosettaEnumeration => String =
    CdmEnumerationGenerator.derivePackageName

  override val translate: RosettaEnumeration => String = e => {
    generateSealedTrait(e) + generateCompanionObject(e, dependencies(e))
  }

  private def getAllAncestors(e: RosettaEnumeration): List[RosettaEnumeration] =
    Option(e.getSuperType) match {
      case Some(superType) => superType :: getAllAncestors(superType)
      case None => Nil
    }

  private def generateSealedTrait(e: RosettaEnumeration): String = {
    val comment = generateOptionalComment(e)
    s"${comment}sealed trait ${e.getName}\n"
  }

  private def generateCompanionObject(e: RosettaEnumeration, extendingEnums: Iterable[RosettaEnumeration]): String = {
    val ancestorVals = getInheritedEnumValues(e)
    val inheritedVals = ancestorVals.keys.map(generateVal(e, ancestorVals))
    val enumValues = e.getEnumValues.asScala
    val caseObjects = enumValues.map(generateCaseObject(e, extendingEnums))
    s"""object ${e.getName} {
       |${inheritedVals.mkString}${caseObjects.mkString}}
       |""".stripMargin
  }

  private def getInheritedEnumValues(e: RosettaEnumeration): Map[RosettaEnumValue, RosettaEnumeration] = {
    def loop(enumOpt: Option[RosettaEnumeration]): Map[RosettaEnumValue, RosettaEnumeration] = enumOpt match {
      case Some(enum) => loop(Option(enum.getSuperType)) ++
        enum.getEnumValues.asScala.map(_ -> enum)
      case None => Map.empty
    }
    loop(Option(e.getSuperType))
  }

  private def generateVal(superTrait: RosettaType, inheritedValues: Map[RosettaEnumValue, RosettaEnumeration])(e: RosettaEnumValue): String = {
    val comment = generateOptionalComment(e, "  ")
    val valName = e.getName
    val ancestor = inheritedValues(e)
    s"$comment  val $valName: ${superTrait.getName} = ${ancestor.getName}.$valName\n"
  }

  private def generateCaseObject(superTrait: RosettaEnumeration, extendingEnums: Iterable[RosettaEnumeration])(e: RosettaEnumValue): String = {
    val comment = generateOptionalComment(e, "  ")
    val caseObjectName = e.getName
    val extending = generateExtendsClauseFromTypes(superTrait :: extendingEnums.toList)
    s"$comment  case object $caseObjectName$extending\n"
  }
}
object CdmEnumerationGenerator {
  def derivePackageName(e: RosettaEnumeration): String =
    s"${AbstractCdmGenerator.basePkg}.${e.getModel.getName}.enums"
}
