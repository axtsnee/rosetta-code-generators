package com.regnosys.rosetta.generator.scalawrapper

import scala.jdk.CollectionConverters._
import com.regnosys.rosetta.rosetta.{RosettaEnumValue, RosettaEnumeration, RosettaType}
import CdmGeneratorFunctions._

object CdmEnumerationGenerator {
  def generate(extendingEnums: Set[RosettaType]): RosettaEnumeration => String = e => {
    generateSealedTrait(e) + generateCompanionObject(e, extendingEnums)
  }

  def generateSealedTrait(e: RosettaEnumeration): String = {
    val comment = makeOptionalComment(e)
    s"${comment}sealed trait ${e.getName}\n"
  }

  private def generateCompanionObject(e: RosettaEnumeration, extendingEnums: Set[RosettaType]): String = {
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
    val comment = makeOptionalComment(e, "  ")
    val valName = e.getName
    val ancestor = inheritedValues(e)
    s"$comment  val $valName: ${superTrait.getName} = ${ancestor.getName}.$valName\n"
  }

  private def generateCaseObject(superTrait: RosettaType, extendingEnums: Set[RosettaType])(e: RosettaEnumValue): String = {
    val comment = makeOptionalComment(e, "  ")
    val caseObjectName = e.getName
    val extending = generateExtendsClauseFromTypes(superTrait :: extendingEnums.toList)
    s"$comment  case object $caseObjectName$extending\n"
  }
}
