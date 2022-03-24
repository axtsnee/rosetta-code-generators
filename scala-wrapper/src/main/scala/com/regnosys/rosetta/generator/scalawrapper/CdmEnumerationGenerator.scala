package com.regnosys.rosetta.generator.scalawrapper

import scala.jdk.CollectionConverters._

import com.regnosys.rosetta.rosetta.simple.Attribute
import com.regnosys.rosetta.rosetta.{RosettaDefinable, RosettaEnumValue, RosettaType}

object CdmEnumerationGenerator extends CdmGenerator {
  def generate(enclosingTypes: List[RosettaType]): CdmEnumeration => Vector[String] = c => {
    val allSuperTypes =
      Option(c.element.getSuperType) match {
        case Some(superType) => superType :: enclosingTypes
        case None => enclosingTypes
      }
    Vector(generateSealedTrait(c.element, allSuperTypes, Nil) +
      generateCompanionObject(c.element.getEnumValues.asScala, c.element))
  }

  def generateSealedTrait(r: RosettaType with RosettaDefinable, superTypes: Iterable[RosettaType], attributes: Iterable[Attribute]): String = {
    val comment = makeOptionalComment(r)
    s"""${comment}sealed trait ${r.getName}${generateExtendsClauseFromTypes(superTypes)}${generateFields(attributes)}"""
  }

  private def generateCompanionObject(enumValues: Iterable[RosettaEnumValue], superTrait: RosettaType): String = {
    val caseObjects = enumValues.map(generateCaseObject(superTrait))
    s"""object ${superTrait.getName} {
       |${caseObjects.mkString("\n")}}
       |""".stripMargin
  }

  private def generateCaseObject(superTrait: RosettaType)(e: RosettaEnumValue): String = {
    val caseObjectName = e.getName
    val extending = generateExtendsClauseFromTypes(List(superTrait))
    s"""  /** ${mapNullToEmptyString(e.getDefinition)} */
       |  case object $caseObjectName$extending
       |""".stripMargin
  }
}
