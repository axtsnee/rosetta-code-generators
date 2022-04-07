package com.regnosys.rosetta.generator.scalawrapper

import scala.jdk.CollectionConverters._
import com.regnosys.rosetta.rosetta.{RosettaEnumValue, RosettaEnumeration, RosettaType}
import GeneratorFunctions._
import com.regnosys.rosetta.generator.java.enums.EnumHelper

case class CdmEnumerationGenerator(
    analysis: RootElementAnalyzer
) extends AbstractCdmGenerator(analysis.enums) {
  private val subtypesByEnum: Map[RosettaEnumeration, Set[RosettaEnumeration]] =
    analysis.enums.foldLeft(Map.empty[RosettaEnumeration, Set[RosettaEnumeration]]) {
      case (acc, e: RosettaEnumeration) =>
        getAllAncestors(e).foldLeft(acc)((acc, ancestor) => {
          acc.updatedWith(ancestor) {
            case Some(descendants) => Some(descendants + e)
            case None => Some(Set(e))
          }
        })
    }.withDefaultValue(Set.empty)

  override val dependencies: RosettaEnumeration => Iterable[RosettaEnumeration] =
    analysis.enums.foldLeft(subtypesByEnum) {
      case (acc, e: RosettaEnumeration) =>
        getAllAncestors(e).foldLeft(acc)((acc, ancestor) => {
          acc.updatedWith(e) {
            case Some(ancestors) => Some(ancestors + ancestor)
            case None => Some(Set(ancestor))
          }
        })
    }.withDefaultValue(Set.empty)

  override val derivePackageName: RosettaEnumeration => String =
    CdmEnumerationGenerator.derivePackageName

  override val translate: RosettaEnumeration => String = e => {
    generateSealedTrait(e) + generateCompanionObject(e, subtypesByEnum(e))
  }

  private def getAllAncestors(e: RosettaEnumeration): List[RosettaEnumeration] =
    Option(e.getSuperType) match {
      case Some(superType) => superType :: getAllAncestors(superType)
      case None => Nil
    }

  private def generateSealedTrait(e: RosettaEnumeration): String =
    s"${generateOptionalComment(e)}sealed trait ${e.getName}\n"

  private def generateCompanionObject(e: RosettaEnumeration, extendingEnums: Iterable[RosettaEnumeration]): String = {
    val name = e.getName
    val javaName = rosettaTypeToJavaType(e)
    val enumValues = e.getEnumValues.asScala
    val caseObjects = enumValues.map(generateCaseObject(e, extendingEnums)).mkString
    val enumValuesByAncestor = getValuesByAncestor(e)
    val inheritedVals = enumValuesByAncestor.keys.map(generateVal(e, enumValuesByAncestor)).mkString
    val allEnumValues = enumValuesByAncestor.keys ++ enumValues
    val scalaMatchStatements = allEnumValues.map(matchOnScalaValue(e)).mkString("\n")
    val javaMatchStatements = allEnumValues.map(matchOnJavaValue(e)).mkString("\n")
    s"""object $name {
       |$inheritedVals$caseObjects
       |  implicit class ${scalaTypeClassName(name)}(x: $name) {
       |    def asJava: $javaName = x match {
       |$scalaMatchStatements
       |    }
       |  }
       |
       |  implicit class ${javaTypeClassName(name)}(x: $javaName) {
       |    def asScala: $name = x match {
       |$javaMatchStatements
       |    }
       |  }
       |}
       |""".stripMargin
  }

  private def matchOnScalaValue(e: RosettaEnumeration)(v: RosettaEnumValue): String = {
    val pattern =
      if (!e.getEnumValues.asScala.contains(v))
        s"${v.getEnumeration.getName}.${v.getName}"
      else if (v.getName.head.isUpper)
        v.getName
      else
        s"`${v.getName}`"
    s"      case $pattern => ${rosettaTypeToJavaType(e)}.${EnumHelper.formatEnumName(v.getName)}"
  }

  private def matchOnJavaValue(e: RosettaEnumeration)(v: RosettaEnumValue): String =
    s"      case ${rosettaTypeToJavaType(e)}.${EnumHelper.formatEnumName(v.getName)} => ${v.getName}"

  private def getValuesByAncestor(e: RosettaEnumeration): Map[RosettaEnumValue, RosettaEnumeration] = {
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
    val extending = generateExtendsClauseFromTypes(superTrait :: extendingEnums.toList)
    s"$comment  case object ${e.getName}$extending\n"
  }
}
object CdmEnumerationGenerator {
  def derivePackageName(e: RosettaEnumeration): String =
    s"${AbstractCdmGenerator.basePkg}.${e.getModel.getName}.enums"
}
