package com.regnosys.rosetta.generator.scalawrapper

import com.regnosys.rosetta.generator.`object`.{ExpandedAttribute, ExpandedType}
import com.regnosys.rosetta.generator.util.RosettaAttributeExtensions
import com.regnosys.rosetta.rosetta.{RosettaDefinable, RosettaType}
import com.regnosys.rosetta.rosetta.simple.{Attribute, Data}

object GeneratorFunctions {
  def mapNullToEmptyString(x: => String): String = Option(x).getOrElse("")

  def rosettaAttrToScalaType(attr: Attribute): String =
    rosettaAttrToScalaType(RosettaAttributeExtensions.toExpandedAttribute(attr))

  def rosettaAttrToScalaType(expandedAttr: ExpandedAttribute): String = {
    val baseType = rosettaTypeToScalaType(expandedAttr.getType)
    if (expandedAttr.isSingleOptional) s"Option[$baseType]"
    else if (expandedAttr.isMultiple) s"List[$baseType]"
    else baseType
  }

  def rosettaTypeToScalaType(typ: RosettaType): String =
    rosettaTypeToScalaType(RosettaAttributeExtensions.toExpandedType(typ))

  def rosettaTypeToScalaType(expandedType: ExpandedType): String =
    Option(expandedType.getName)
      .map(rosettaTypeNameToScalaType)
      .getOrElse("UndefinedRosettaType")

  def rosettaTypeNameToScalaType(typeName: String): String =
    typeName match {
      case "string" => "String"
      case "int" => "Int"
      case "time" => "LocalTime"
      case "date" => "LocalDate"
      case "dateTime" => "LocalDateTime"
      case "zonedDateTime" => "ZonedDateTime"
      case "number" => "BigDecimal"
      case "boolean" => "Boolean"
      case "productType" => "Any" //RQualifiedType.PRODUCT_TYPE
      case "eventType" => "Any" //RQualifiedType.EVENT_TYPE
      case "calculation" => "Any" //RCalculationType.CALCULATION
      case _ => typeName
    }

  def mixSuperTypeWithEnclosingTypes(e: Data, enclosingTypes: Iterable[RosettaType]): List[RosettaType] =
    Option(e.getSuperType) match {
      case Some(superType) => superType :: enclosingTypes.toList
      case None => enclosingTypes.toList
    }

  def generateExtendsClauseFromTypes(superTypes: Iterable[RosettaType]): String =
    generateExtendsClauseFromStrings(superTypes.map(rosettaTypeToScalaType))

  def generateExtendsClauseFromStrings(superTypes: Iterable[String]): String =
    superTypes match {
      case hd :: tl => s" extends $hd${generateWithClausesFromStrings(tl)}"
      case Nil => ""
    }

  def generateWithClausesFromTypes(superTypes: Iterable[RosettaType]): String =
    generateWithClausesFromStrings(superTypes.map(rosettaTypeToScalaType))

  def generateWithClausesFromStrings(superTypes: Iterable[String]): String =
    superTypes match {
      case hd :: tl => s" with $hd" + generateWithClausesFromStrings(tl)
      case Nil => ""
    }

  def generateFields(attributes: Iterable[Attribute]): String = {
    if (attributes.isEmpty)
      "\n"
    else
      attributes.map(a => s"  def ${a.getName}: ${rosettaAttrToScalaType(a)}").mkString(" {\n", ",\n", "}\n")
  }

  def makeOptionalComment(e: RosettaDefinable, indent: String = ""): String =
    Option(e.getDefinition) match {
      case Some(defn) => s"$indent/** $defn */\n"
      case None => ""
    }
}
