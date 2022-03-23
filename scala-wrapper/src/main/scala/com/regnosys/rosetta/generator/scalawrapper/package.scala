package com.regnosys.rosetta.generator

import com.regnosys.rosetta.generator.util.RosettaAttributeExtensions
import com.regnosys.rosetta.rosetta.{RosettaDefinable, RosettaType}
import com.regnosys.rosetta.rosetta.simple.{Attribute, Data}

package object scalawrapper {
  def mapNullToEmptyString(x: => String): String = Option(x).getOrElse("")

  def rosettaAttrToScalaType(attr: Attribute): String = {
    val expandedAttr = RosettaAttributeExtensions.toExpandedAttribute(attr)
    val baseType =
      if (expandedAttr.getType.isBuiltInType)
        expandedAttr.getType.getName match {
          case "string" => "String"
          case "int" => "Int"
          case "time" => "java.time.LocalTime"
          case "date" => "java.time.LocalDate"
          case "dateTime" => "java.time.LocalDateTime"
          case "zonedDateTime" => "java.time.ZonedDateTime"
          case "number" => "scala.math.BigDecimal"
          case "boolean" => "Boolean"
          case "productType" => "String" //RQualifiedType.PRODUCT_TYPE
          case "eventType" => "String" //RQualifiedType.EVENT_TYPE
          case "calculation" => "String" //RCalculationType.CALCULATION
          case x =>
            println(s"${expandedAttr.getType} is a builtin type we have not encountered before")
            x
        }
      else
        expandedAttr.getType.getName
    if (expandedAttr.isSingleOptional) s"Option[$baseType]"
    else if (expandedAttr.isMultiple) s"List[$baseType]"
    else baseType
  }

  def mixSuperTypeWithEnclosingTypes(e: Data, enclosingTypes: Iterable[RosettaType]): List[RosettaType] =
    Option(e.getSuperType) match {
      case Some(superType) => superType :: enclosingTypes.toList
      case None => enclosingTypes.toList
    }

  def generateExtendsClauseFromTypes(superTypes: Iterable[RosettaType]): String =
    generateExtendsClauseFromStrings(superTypes.map(_.getName))

  def generateExtendsClauseFromStrings(superTypes: Iterable[String]): String =
    superTypes match {
      case hd :: tl => s""" extends $hd${generateWithClausesFromStrings(tl)}"""
      case Nil => ""
    }

  def generateWithClausesFromTypes(superTypes: Iterable[RosettaType]): String =
    generateWithClausesFromStrings(superTypes.map(_.getName))

  def generateWithClausesFromStrings(superTypes: Iterable[String]): String =
    superTypes match {
      case hd :: tl => s" with $hd" + generateWithClausesFromStrings(tl)
      case Nil => ""
    }

  def generateFields(attributes: Iterable[Attribute]): String = {
    if (attributes.isEmpty)
      "\n"
    else
      attributes.map(a => s"  def ${a.getName}: ${a.getType}").mkString(" {\n", ",\n", "}\n")
  }

  def makeOptionalComment(e: RosettaDefinable): String =
    Option(e.getDefinition) match {
      case Some(defn) => s"/** $defn */\n"
      case None => ""
    }
}
