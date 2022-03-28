package com.regnosys.rosetta.generator.scalawrapper

import scala.jdk.CollectionConverters._

import com.regnosys.rosetta.generator.`object`.{ExpandedAttribute, ExpandedType}
import com.regnosys.rosetta.generator.util.RosettaAttributeExtensions
import com.regnosys.rosetta.rosetta.simple.Attribute
import com.regnosys.rosetta.rosetta.{RosettaDefinable, RosettaType}

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
/*
  def generateFields(attributes: Iterable[Attribute]): String = {
    if (attributes.isEmpty)
      "\n"
    else
      attributes.map(a => s"  def ${a.getName}: ${rosettaAttrToScalaType(a)}").mkString(" {\n", ",\n", "}\n")
  }
*/
  def generateOptionalComment(e: RosettaDefinable, indent: String = ""): String =
    Option(e.getDefinition) match {
      case Some(defn) => s"$indent/** $defn */\n"
      case None => ""
    }

  def generatePartialClassComment(e: RosettaDefinable): String = {
    Option(e.getDefinition) match {
      case Some(defn) => s"""/** $defn
                            |  *
                            |""".stripMargin
      case None => "/**\n"
    }
  }

  def generateParamComments(attributes: Iterable[Attribute]): String = {
    val comments =
      for {
        attr <- attributes
        defnOpt = Option(attr.getDefinition)
        if defnOpt.exists(_.nonEmpty)
      } yield s"""  * @param ${attr.getName} ${defnOpt.getOrElse("")}"""
    comments.mkString("\n")
  }

  def debugFunction(f: com.regnosys.rosetta.rosetta.simple.Function): String =
    s"""$f
       |  getName ${f.getName}
       |  getConditions ${f.getConditions.asScala}
       |  getDefinition ${f.getDefinition}
       |  getReferences ${f.getReferences.asScala}
       |  getAnnotations ${f.getAnnotations.asScala}
       |  getOutput ${f.getOutput}
       |  getPostConditions ${f.getPostConditions}
       |  getInputs ${f.getInputs.asScala}
       |  getOperations ${f.getOperations.asScala}
       |  getShortcuts ${f.getShortcuts.asScala}
       |  getReferences ${f.getReferences.asScala}
       |""".stripMargin

  def debugAttribute(a: Attribute): String =
    s"""$a
       | getName ${a.getName}
       | getType ${a.getType}
       | isOverride ${a.isOverride}
       | isOnlyElement ${a.isOnlyElement}
       | isIsTypeInferred ${a.isIsTypeInferred}
       | getDefinition ${a.getDefinition}
       | getSynonyms ${a.getSynonyms.asScala}
       | getAnnotations ${a.getAnnotations.asScala}
       | getRuleReference ${a.getRuleReference}
       | getCard ${a.getCard}
       | getReferences ${a.getReferences}
       |""".stripMargin
}
