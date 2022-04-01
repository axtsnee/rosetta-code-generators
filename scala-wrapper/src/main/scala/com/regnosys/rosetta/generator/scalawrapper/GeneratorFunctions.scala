package com.regnosys.rosetta.generator.scalawrapper

import scala.jdk.CollectionConverters._
import com.regnosys.rosetta.generator.`object`.{ExpandedAttribute, ExpandedType}
import com.regnosys.rosetta.generator.util.RosettaAttributeExtensions
import com.regnosys.rosetta.rosetta.simple.{Annotation, AnnotationRef, Attribute}
import com.regnosys.rosetta.rosetta.{RosettaDefinable, RosettaType}

object GeneratorFunctions {
  def mapNullToEmptyString(x: => String): String = Option(x).getOrElse("")

  def rosettaAttrToScalaType(attr: Attribute): String = {
    val baseType = rosettaTypeToScalaType(attr.getType)
    val cardinality = attr.getCard
    if (cardinality.getInf == 0 && cardinality.getSup == 1) s"Option[$baseType]"
    else if (cardinality.isIsMany) s"List[$baseType]"
    else baseType
  }

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

  def rosettaTypeToJavaType(typ: RosettaType): String =
    typ.getName match {
      case "string" => "String"
      case "int" => "int"
      case "time" => "LocalTime"
      case "date" => "LocalDate"
      case "dateTime" => "LocalDateTime"
      case "zonedDateTime" => "ZonedDateTime"
      case "number" => "java.math.BigDecimal"
      case "boolean" => "boolean"
      case "productType" => "Object" //RQualifiedType.PRODUCT_TYPE
      case "eventType" => "Object" //RQualifiedType.EVENT_TYPE
      case "calculation" => "Object" //RCalculationType.CALCULATION
      case _ => s"${typ.getModel.getName}.${typ.getName}"
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

  def generateOptionalComment(e: RosettaDefinable, indent: String = ""): String =
    Option(e.getDefinition) match {
      case Some(defn) if defn.nonEmpty => s"$indent/** $defn */\n"
      case _ => ""
    }

  def generatePartialClassComment(e: RosettaDefinable): String = {
    Option(e.getDefinition) match {
      case Some(defn) if defn.nonEmpty =>
        s"""/** $defn
           |  *
           |""".stripMargin
      case _ => "/**\n"
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
       |  getDefinition ${f.getDefinition}
       |  getInputs ${f.getInputs.asScala}
       |  getOutput ${f.getOutput}
       |  getConditions ${f.getConditions.asScala}
       |  getPostConditions ${f.getPostConditions.asScala}
       |  getAnnotations ${f.getAnnotations.asScala.map(debugAnnotationRef)}
       |  getReferences ${f.getReferences.asScala}
       |  getOperations ${f.getOperations.asScala}
       |  getShortcuts ${f.getShortcuts.asScala}
       |""".stripMargin

  def debugAnnotationRef(a: AnnotationRef): String =
    s"""$a
       | getQualifiers ${a.getQualifiers.asScala}
       | getAnnotation ${debugAnnotation(a.getAnnotation)}
       |""".stripMargin

  def debugAnnotation(a: Annotation): String =
    s"""$a
       | eIsProxy ${a.eIsProxy}
       | getName ${a.getName}
       | getDefinition ${a.getDefinition}
       | getAttributes ${a.getAttributes.asScala}
       | getModel ${a.getModel}
       | getPrefix ${a.getPrefix}
       | getAnnotations ${a.getAnnotations.asScala.map(debugAnnotationRef)}
       |""".stripMargin

  def debugAttribute(a: Attribute): String =
    s"""$a
       | getName ${a.getName}
       | getType ${a.getType}
       | getDefinition ${a.getDefinition}
       | getCard ${a.getCard}
       | getAnnotations ${a.getAnnotations.asScala.map(debugAnnotationRef)}
       | isOverride ${a.isOverride}
       | isOnlyElement ${a.isOnlyElement}
       | isIsTypeInferred ${a.isIsTypeInferred}
       | getSynonyms ${a.getSynonyms.asScala}
       | getRuleReference ${a.getRuleReference}
       | getReferences ${a.getReferences}
       |""".stripMargin
}
