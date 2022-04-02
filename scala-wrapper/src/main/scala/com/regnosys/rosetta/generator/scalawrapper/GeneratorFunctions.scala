package com.regnosys.rosetta.generator.scalawrapper

import scala.jdk.CollectionConverters._

import com.regnosys.rosetta.rosetta.simple.{Annotation, AnnotationRef, Attribute}
import com.regnosys.rosetta.rosetta.{RosettaDefinable, RosettaType}

object GeneratorFunctions {
  def mapNullToEmptyString(x: => String): String = Option(x).getOrElse("")

  def isSingleOptional(a: Attribute): Boolean = {
    val cardinality = a.getCard
    cardinality.getInf == 0 && cardinality.getSup == 1
  }

  def isMultiple(a: Attribute): Boolean =
    a.getCard.isIsMany

  def rosettaAttrToScalaType(a: Attribute): String = {
    val baseType = rosettaTypeToScalaType(a.getType)
    if (isSingleOptional(a)) s"Option[$baseType]"
    else if (isMultiple(a)) s"List[$baseType]"
    else baseType
  }

  def rosettaTypeToScalaType(typ: RosettaType): String =
    Option(typ.getName)
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
      case "productType" => "String" //RQualifiedType.PRODUCT_TYPE
      case "eventType" => "String" //RQualifiedType.EVENT_TYPE
      case "calculation" => "String" //RCalculationType.CALCULATION
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
      case "productType" => "String" //RQualifiedType.PRODUCT_TYPE
      case "eventType" => "String" //RQualifiedType.EVENT_TYPE
      case "calculation" => "String" //RCalculationType.CALCULATION
      case _ => s"${typ.getModel.getName}.${typ.getName}"
    }

  def hasMetadataAnnotation(a: Attribute): Boolean =
    a.getAnnotations.asScala.exists(_.getAnnotation.getName == "metadata")

  def convertJavaAttributeToScala(
      a: Attribute,
      nameOpt: Option[String] = None,
      customConverterOpt: Option[String] = None
  ): String = {
    val name = nameOpt.getOrElse(a.getName)
    val isMeta = hasMetadataAnnotation(a)
    val customConverter = customConverterOpt.getOrElse(s"${a.getType.getName}.fromJava")
    if (isSingleOptional(a))
      s"Option($name)${mapJavaAttributeToScala(a, customConverter, isMeta)}"
    else if (isMultiple(a))
      s"$name.asScala.toList${mapJavaAttributeToScala(a, customConverter, isMeta)}"
    else {
      val value = if (isMeta) s"$name.getValue" else name
      a.getType.getName match {
        case "string" | "int" | "boolean" | "time" | "dateTime" | "zonedDateTime" => value
        case "productType" => value //RQualifiedType.PRODUCT_TYPE
        case "eventType" => value //RQualifiedType.EVENT_TYPE
        case "calculation" => value //RCalculationType.CALCULATION
        case "date" => s"$value.toLocalDate"
        case "number" => s"BigDecimal($value)"
        case _ => s"$customConverter($value)"
      }
    }
  }

  private def mapJavaAttributeToScala(a: Attribute, customConverter: String, isMeta: Boolean): String =
    a.getType.getName match {
      case "string" | "int" | "boolean" | "time" | "dateTime" | "zonedDateTime" if isMeta => ".map(_.getValue)"
      case "string" | "int" | "boolean" | "time" | "dateTime" | "zonedDateTime" => ""
      case "productType" | "eventType" | "calculation" if isMeta => ".map(_.getValue)"
      case "productType" | "eventType" | "calculation" => ""
      case "date" if isMeta => ".map(_.getValue.toLocalDate)"
      case "date" => ".map(_.toLocalDate)"
      case "number" if isMeta => ".map(b => BigDecimal(b.getValue))"
      case "number" => ".map(BigDecimal.apply)"
      case _ if isMeta => s".map(a => $customConverter(a.getValue))"
      case _ => s".map($customConverter)"
    }

  def convertScalaAttributeToJava(a: Attribute, nameOpt: Option[String] = None): String = {
    val name = nameOpt.getOrElse(a.getName)
    rosettaAttrToScalaType(a) match {
      case s"Option[$t]" => s"$name${mapScalaToJava(t)}.orNull"
      case s"List[$t]" => s"$name${mapScalaToJava(t)}.asJava"
      case "Boolean" | "Int" | "String" | "LocalTime" | "LocalDateTime" | "ZonedDateTime" => name
      case "productType" | "eventType" | "calculation" => name
      case "BigDecimal" => s"$name.bigDecimal"
      case "LocalDate"  => s"com.rosetta.model.lib.records.Date.of($name)"
      case typeName => s"$typeName.toJava($name)"
    }
  }

  private def mapScalaToJava(typeName: String): String =
    typeName match {
      case "String" | "LocalTime" | "LocalDateTime" | "ZonedDateTime" => ""
      case "productType" | "eventType" | "calculation" => ""
      case "Boolean" => ".map(b => b: java.lang.Boolean)"
      case "Int" => ".map(i => i: Integer)"
      case "BigDecimal" => ".map(_.bigDecimal)"
      case "LocalDate"  => ".map(com.rosetta.model.lib.records.Date.of)"
      case _ => s".map($typeName.toJava)"
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
