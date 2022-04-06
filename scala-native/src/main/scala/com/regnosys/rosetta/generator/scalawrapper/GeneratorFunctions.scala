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

  def rosettaAttrToJavaType(a: Attribute): String = {
    val baseType = rosettaTypeToJavaType(a.getType)
    if (isMultiple(a)) s"java.util.List[_ <: $baseType]"
    else baseType
  }

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
      case "int" => "java.lang.Integer"
      case "time" => "LocalTime"
      case "date" => "com.rosetta.model.lib.records.Date"
      case "dateTime" => "LocalDateTime"
      case "zonedDateTime" => "ZonedDateTime"
      case "number" => "java.math.BigDecimal"
      case "boolean" => "java.lang.Boolean"
      case "productType" => "String" //RQualifiedType.PRODUCT_TYPE
      case "eventType" => "String" //RQualifiedType.EVENT_TYPE
      case "calculation" => "String" //RCalculationType.CALCULATION
      case _ => s"${typ.getModel.getName}.${typ.getName}"
    }

  def hasMetadataAnnotation(a: Attribute): Boolean =
    a.getAnnotations.asScala.exists(_.getAnnotation.getName == "metadata")

  def convertRosettaAttributeFromJavaToScalaTry(
      a: Attribute,
      nameOpt: Option[String] = None,
      customConverterOpt: Option[String => String] = None
  ): String = {
    val name = nameOpt.getOrElse(a.getName)
    val typeName = a.getType.getName
    val isMeta = hasMetadataAnnotation(a)
    val customConverter = customConverterOpt.getOrElse((x: String) => s"new $typeName.ScalaConverter($x).asScala")
    if (isSingleOptional(a)) {
      val mapping = mapRosettaAttributeFromJavaToScalaTry(a, customConverter, isMeta)
      s"traverseTry(Option($name))($mapping)"
    } else if (isMultiple(a)) {
      val mapping = mapRosettaAttributeFromJavaToScalaTry(a, customConverter, isMeta)
      s"traverseTry(Option($name.asScala).getOrElse(Nil).toList)($mapping)"
    } else {
      val value = if (isMeta) s"$name.getValue" else name
      convertRosettaTypeFromJavaToScalaTry(typeName, value, customConverter)
    }
  }

  def convertRosettaTypeFromJavaToScalaTry(typeName: String, value: String, customConverter: String => String): String =
    typeName match {
      case "string" | "int" | "boolean" | "time" | "dateTime" | "zonedDateTime" => s"Success($value)"
      case "productType" => s"Success($value)" //RQualifiedType.PRODUCT_TYPE
      case "eventType" => s"Success($value)" //RQualifiedType.EVENT_TYPE
      case "calculation" => s"Success($value)" //RCalculationType.CALCULATION
      case "date" => s"Success($value.toLocalDate)"
      case "number" => s"Success(BigDecimal($value))"
      case enum if enum.endsWith("Enum") => s"Success(new $enum.ScalaConverter($value).asScala)"
      case _ => customConverter(value)
    }

  private def mapRosettaAttributeFromJavaToScalaTry(a: Attribute, customConverter: String => String, isMeta: Boolean): String =
    a.getType.getName match {
      case "string" | "int" | "boolean" | "time" | "dateTime" | "zonedDateTime" if isMeta => "x => Success(x.getValue)"
      case "string" | "time" | "dateTime" | "zonedDateTime" => "Success.apply"
      case "int" => "i => Success(i: Int)"
      case "boolean" => "n => Success(n: Boolean)"
      case "productType" | "eventType" | "calculation" if isMeta => ".map(x => Success(x.getValue))"
      case "productType" | "eventType" | "calculation" => "Success.apply"
      case "date" if isMeta => "d => Try(d.getValue.toLocalDate)"
      case "date" => "d => Try(d.toLocalDate)"
      case "number" if isMeta => "b => Success(BigDecimal(b.getValue))"
      case "number" => "b => Success(BigDecimal(b))"
      case enum if enum.endsWith("Enum") && isMeta => s"e => Success(new $enum.ScalaConverter(e.getValue).asScala)"
      case enum if enum.endsWith("Enum") => s"e => Success(new $enum.ScalaConverter(e).asScala)"
      case _ if isMeta => s"a => ${customConverter("a.getValue")}"
      case _ => s"a => ${customConverter("a")}"
    }

  def convertRosettaAttributeFromScalaToJava(a: Attribute, nameOpt: Option[String] = None): String =
    convertScalaTypeToJava(rosettaAttrToScalaType(a), nameOpt.getOrElse(a.getName))

  def convertScalaTypeToJava(typeToConvert: String, thingToConvert: String): String = {
    typeToConvert match {
      case s"Option[$t]" => s"$thingToConvert${mapScalaToJava(t)}.orNull"
      case s"List[$t]" => s"$thingToConvert${mapScalaToJava(t)}.asJava"
      case "Boolean" | "Int" | "String" | "LocalTime" | "LocalDateTime" | "ZonedDateTime" => thingToConvert
      case "productType" | "eventType" | "calculation" => thingToConvert
      case "BigDecimal" => s"$thingToConvert.bigDecimal"
      case "LocalDate"  => s"com.rosetta.model.lib.records.Date.of($thingToConvert)"
      case _ => s"new $typeToConvert.JavaConverter($thingToConvert).asJava"
    }
  }

  private def mapScalaToJava(typeToConvert: String): String =
    typeToConvert match {
      case "String" | "LocalTime" | "LocalDateTime" | "ZonedDateTime" => ""
      case "productType" | "eventType" | "calculation" => ""
      case "Boolean" => ".map(b => b: java.lang.Boolean)"
      case "Int" => ".map(i => i: Integer)"
      case "BigDecimal" => ".map(_.bigDecimal)"
      case "LocalDate"  => ".map(com.rosetta.model.lib.records.Date.of)"
      case _ => s".map(t => new $typeToConvert.JavaConverter(t).asJava)"
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
