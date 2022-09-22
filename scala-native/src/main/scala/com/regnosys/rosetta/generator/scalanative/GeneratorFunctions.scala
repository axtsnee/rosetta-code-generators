package com.regnosys.rosetta.generator.scalanative

import scala.jdk.CollectionConverters._

import com.regnosys.rosetta.generator.util.RosettaAttributeExtensions
import com.regnosys.rosetta.rosetta.simple.{Annotated, Annotation, AnnotationRef, Attribute}
import com.regnosys.rosetta.rosetta.{RosettaDefinable, RosettaEnumeration, RosettaType}

object GeneratorFunctions {
  def mapNullToEmptyString(x: => String): String = Option(x).getOrElse("")

  def isSingleOptional(a: Attribute): Boolean = {
    val cardinality = a.getCard
    cardinality.getInf == 0 && cardinality.getSup == 1
  }

  def isMultiple(a: Attribute): Boolean =
    a.getCard.isIsMany

  def rosettaAttrToJavaType(a: Attribute): String = {
    val rosettaType = a.getType
    val isMeta = hasMetadataAnnotation(a)
    val baseType =
      if (isMeta) rosettaMetaToJavaInterface(a)
      else rosettaTypeToJavaType(rosettaType)
    if (isMultiple(a))
      rosettaType match {
        case builtIn if RosettaAttributeExtensions.isBuiltInType(builtIn) =>
          s"java.util.List[$baseType]"
        case _: RosettaEnumeration if !isMeta => s"java.util.List[$baseType]"
        case _ => s"java.util.List[_ <: $baseType]"
      }
    else
      baseType
  }

  def rosettaMetaToJavaInterface(a: Attribute): String = {
    val typ = a.getType
    if (hasMetadataReference(a))
      typ.getName match {
        case "date" =>
          "com.rosetta.model.metafields.BasicReferenceWithMetaDate"
        case "string" =>
          "com.rosetta.model.metafields.BasicReferenceWithMetaString"
        case typeName =>
          s"${typ.getModel.getName}.metafields.ReferenceWithMeta${toUpperFirst(typeName)}"
      }
    else
      s"${typ.getModel.getName}.metafields.FieldWithMeta${toUpperFirst(typ.getName)}"
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
      case typeName => s"${typ.getModel.getName}.$typeName"
    }

  def hasMetadataAnnotation(a: Annotated): Boolean =
    a.getAnnotations
      .asScala
      .exists(_.getAnnotation.getName == "metadata")

  def hasMetadataReference(a: Annotated): Boolean =
    a.getAnnotations
      .asScala
      .filter(_.getAnnotation.getName == "metadata")
      .exists { annotation =>
        val name = annotation.getAttribute.getName
        name == "reference" || name == "address"
      }

  def convertRosettaAttributeFromJavaToScalaTry(
      a: Attribute,
      nameOpt: Option[String] = None,
      customConverterOpt: Option[String => String] = None
  ): String = {
    val name = nameOpt.getOrElse(a.getName)
    val typeName = a.getType.getName
    val isMeta = hasMetadataAnnotation(a)
    val customConverter = customConverterOpt.getOrElse { (x: String) =>
      s"new $typeName.${javaTypeClassName(typeName)}($x).asScala"
    }
    if (isSingleOptional(a)) {
      val mapping =
        mapRosettaAttributeFromJavaToScalaTry(a, customConverter, isMeta)
      s"traverseTry(Option($name))($mapping)"
    } else if (isMultiple(a)) {
      val mapping =
        mapRosettaAttributeFromJavaToScalaTry(a, customConverter, isMeta)
      s"traverseTry(Option($name.asScala).getOrElse(Nil).toList)($mapping)"
    } else {
      val value =
        if (isMeta)
          s"Try($name.getValue)"
        else
          s"Try($name)"
      convertRosettaTypeFromJavaTryToScalaTry(typeName, value, customConverter)
    }
  }

  def convertRosettaTypeFromJavaTryToScalaTry(
      typeName: String,
      value: String,
      customConverter: String => String
  ): String =
    typeName match {
      case "string" | "int" | "boolean" | "time" | "dateTime" | "zonedDateTime" => value
      case "productType" => value //RQualifiedType.PRODUCT_TYPE
      case "eventType" => value //RQualifiedType.EVENT_TYPE
      case "calculation" => value //RCalculationType.CALCULATION
      case "date" => s"$value.map(_.toLocalDate)"
      case "number" => s"$value.map(BigDecimal.apply)"
      case enum if enum.endsWith("Enum") =>
        s"$value.map(v => new $enum.${javaTypeClassName(typeName)}(v).asScala)"
      case _ => s"$value.flatMap(v => ${customConverter("v")})"
    }

  private def mapRosettaAttributeFromJavaToScalaTry(
      a: Attribute,
      customConverter: String => String,
      isMeta: Boolean
  ) = {
    a.getType.getName match {
      case "int" | "boolean" | "string" | "time" | "dateTime" | "zonedDateTime" if isMeta =>
        "x => Try(x.getValue)"
      case "int" => "i => Try(i: Int)"
      case "boolean" => "n => Try(n: Boolean)"
      case "string" | "time" | "dateTime" | "zonedDateTime" => "Success.apply"
      case "productType" | "eventType" | "calculation" if isMeta =>
        ".map(x => Try(x.getValue))"
      case "productType" | "eventType" | "calculation" => "Success.apply"
      case "date" if isMeta => "d => Try(d.getValue.toLocalDate)"
      case "date" => "d => Try(d.toLocalDate)"
      case "number" if isMeta => "b => Try(BigDecimal(b.getValue))"
      case "number" => "b => Try(BigDecimal(b))"
      case enum if enum.endsWith("Enum") && isMeta =>
        s"e => Try(new $enum.${javaTypeClassName(enum)}(e.getValue).asScala)"
      case enum if enum.endsWith("Enum") =>
        s"e => Try(new $enum.${javaTypeClassName(enum)}(e).asScala)"
      case _ if isMeta => s"""a => ${customConverter("a.getValue")}"""
      case _ => s"a => ${customConverter("a")}"
    }
  }

  def convertRosettaAttributeFromScalaToJava(
      a: Attribute,
      couldBeMeta: Boolean,
      couldBeOption: Boolean,
      nameOpt: Option[String] = None
  ): String = {
    val typeToConvert = a.getType.getName
    val thingToConvert = nameOpt.getOrElse(a.getName)
    val isMeta = couldBeMeta && hasMetadataAnnotation(a)
    val isOption = couldBeOption && isSingleOptional(a)
    val isList = isMultiple(a)
    val metaBuilder = if (isMeta) rosettaMetaToJavaInterface(a) + ".builder" else ""
    if (isOption) {
      if (isMeta) s"$thingToConvert${mapRosettaToJavaMeta(a, typeToConvert, metaBuilder)}.orNull"
      else s"$thingToConvert${mapRosettaToJava(typeToConvert)}.orNull"
    } else if (isList) {
      if (isMeta) s"$thingToConvert${mapRosettaToJavaMeta(a, typeToConvert, metaBuilder)}.asJava"
      else s"$thingToConvert${mapRosettaToJava(typeToConvert)}.asJava"
    } else
      typeToConvert match {
        case "boolean" | "int" | "time" | "dateTime" | "zonedDateTime" => thingToConvert
        case "date" if isMeta =>
          createMetaBuilder(
            s"${rosettaMetaToJavaInterface(a)}.builder",
            s"com.rosetta.model.lib.records.Date.of($thingToConvert)")
        case "date" => s"com.rosetta.model.lib.records.Date.of($thingToConvert)"
        case "string" if isMeta =>
          createMetaBuilder(
            s"${rosettaMetaToJavaInterface(a)}.builder",
            thingToConvert)
        case "string" => thingToConvert
        case "productType" | "eventType" | "calculation" if isMeta => ???
        case "productType" | "eventType" | "calculation" => thingToConvert
        case "number" => s"$thingToConvert.bigDecimal"
        case _ if isMeta =>
          createMetaBuilder(
            s"${rosettaMetaToJavaInterface(a)}.builder",
            convertRosettaTypeFromScalaToJava(typeToConvert, thingToConvert))
        case _ => convertRosettaTypeFromScalaToJava(typeToConvert, thingToConvert)
      }
  }

  def convertRosettaTypeFromScalaToJava(rosettaName: String, thingToConvert: String): String =
    s"new $rosettaName.${scalaTypeClassName(rosettaName)}($thingToConvert).asJava"

  private def createMetaBuilder(metaBuilder: String, value: String, indent: String = ""): String = {
    val setMeta =
      if (metaBuilder.contains("FieldWithMeta"))
        s"""
           |$indent  .setMeta(MetaFields.builder
           |$indent    .addKey(Key.builder
           |$indent      .setScope("GLOBAL")
           |$indent      .setKeyValue(UUID.randomUUID.toString)
           |$indent    )
           |$indent  )
           |""".stripMargin
      else ""
    s"""$indent$metaBuilder$setMeta
       |$indent  .setValue($value)""".stripMargin
  }

  private def mapRosettaToJavaMeta(a: Attribute, typeToConvert: String, metaBuilder: String): String = {
    val (patternMatchVar, value) =
      typeToConvert match {
        case "string" | "time" | "dateTime" | "zonedDateTime" => ("t", "t")
        case "date"  => ("d", "com.rosetta.model.lib.records.Date.of(d)")
        case "productType" | "eventType" | "calculation" => ???
        case "boolean" => ("b", "b: java.lang.Boolean")
        case "int" => ("i", "i: Integer")
        case "number" => ("b", "b.bigDecimal")
        case _ => ("t", convertRosettaTypeFromScalaToJava(typeToConvert, "t"))
      }
    s""".map { $patternMatchVar =>
       |${createMetaBuilder(metaBuilder, value, "  ")}
       |}""".stripMargin
  }

  private def mapRosettaToJava(typeToConvert: String): String =
    typeToConvert match {
      case "string" | "time" | "dateTime" | "zonedDateTime" => ""
      case "date"  => ".map(com.rosetta.model.lib.records.Date.of)"
      case "productType" | "eventType" | "calculation" => ""
      case "boolean" => ".map(b => b: java.lang.Boolean)"
      case "int" => ".map(i => i: Integer)"
      case "number" => ".map(_.bigDecimal)"
      case _ => s""".map(t => ${convertRosettaTypeFromScalaToJava(typeToConvert, "t")})"""
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

  def generatePartialClassComment(e: RosettaDefinable, indent: String = ""): String = {
    Option(e.getDefinition) match {
      case Some(defn) if defn.nonEmpty =>
        s"""$indent/** $defn
           |$indent  *
           |""".stripMargin
      case _ => s"$indent/**\n"
    }
  }

  def generateParamComments(attributes: Iterable[Attribute], indent: String = ""): String = {
    val comments =
      for {
        attr <- attributes
        defnOpt = Option(attr.getDefinition)
        if defnOpt.exists(_.nonEmpty)
      } yield s"""$indent  * @param ${attr.getName} ${defnOpt.getOrElse("")}"""
    comments.mkString("\n")
  }

  def javaTypeClassName(typeName: String): String = s"Convert${typeName}ToScala"

  def scalaTypeClassName(typeName: String): String = s"Convert${typeName}ToJava"

  def toUpperFirst(s: String): String = s.head.toUpper + s.tail

  def toLowerFirst(s: String): String = s.head.toLower + s.tail

  def debugFunction(f: com.regnosys.rosetta.rosetta.simple.Function): String =
    s"""$f
       |  getName ${f.getName}
       |  getDefinition ${f.getDefinition}
       |  getInputs ${f.getInputs.asScala}
       |  getOutput ${f.getOutput}
       |  getConditions ${f.getConditions.asScala}
       |  getPostConditions ${f.getPostConditions.asScala}
       |  getAnnotations ${f.getAnnotations.asScala.map(debugAnnotationRef("    "))}
       |  getReferences ${f.getReferences.asScala}
       |  getOperations ${f.getOperations.asScala}
       |  getShortcuts ${f.getShortcuts.asScala}
       |""".stripMargin

  def debugAnnotationRef(indent: String)(a: AnnotationRef): String =
    s"""$indent$a
       |$indent  getAttribute ${a.getAttribute}
       |$indent  getQualifiers ${a.getQualifiers.asScala}
       |$indent  getAnnotation ${debugAnnotation(indent + "  ")(a.getAnnotation)}
       |""".stripMargin

  def debugAnnotation(indent: String)(a: Annotation): String =
    s"""$indent$a
       |$indent  eIsProxy ${a.eIsProxy}
       |$indent  getName ${a.getName}
       |$indent  getDefinition ${a.getDefinition}
       |$indent  getAttributes ${a.getAttributes.asScala}
       |$indent  getModel ${a.getModel}
       |$indent  getPrefix ${a.getPrefix}
       |$indent  getAnnotations ${a.getAnnotations.asScala.map(debugAnnotationRef(indent + "  "))}
       |""".stripMargin

  def debugAttribute(indent: String)(a: Attribute): String =
    s"""$indent$a
       |$indent  getName ${a.getName}
       |$indent  getType ${a.getType}
       |$indent  getDefinition ${a.getDefinition}
       |$indent  getCard ${a.getCard}
       |$indent  getAnnotations ${a.getAnnotations.asScala.map(debugAnnotationRef(indent + "  "))}
       |$indent  isOverride ${a.isOverride}
       |$indent  isOnlyElement ${a.isOnlyElement}
       |$indent  isIsTypeInferred ${a.isIsTypeInferred}
       |$indent  getSynonyms ${a.getSynonyms.asScala}
       |$indent  getRuleReference ${a.getRuleReference}
       |$indent  getReferences ${a.getReferences}
       |""".stripMargin
}
