package com.regnosys.rosetta.generator.scalawrapper

import scala.jdk.CollectionConverters._

import com.regnosys.rosetta.generator.util.RosettaAttributeExtensions
import com.regnosys.rosetta.rosetta.RosettaType
import com.regnosys.rosetta.rosetta.simple.{AnnotationRef, Attribute, Data}
import GeneratorFunctions._

case class CdmTypeGenerator(analysis: RootElementAnalyzer) extends AbstractCdmGenerator(analysis.types) {
  private val enclosingTypes: Map[RosettaType, List[RosettaType]] =
    analysis.types.foldLeft(Map.empty[RosettaType, List[Data]]) {
      case (acc, e) if shouldBeSumType(e) =>
        e.getAttributes.asScala.foldLeft(acc)((acc, attr) => {
          acc.updatedWith(attr.getType) {
            case Some(list) => Some(e :: list)
            case None       => Some(List(e))
          }
        })
      case (acc, _) => acc
    }.withDefaultValue(Nil)

  private val subtypesByType: Map[Data, Set[Data]] =
    analysis.types.foldLeft(Map.empty[Data, Set[Data]]) {
      case (acc, e: Data) =>
        getAllAncestors(e).foldLeft(acc)((acc, ancestor) => {
          acc.updatedWith(ancestor) {
            case Some(descendants) => Some(descendants + e)
            case None => Some(Set(e))
          }
        })
    }.withDefaultValue(Set.empty)

  override val dependencies: Data => List[RosettaType] =
    analysis.types.foldLeft(enclosingTypes) {
      case (outerAcc, e) =>
        val accWithSupertype = outerAcc.updatedWith(e) {
          case s@Some(list) => Option(e.getSuperType).map(_ :: list).orElse(s)
          case None         => Option(e.getSuperType).map(_ :: Nil)
        }
        getAllAttributes(e)
          .foldLeft(accWithSupertype)((innerAcc, attr) => {
            val attrType = attr.getType
            if (RosettaAttributeExtensions.isBuiltInType(attrType))
              innerAcc
            else
              innerAcc.updatedWith(e) {
                case Some(list) => Some(attrType :: list)
                case None       => Some(attrType :: Nil)
              }
        })
    }.withDefaultValue(Nil)

  override val derivePackageName: Data => String = CdmTypeGenerator.derivePackageName

  override val translate: Data => String = e => {
    val allSuperTypes = mixSuperTypeWithEnclosingTypes(e)
    if (shouldBeSumType(e)) {
      if (getAllAttributes(e).isEmpty) {
        generateEmptySealedTrait(e, allSuperTypes)
      } else {
        generateSealedTrait(e, allSuperTypes)
      }
    } else {
      generateTrait(e, allSuperTypes) + "\n" + generateCaseClass(e)
    }
  }

  private def getAllAncestors(e: Data): List[Data] =
    Option(e.getSuperType) match {
      case Some(superType) => superType :: getAllAncestors(superType)
      case None => Nil
    }

  private def shouldBeSumType(e: Data): Boolean = {
    val attrs = e.getAttributes.asScala
    getInheritedAttributes(e).isEmpty && (
      attrs.isEmpty || (
        e.getConditions.asScala.exists(c => Option(c.getConstraint).exists(_.isOneOf)) &&
        attrs.groupBy(_.getType).values.forall(_.lengthIs == 1)  && // the type of each attr is different
        attrs.forall { a =>
          isSingleOptional(a) &&
            !RosettaAttributeExtensions.isBuiltInType(a.getType) &&
            (a.getType match {
              case _: Data => true
              case _ => false
            })
        }
      )
    )
  }

  private def getInheritedAttributes(e: Data): Vector[Attribute] = {
    def loop(dOpt: Option[Data]): Vector[Attribute] = dOpt match {
      case Some(d) => loop(Option(d.getSuperType)) ++ d.getAttributes.asScala
      case None => Vector.empty
    }
    loop(Option(e.getSuperType))
  }

  private def getAllAttributes(e: Data): Vector[Attribute] =
    getInheritedAttributes(e) ++ e.getAttributes.asScala

  private def mixSuperTypeWithEnclosingTypes(e: Data): List[RosettaType] =
    Option(e.getSuperType) match {
      case Some(superType) => superType :: enclosingTypes(e)
      case None => enclosingTypes(e)
    }

  private def generateEmptySealedTrait(e: Data, superTypes: Iterable[RosettaType]): String = {
    val scalaTypeName = e.getName
    val traitComment = generateOptionalComment(e)
    val extending = generateExtendsClauseFromTypes(superTypes)
    val sealedTrait = s"${traitComment}sealed trait $scalaTypeName$extending"
    // subTypes must be sorted from deepest descendant to shallowest to
    // prevent a supertype's case statement from hiding the subtype's.
    val subTypes = subtypesByType(e).toList.sortBy(t => getAllAncestors(t).length).reverse
    if (subTypes.isEmpty) {
      sealedTrait + "\n"
    } else {
      val javaTypeName = rosettaTypeToJavaType(e)
      val toJavaStmts = subTypes.map { t =>
        val name = t.getName
        s"        case ja: $name => new $name.${scalaTypeClassName(name)}(ja).asJava"
      }.mkString("\n")
      val fromJavaStmts = subTypes.map { t =>
        val name = t.getName
        s"        case sc: ${rosettaTypeToJavaType(t)} => new $name.${javaTypeClassName(name)}(sc).asScala"
      }.mkString("\n")
      s"""$sealedTrait
         |object $scalaTypeName {
         |  implicit class ${scalaTypeClassName(scalaTypeName)}(t: $scalaTypeName) {
         |    def asJava: $javaTypeName =
         |      t match {
         |$toJavaStmts
         |      }
         |  }
         |
         |  implicit class ${javaTypeClassName(scalaTypeName)}(t: $javaTypeName) {
         |    def asScala(implicit validator: RosettaTypeValidator): Try[$scalaTypeName] =
         |      t match {
         |$fromJavaStmts
         |      }
         |  }
         |}
         |""".stripMargin
    }
  }

  private def generateSealedTrait(e: Data, superTypes: Iterable[RosettaType]): String = {
    val scalaTypeName = e.getName
    val traitComment = generateOptionalComment(e)
    val extending = generateExtendsClauseFromTypes(superTypes)
    val javaTypeName = rosettaTypeToJavaType(e)
    val descendantsBeforeAncestors: Ordering[Attribute] = (a, b) => {
      (a.getType, b.getType) match {
        case (ta: Data, tb: Data) =>
          if (getAllAncestors(ta).contains(tb)) -1
          else if (getAllAncestors(tb).contains(ta)) +1
          else 0
        case (_, _) => 0
      }
    }
    val attrs = e.getAttributes.asScala
    val javaObjectName = "ja"
    val scalaObjectName = "sc"
    val pattern =
      attrs
        .map(a => s"$javaObjectName.get${toUpperFirst(a.getName)}")
        .mkString("(Option(", ")).orElse(Option(", "))")
    s"""${traitComment}sealed trait $scalaTypeName$extending
       |object $scalaTypeName {
       |  implicit class ${scalaTypeClassName(scalaTypeName)}($scalaObjectName: $scalaTypeName) {
       |    def asJava: $javaTypeName = new $javaTypeName { self =>
       |${generateConvertingAccessor(e, attrs, scalaObjectName).mkString}
       |${generateStandardJavaMethods(scalaTypeName, javaTypeName, e.getAnnotations.asScala)}
       |    }
       |  }
       |
       |  implicit class ${javaTypeClassName(scalaTypeName)}($javaObjectName: $javaTypeName) {
       |    def asScala(implicit validator: RosettaTypeValidator): Try[$scalaTypeName] =
       |      $pattern match { // make sure subtype cases come before supertype to avoid unreachable code
       |${generateConversionCaseStatements(attrs.sorted(descendantsBeforeAncestors), javaObjectName).mkString}
       |      }
       |  }
       |}
       |""".stripMargin
  }

  private def generateConversionCaseStatements(attrs: Iterable[Attribute], javaObject: String) = {
    attrs.map { a =>
      val typeName = a.getType.getName
      val converter: String => String = x => s"new $typeName.${javaTypeClassName(typeName)}($x).asScala"
      val conversion = convertRosettaTypeFromJavaToScalaTry(typeName, javaObject, converter)
      s"""        case Some($javaObject: ${rosettaTypeToJavaType(a.getType)}) =>
         |          $conversion
         |""".stripMargin
    }
  }

  private def generateConvertingAccessor(e: Data, attrs: Iterable[Attribute], scalaObjectName: String) = {
    (getInheritedAttributes(e) ++ attrs).map { a =>
      val accessor = s"get${toUpperFirst(a.getName)}"
      val scalaType = rosettaTypeToScalaType(a.getType)
      val javaType = rosettaAttrToJavaType(a)
      val matched = "x"
      val conversion = convertRosettaAttributeFromScalaToJava(a, couldBeMeta = true, couldBeOption = false, Some(matched))
      s"""      override def $accessor: $javaType = $scalaObjectName match {
         |        case $matched: $scalaType =>
         |          $conversion
         |        case _ => None.orNull
         |      }
         |""".stripMargin
    }
  }

  private def generateTrait(e: Data, allSuperTypes: List[RosettaType]): String = {
    val scalaTypeName = e.getName
    val attrs = e.getAttributes.asScala
    val traitComment = generateOptionalComment(e)
    val fields = generateTraitFields(attrs)
    val extending = generateExtendsClauseFromTypes(allSuperTypes)
    val javaTypeName = rosettaTypeToJavaType(e)
    val allAttrs = getInheritedAttributes(e) ++ attrs
    val scalaObjectName = "sc"
    val javaObjectName = "ja"
    val escapedAttributeNames = allAttrs.map(n => escapeReservedWords(n.getName)).mkString(", ")
    s"""${traitComment}trait $scalaTypeName$extending {$fields}
        |object $scalaTypeName {
        |  implicit class ${scalaTypeClassName(scalaTypeName)}($scalaObjectName: $scalaTypeName) {
        |    def asJava: $javaTypeName = new $javaTypeName { self =>
        |${allAttrs.map(generateDelegatingAccessor(scalaObjectName)).mkString("\n")}
        |${generateStandardJavaMethods(scalaTypeName, javaTypeName, e.getAnnotations.asScala)}
        |    }
        |  }
        |
        |  implicit class ${javaTypeClassName(scalaTypeName)}($javaObjectName: $javaTypeName) {
        |    def asScala(implicit validator: RosettaTypeValidator): Try[$scalaTypeName] =
        |      for {
        |${allAttrs.map(a => generateJavaObjectAccessor(a, javaObjectName)).mkString("\n")}
        |      } yield Default$scalaTypeName($escapedAttributeNames)
        |  }
        |}
        |""".stripMargin
  }

  private def generateStandardJavaMethods(
      typeName: String,
      javaTypeName: String,
      annotationRefs: Iterable[AnnotationRef]
  ): String = {
    val metaType =
      if (annotationRefs.filter(_.getAnnotation.getName == "metadata").exists(_.getAttribute.getName == "template"))
        "MetaAndTemplateFields"
      else
        "MetaFields"
    s"""
       |      def build: $javaTypeName = self
       |      def toBuilder: $javaTypeName.${typeName}Builder = ???
       |      def getMeta(): com.rosetta.model.metafields.$metaType =
       |        new com.rosetta.model.metafields.$metaType.${metaType}BuilderImpl().build
       |""".stripMargin
  }

  private def generateCaseClass(e: Data): String = {
    val scalaTypeName = e.getName
    val javaTypeName = rosettaTypeToJavaType(e)
    val attributes = getAllAttributes(e)
    val classComment =
      if (attributes.flatMap(a => Option(a.getDefinition)).isEmpty)
        generateOptionalComment(e)
      else
        generatePartialClassComment(e) + generateParamComments(attributes) + "\n  */\n"
    val fields = generateCaseClassFields(attributes)
    val extending = generateExtendsClauseFromStrings(List(scalaTypeName))
    val javaFacade = s"$scalaTypeName.${scalaTypeClassName(scalaTypeName)}(this).asJava"
    s"""${classComment}final case class Default$scalaTypeName($fields)(implicit validator: RosettaTypeValidator)$extending {
        |  private val validation = validator.runProcessStep(classOf[$javaTypeName], $javaFacade)
        |  if (!validation.success)
        |    throw new IllegalStateException(validation.validationFailures.asScala.mkString("; "))
        |}
        |
        |""".stripMargin
  }

  private def generateJavaObjectAccessor(a: Attribute, ref: String): String = {
    val name = a.getName
    val accessor = s"$ref.get${toUpperFirst(name)}"
    val conversion = convertRosettaAttributeFromJavaToScalaTry(a, Some(accessor))
    s"        ${escapeReservedWords(name)} <- $conversion"
  }

  private def generateDelegatingAccessor(scalaObject: String)(a: Attribute): String = {
    val name = a.getName
    val field = s"$scalaObject.${escapeReservedWords(name)}"
    val accessor = s"get${toUpperFirst(name)}"
    val javaType = rosettaAttrToJavaType(a)
    val conversion = convertRosettaAttributeFromScalaToJava(a, couldBeMeta = true, couldBeOption = true, Some(field))
    s"""      override def $accessor: $javaType =
       |        $conversion
       |""".stripMargin
  }

  private def generateTraitFields(attributes: Iterable[Attribute]): String = {
    val fields = attributes.map { attr =>
      val comment = generateOptionalComment(attr, "  ")
      val name = escapeReservedWords(attr.getName)
      s"$comment  def $name: ${rosettaAttrToScalaType(attr)}"
    }
    if (fields.isEmpty) "" else fields.mkString("\n", "\n", "\n")
  }

  private def generateCaseClassFields(attributes: Iterable[Attribute]): String = {
    val fields = attributes.map(a => s"  ${escapeReservedWords(a.getName)}: ${rosettaAttrToScalaType(a)}")
    if (fields.isEmpty) "" else fields.mkString("\n", ",\n", "\n")
  }

  private def escapeReservedWords(s: String): String = s match {
    case "val" => "`val`"
    case safe  => safe
  }
}
object CdmTypeGenerator {
  def derivePackageName(e: Data): String =
    s"${AbstractCdmGenerator.basePkg}.${e.getModel.getName}.types"
}
