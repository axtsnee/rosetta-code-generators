package com.regnosys.rosetta.generator.scalanative

import scala.jdk.CollectionConverters._

import com.regnosys.rosetta.generator.util.RosettaAttributeExtensions
import com.regnosys.rosetta.rosetta.RosettaType
import com.regnosys.rosetta.rosetta.simple.{AnnotationRef, Attribute, Data}
import GeneratorFunctions._

case class ScalaTypeGenerator(analysis: RootElementAnalyzer) extends AbstractScalaGenerator(analysis.types) {
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

  override val derivePackageName: Data => String = ScalaTypeGenerator.derivePackageName

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
    val rosettaName = e.getName
    val scalaTypeName = rosettaName
    val traitComment = generateOptionalComment(e, "  ")
    val extending = generateExtendsClauseFromTypes(superTypes)
    s"$traitComment  sealed trait $scalaTypeName$extending\n"
  }

  private def generateSealedTrait(e: Data, superTypes: Iterable[RosettaType]): String = {
    val rosettaName = e.getName
    val javaTypeName = rosettaTypeToJavaType(e)
    val scalaTypeName = rosettaName
    val traitComment = generateOptionalComment(e, "  ")
    val extending = generateExtendsClauseFromTypes(superTypes)
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
    val conversionCaseStatements =
      generateConversionCaseStatements(attrs.sorted(descendantsBeforeAncestors)).mkString
    s"""$traitComment  sealed trait $scalaTypeName$extending
       |  object $scalaTypeName {
       |    implicit class ${scalaTypeClassName(rosettaName)}($scalaObjectName: $scalaTypeName) {
       |      def asJava: $javaTypeName.${rosettaName}Builder = {
       |        val builder = $javaTypeName.builder
       |${generateMetaMutator(getAllAnnotationRefs(e))}
       |${generateBuilderMutator(e, attrs, scalaObjectName).mkString}
       |        builder
       |      }
       |    }
       |
       |    implicit class ${javaTypeClassName(rosettaName)}(javaObject: $javaTypeName) {
       |      def asScalaResolvingReferences(
       |        implicit validator: RosettaTypeValidator
       |      ): Try[$scalaTypeName] = {
       |        val builder = javaObject.toBuilder
       |        new ReferenceResolverProcessStep(CdmReferenceConfig.get()).runProcessStep(classOf[$javaTypeName], builder)
       |        val $javaObjectName = builder.build
       |        $pattern match { // make sure subtype cases come before supertype to avoid unreachable code
       |$conversionCaseStatements
       |          case impossible => Failure(new IllegalStateException(s"Found impossible case '$$impossible'."))
       |        }
       |      }
       |
       |      def asScala(
       |        implicit validator: RosettaTypeValidator
       |      ): Try[$scalaTypeName] = {
       |        val $javaObjectName = javaObject
       |        $pattern match { // make sure subtype cases come before supertype to avoid unreachable code
       |$conversionCaseStatements
       |          case impossible => Failure(new IllegalStateException(s"Found impossible case '$$impossible'."))
       |        }
       |      }
       |    }
       |
       |${generateImplicitConverter(rosettaName, javaTypeName, scalaTypeName)}
       |  }
       |""".stripMargin
  }

  private def generateConversionCaseStatements(
      attrs: Iterable[Attribute]
  ): Iterable[String] =
    attrs.map { a =>
      val typeName = a.getType.getName
      val converter: String => String =
        x => s"new $typeName.${javaTypeClassName(typeName)}($x).asScala"
      val matched = "x"
      val conversion = convertRosettaTypeFromJavaTryToScalaTry(typeName, s"Success($matched)", converter)
      s"""          case Some($matched: ${rosettaTypeToJavaType(a.getType)}) =>
         |            $conversion
         |""".stripMargin
    }

  private def generateBuilderMutator(
                                          e: Data,
                                          attrs: Iterable[Attribute],
                                          scalaObjectName: String
                                        ): Vector[String] = {
    (getInheritedAttributes(e) ++ attrs).map { a =>
      val mutator = s"set${toUpperFirst(a.getName)}"
      val scalaType = rosettaTypeToScalaType(a.getType)
      val matched = "x"
      val conversion =
        convertRosettaAttributeFromScalaToJava(
          a,
          couldBeMeta = true,
          couldBeOption = false,
          nameOpt = Some(matched)
        )
      s"""        builder.$mutator(
         |          $scalaObjectName match {
         |            case $matched: $scalaType =>
         |              $conversion
         |            case _ => None.orNull
         |          }
         |        )
         |""".stripMargin
    }
  }

  private def generateTrait(e: Data, allSuperTypes: List[RosettaType]): String = {
    val rosettaName = e.getName
    val javaTypeName = rosettaTypeToJavaType(e)
    val scalaTypeName = rosettaName
    val attrs = e.getAttributes.asScala
    val traitComment = generateOptionalComment(e, "  ")
    val fields = generateTraitFields(attrs)
    val extending = generateExtendsClauseFromTypes(allSuperTypes)
    val allAttrs = getInheritedAttributes(e) ++ attrs
    val scalaObjectName = "sc"
    val javaObjectName = "ja"
    val forComprehensionTerms =
      allAttrs
        .map(a => generateForComprehensionTerm(a, javaObjectName))
        .mkString("\n")
    val escapedAttributeNames =
      allAttrs.map(n => escapeReservedWords(n.getName)).mkString(", ")
    s"""$traitComment  trait $scalaTypeName$extending {$fields  }
        |  object $scalaTypeName {
        |    implicit class ${scalaTypeClassName(rosettaName)}($scalaObjectName: $scalaTypeName) {
        |      def asJava: $javaTypeName.${rosettaName}Builder = {
        |        val builder = $javaTypeName.builder
        |${generateMetaMutator(getAllAnnotationRefs(e))}
        |${allAttrs.map(generateBuilderMutator(scalaObjectName)).mkString("\n")}
        |        builder
        |      }
        |    }
        |
        |    implicit class ${javaTypeClassName(rosettaName)}(javaObject: $javaTypeName) {
        |      def asScalaResolvingReferences(
        |        implicit validator: RosettaTypeValidator
        |      ): Try[$scalaTypeName] = {
        |        val builder = javaObject.toBuilder
        |        new ReferenceResolverProcessStep(CdmReferenceConfig.get()).runProcessStep(classOf[$javaTypeName], builder)
        |        val $javaObjectName = builder.build
        |        for {
        |$forComprehensionTerms
        |        } yield Default$scalaTypeName($escapedAttributeNames)
        |      }
        |
        |      def asScala(
        |        implicit validator: RosettaTypeValidator
        |      ): Try[$scalaTypeName] = {
        |        val $javaObjectName = javaObject
        |        for {
        |$forComprehensionTerms
        |        } yield Default$scalaTypeName($escapedAttributeNames)
        |      }
        |    }
        |
        |${generateImplicitConverter(rosettaName, javaTypeName, scalaTypeName)}
        |  }
        |""".stripMargin
  }

  private def generateImplicitConverter(rosettaName: String, javaTypeName: String, scalaTypeName: String): String =
    s"""    implicit def ${toLowerFirst(rosettaName)}Converter(
       |        implicit validator: RosettaTypeValidator
       |    ): $javaTypeName => Try[$scalaTypeName] =
       |      x => {
       |        new ${javaTypeClassName(rosettaName)}(x).asScalaResolvingReferences
       |      }""".stripMargin

  private def getAllAnnotationRefs(e: Data): Iterable[AnnotationRef] = {
    Option(e.getSuperType) match {
      case Some(superType) => getAllAnnotationRefs(superType) ++ e.getAnnotations.asScala
      case None => e.getAnnotations.asScala
    }
  }

  private def generateMetaMutator(annotationRefs: Iterable[AnnotationRef]) = {
    val metadataAnnotations =
      annotationRefs.filter(_.getAnnotation.getName == "metadata")
    val metaTypeOpt =
      if (metadataAnnotations.exists(_.getAttribute.getName == "template"))
        Some("MetaAndTemplateFields")
      else if (metadataAnnotations.nonEmpty)
        Some("MetaFields")
      else
        None
    metaTypeOpt.map { metaType =>
      s"""        builder.setMeta(
         |          com.rosetta.model.metafields.$metaType.builder
         |            .addKey(Key.builder
         |              .setScope("GLOBAL")
         |              .setKeyValue(UUID.randomUUID.toString)
         |            )
         |        )
         |""".stripMargin
    }.getOrElse("")
  }

  private def generateCaseClass(e: Data): String = {
    val scalaTypeName = e.getName
    val javaTypeName = rosettaTypeToJavaType(e)
    val attributes = getAllAttributes(e)
    val classComment =
      if (attributes.flatMap(a => Option(a.getDefinition)).isEmpty)
        generateOptionalComment(e, "  ")
      else
        generatePartialClassComment(e, "  ") +
          generateParamComments(attributes, "  ") +
          "\n    */\n"
    val fields = generateCaseClassFields(attributes)
    val extending = generateExtendsClauseFromStrings(List(scalaTypeName))
    val javaFacade =
      s"$scalaTypeName.${scalaTypeClassName(scalaTypeName)}(this).asJava"
    s"""$classComment  final case class Default$scalaTypeName($fields)(
        |    implicit validator: RosettaTypeValidator
        |  )$extending {
        |    private val validation =
        |      validator.runProcessStep(classOf[$javaTypeName], $javaFacade)
        |    if (!validation.success)
        |      throw new IllegalStateException(
        |        validation.validationFailures.asScala.mkString("; ")
        |      )
        |  }
        |""".stripMargin
  }

  private def generateForComprehensionTerm(
      a: Attribute,
      ref: String
  ): String = {
    val name = a.getName
    val accessor = s"$ref.get${toUpperFirst(name)}"
    val conversion =
      convertRosettaAttributeFromJavaToScalaTry(
        a,
        nameOpt = Some(accessor)
      )
    s"          ${escapeReservedWords(name)} <- $conversion"
  }

  private def generateBuilderMutator(scalaObject: String)(a: Attribute): String = {
    val name = a.getName
    val field = s"$scalaObject.${escapeReservedWords(name)}"
    val mutator = s"set${toUpperFirst(name)}"
    val conversion =
      convertRosettaAttributeFromScalaToJava(
        a,
        couldBeMeta = true,
        couldBeOption = true,
        nameOpt = Some(field)
      )
    s"        builder.$mutator($conversion)"
  }

  private def generateTraitFields(attributes: Iterable[Attribute]): String = {
    val fields = attributes.map { attr =>
      val comment = generateOptionalComment(attr, "    ")
      val name = escapeReservedWords(attr.getName)
      s"$comment    def $name: ${rosettaAttrToScalaType(attr)}"
    }
    if (fields.isEmpty) "" else fields.mkString("\n", "\n", "\n")
  }

  private def generateCaseClassFields(attributes: Iterable[Attribute]): String = {
    val fields = attributes.map { a =>
      s"    ${escapeReservedWords(a.getName)}: ${rosettaAttrToScalaType(a)}"
    }
    if (fields.isEmpty) "" else fields.mkString("\n", ",\n", "\n  ")
  }

  private def escapeReservedWords(s: String): String = s match {
    case "val" => "`val`"
    case safe  => safe
  }
}
object ScalaTypeGenerator {
  def derivePackageName(e: Data): String =
    s"${AbstractScalaGenerator.deriveParentPackage(e)}.types"
}
