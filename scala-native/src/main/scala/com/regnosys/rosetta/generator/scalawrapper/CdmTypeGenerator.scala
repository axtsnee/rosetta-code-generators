package com.regnosys.rosetta.generator.scalawrapper

import scala.jdk.CollectionConverters._

import com.regnosys.rosetta.generator.util.RosettaAttributeExtensions
import com.regnosys.rosetta.rosetta.RosettaType
import com.regnosys.rosetta.rosetta.simple.{Attribute, Data}
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
        (getInheritedAttributes(e) ++ e.getAttributes.asScala)
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
      if ((getInheritedAttributes(e) ++ e.getAttributes.asScala).isEmpty) {
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
      val toJavaStmts = subTypes.map(t => s"        case ja: ${t.getName} => new ${t.getName}.JavaConverter(ja).asJava").mkString("\n")
      val fromJavaStmts = subTypes.map(t => s"        case sc: ${rosettaTypeToJavaType(t)} => new ${t.getName}.ScalaConverter(sc).asScala").mkString("\n")
      s"""$sealedTrait
         |object $scalaTypeName {
         |  implicit class JavaConverter(t: $scalaTypeName) {
         |    def asJava: $javaTypeName =
         |      t match {
         |$toJavaStmts
         |      }
         |  }
         |
         |  implicit class ScalaConverter(t: $javaTypeName) {
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
    val name = e.getName
    val traitComment = generateOptionalComment(e)
    val extending = generateExtendsClauseFromTypes(superTypes)
    val javaName = rosettaTypeToJavaType(e)
    implicit val descendantsBeforeAncestors: Ordering[Attribute] = (a, b) => {
      (a.getType, b.getType) match {
        case (ta: Data, tb: Data) =>
          if (getAllAncestors(ta).contains(tb)) -1
          else if (getAllAncestors(tb).contains(ta)) +1
          else 0
        case (_, _) => 0
      }
    }
    val attrs = e.getAttributes.asScala.sorted
    val javaObject = "x"
    val pattern =
      attrs
        .map(a => s"$javaObject.get${toUpperFirst(a.getName)}")
        .mkString("(Option(", ")).orElse(Option(", "))")
    val caseStmts = attrs.map { a =>
      val typeName = a.getType.getName
      s"""        case Some($javaObject: ${rosettaTypeToJavaType(a.getType)}) =>
         |          ${convertRosettaTypeFromJavaToScalaTry(typeName, javaObject, (x: String) => s"new $typeName.ScalaConverter($x).asScala")}
         |""".stripMargin
    }
    s"""${traitComment}sealed trait $name$extending
       |object $name {
       |  implicit class JavaConverter(t: $name) {
       |    def asJava: $javaName = {
       |      val builder = new $javaName.${name}BuilderImpl()
       |      t match {
       |${generateCaseStmtForBuilder(attrs).mkString("\n")}
       |      }
       |      builder.build
       |    }
       |  }
       |
       |  implicit class ScalaConverter($javaObject: $javaName) {
       |    def asScala(implicit validator: RosettaTypeValidator): Try[$name] =
       |      $pattern match {
       |${caseStmts.mkString}
       |      }
       |  }
       |}
       |""".stripMargin
  }

  private def generateCaseStmtForBuilder(attrs: Iterable[Attribute]): Iterable[String] = {
    val patMatchVar = "x"
    attrs.map { a =>
      val scalaType = rosettaTypeToScalaType(a.getType)
      val setter = generateBuilderStmtForObj(a, scalaType, patMatchVar, "")
      s"        case $patMatchVar: $scalaType => builder$setter"
    }
  }

  private def generateTrait(e: Data, allSuperTypes: List[RosettaType]): String = {
    val name = e.getName
    val attrs = e.getAttributes.asScala
    val traitComment = generateOptionalComment(e)
    val fields = generateTraitFields(attrs)
    val extending = generateExtendsClauseFromTypes(allSuperTypes)
    val javaName = rosettaTypeToJavaType(e)
    val allAttrs = getInheritedAttributes(e) ++ attrs
    val scalaObject = "a"
    val javaObject = "b"
    val forStatements = allAttrs.map(a => generateJavaObjectAccessor(a, javaObject)).mkString("\n")
    val builderStatements = allAttrs.map(a => generateBuilderStmtForAttr(a, scalaObject, "        ")).mkString("\n")
    val escapedAttributeNames = allAttrs.map(n => escapeReservedWords(n.getName)).mkString(", ")
    s"""${traitComment}trait $name$extending {$fields}
        |object $name {
        |  implicit class JavaConverter($scalaObject: $name) {
        |    def asJava: $javaName = $scalaObject match {
        |      case d: Default$name => d.toJava
        |      case _ => buildJava($scalaObject)
        |    }
        |  }
        |
        |  implicit class ScalaConverter($javaObject: ${rosettaTypeToJavaType(e)}) {
        |    def asScala(implicit validator: RosettaTypeValidator): Try[$name] =
        |      for {
        |$forStatements
        |      } yield Default$name($escapedAttributeNames)
        |  }
        |
        |  private[types] def buildJava($scalaObject: $name): $javaName =
        |    new $javaName.${name}BuilderImpl()
        |$builderStatements
        |      .build
        |}
        |""".stripMargin
  }

  private def generateCaseClass(e: Data): String = {
    val name = e.getName
    val attributes = getInheritedAttributes(e) ++ e.getAttributes.asScala.toVector
    val classComment =
      if (attributes.flatMap(a => Option(a.getDefinition)).isEmpty)
        generateOptionalComment(e)
      else
        generatePartialClassComment(e) + generateParamComments(attributes) + "\n  */\n"
    val fields = generateCaseClassFields(attributes)
    val extending = generateExtendsClauseFromStrings(List(name))
    s"""${classComment}final case class Default$name($fields)(implicit validator: RosettaTypeValidator)$extending {
        |  private[types] val toJava: ${rosettaTypeToJavaType(e)} = {
        |    val unvalidated = $name.buildJava(this)
        |    val validation = validator.runProcessStep(unvalidated.getClass, unvalidated.toBuilder)
        |    if (validation.success) unvalidated
        |    else throw new IllegalStateException(validation.validationFailures.asScala.mkString("; "))
        |  }
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

  private def generateBuilderStmtForAttr(a: Attribute, ref: String, indent: String): String =
    generateBuilderStmt(a, convertRosettaAttributeFromScalaToJava(a, Some(s"$ref.${escapeReservedWords(a.getName)}")), indent)

  private def generateBuilderStmtForObj(a: Attribute, scalaType: String, ref: String, indent: String): String =
    generateBuilderStmt(a, convertScalaTypeToJava(scalaType, ref), indent)

  private def generateBuilderStmt(a: Attribute, convertedAttribute: String, indent: String): String = {
    val name = a.getName
    val value = if (hasMetadataAnnotation(a)) "Value" else ""
    val mutator = s".set${toUpperFirst(name)}$value"
    s"$indent$mutator($convertedAttribute)"
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

  private def toUpperFirst(s: String): String =
    s.head.toUpper + s.tail
}
object CdmTypeGenerator {
  def derivePackageName(e: Data): String =
    s"${AbstractCdmGenerator.basePkg}.${e.getModel.getName}.types"
}
