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
    if (shouldBeSumType(e))
      generateSealedTrait(e, allSuperTypes)
    else
      generateTrait(e, allSuperTypes) + "\n" + generateCaseClass(e)
  }

  private def shouldBeSumType(e: Data): Boolean = {
    val attrs = e.getAttributes.asScala
    getInheritedAttributes(e).isEmpty &&
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

  private def generateSealedTrait(e: Data, superTypes: Iterable[RosettaType]): String = {
    val name = e.getName
    val traitComment = generateOptionalComment(e)
    val extending = generateExtendsClauseFromTypes(superTypes)
    val javaName = rosettaTypeToJavaType(e)
    val attrs = e.getAttributes.asScala
    val javaObject = "x"
    val pattern =
      attrs
        .map(a => s"$javaObject.get${toUpperFirst(a.getName)}")
        .mkString("(Option(", ")).orElse(Option(", "))")
    val caseStmts = attrs.map { a =>
      val typeName = a.getType.getName
      s"""      case Some($javaObject: ${rosettaTypeToJavaType(a.getType)}) =>
         |        ${convertRosettaTypeFromJavaToScala(typeName, javaObject, s"$typeName.fromJava")}
         |""".stripMargin
    }
    s"""${traitComment}sealed trait $name$extending
       |object $name {
       |  def toJava(t: $name): $javaName = {
       |    val builder = new $javaName.${name}BuilderImpl()
       |    t match {
       |${generateCaseStmtForBuilder(attrs).mkString("\n")}
       |    }
       |    builder.build
       |  }
       |
       |  def fromJava($javaObject: ${rosettaTypeToJavaType(e)}): $name =
       |    $pattern match {
       |${caseStmts.mkString}
       |    }
       |}
       |""".stripMargin
  }

  private def generateCaseStmtForBuilder(attrs: Iterable[Attribute]): Iterable[String] = {
    val patMatchVar = "x"
    attrs.map { a =>
      val scalaType = rosettaTypeToScalaType(a.getType)
      val setter = generateBuilderStmtForObj(a, scalaType, patMatchVar, "")
      s"      case $patMatchVar: $scalaType => builder$setter"
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
    val scalaObject = "x"
    val fromJavaFields = allAttrs.map(a => generateJavaObjectAccessor(a, scalaObject)).mkString(", ")
    val builderStatements = allAttrs.map(a => generateBuilderStmtForAttr(a, scalaObject, "      ")).mkString("\n")
    s"""${traitComment}trait $name$extending {$fields}
        |object $name {
        |  def toJava($scalaObject: $name): $javaName = $scalaObject match {
        |    case d: Default$name => d.toJava
        |    case _ => buildJava($scalaObject)
        |  }
        |
        |  def fromJava($scalaObject: ${rosettaTypeToJavaType(e)}): $name =
        |    Default$name($fromJavaFields)
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
    s"""${classComment}final case class Default$name($fields)$extending {
        |  private[types] val toJava: ${rosettaTypeToJavaType(e)} =
        |    $name.buildJava(this)
        |}
        |
        |""".stripMargin
  }

  private def generateJavaObjectAccessor(a: Attribute, ref: String): String =
    convertRosettaAttributeFromJavaToScala(a, Some(s"$ref.get${toUpperFirst(a.getName)}"))

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
