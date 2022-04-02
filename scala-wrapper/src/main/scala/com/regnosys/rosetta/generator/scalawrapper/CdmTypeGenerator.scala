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
            if (RosettaAttributeExtensions.toExpandedType(attrType).isBuiltInType)
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

  private def shouldBeSumType(e: Data): Boolean =
    getInheritedAttributes(e).isEmpty &&
      e.getConditions.asScala.exists(c => Option(c.getConstraint).exists(_.isOneOf)) &&
      e.getAttributes.asScala.forall(isSingleOptional)

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
    s"""${traitComment}sealed trait $name$extending
       |object $name {
       |  def toJava(x: $name): $javaName = {
       |    val builder = new $javaName.${name}BuilderImpl()
       |    //x match {
       |    //  case y: Y => builder.setTheYValue(Y.toJava(y))
       |    //}
       |    builder.build
       |  }
       |
       |  def fromJava(x: ${rosettaTypeToJavaType(e)}): $name = x match {
       |    case _ => ???
       |  }
       |}
       |""".stripMargin
  }

  private def generateTrait(e: Data, allSuperTypes: List[RosettaType]): String = {
    val name = e.getName
    val attrs = e.getAttributes.asScala
    val traitComment = generateOptionalComment(e)
    val fields = generateTraitFields(attrs)
    val extending = generateExtendsClauseFromTypes(allSuperTypes)
    val javaName = rosettaTypeToJavaType(e)
    val allAttrs = getInheritedAttributes(e) ++ attrs
    val fromJavaFields = allAttrs.map(generateJavaObjectAccessor).mkString(", ")
    val builderStatements = allAttrs.map(generateBuildStatement).mkString("\n")
    s"""${traitComment}trait $name$extending {$fields}
        |object $name {
        |  def toJava(x: $name): $javaName = x match {
        |    case d: Default$name => d.toJava
        |    case _ => buildJava(x)
        |  }
        |
        |  def fromJava(x: ${rosettaTypeToJavaType(e)}): $name =
        |    Default$name($fromJavaFields)
        |
        |  private[types] def buildJava(x: $name): $javaName =
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

  private def generateJavaObjectAccessor(a: Attribute): String =
    convertJavaAttributeToScala(a, Some("x.get" + toUpperFirst(a.getName)))

  private def generateBuildStatement(a: Attribute): String = {
    val name = a.getName
    val value = if (hasMetadataAnnotation(a)) "Value" else ""
    val mutator = s".set${toUpperFirst(name)}$value"
    val convertedAttribute = convertScalaAttributeToJava(a, Some("x." + escapeReservedWords(name)))
    s"      $mutator($convertedAttribute)"
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
