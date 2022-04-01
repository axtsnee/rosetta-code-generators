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
      RosettaAttributeExtensions.getExpandedAttributes(e).asScala.forall(_.isSingleOptional)

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
    s"""${traitComment}sealed trait $name$extending
       |${generateCompanionObject(e)}
       |""".stripMargin
  }

  private def generateTrait(e: Data, allSuperTypes: List[RosettaType]): String = {
    val name = e.getName
    val traitComment = generateOptionalComment(e)
    val fields = generateTraitFields(e.getAttributes.asScala)
    val extending = generateExtendsClauseFromTypes(allSuperTypes)
    s"""${traitComment}trait $name$extending {$fields}
        |${generateCompanionObject(e)}
        |""".stripMargin
  }

  private def generateCompanionObject(e: Data): String = {
    val name = e.getName
    val javaName = rosettaTypeToJavaType(e)
    s"""object $name {
       |  def toJava(x: $name): $javaName =
       |    new $javaName.${name}BuilderImpl()
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
    s"""${classComment}final case class Default$name($fields)$extending
        |object Default$name {
        |  def fromJava(x: ${rosettaTypeToJavaType(e)}): Default$name = ???
        |}
        |
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
    if (fields.isEmpty) "" else fields.mkString("\n", s",\n", "\n")
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
