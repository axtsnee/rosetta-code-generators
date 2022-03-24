package com.regnosys.rosetta.generator.scalawrapper

import scala.jdk.CollectionConverters._

import com.regnosys.rosetta.generator.util.RosettaAttributeExtensions
import com.regnosys.rosetta.rosetta.{RosettaDefinable, RosettaType}
import com.regnosys.rosetta.rosetta.simple.{Attribute, Data}

object CdmTypeGenerator extends CdmGenerator {
  def generate(enclosingTypes: Iterable[RosettaType]): CdmType => Vector[String] = c => {
    val allSuperTypes = mixSuperTypeWithEnclosingTypes(c.element, enclosingTypes)
    if (shouldBeSumType(c.element))
      Vector(generateSealedTrait(c.element, allSuperTypes, c.element.getAttributes.asScala))
    else
      Vector(generateTrait(c.element, allSuperTypes), generateCaseClass(c.element))
  }

  def shouldBeSumType(e: Data): Boolean =
    getInheritedAttributes(e).isEmpty &&
      e.getConditions.asScala.exists(_.getConstraint.isOneOf) &&
      RosettaAttributeExtensions.getExpandedAttributes(e).asScala.forall(_.isSingleOptional)

  private def generateSealedTrait(r: RosettaType with RosettaDefinable, superTypes: Iterable[RosettaType], attributes: Iterable[Attribute]): String = {
    val comment = makeOptionalComment(r)
    s"""${comment}sealed trait ${r.getName}${generateExtendsClauseFromTypes(superTypes)}\n"""
  }

  private def generateTrait(e: Data, allSuperTypes: List[RosettaType]): String = {
    val name = e.getName
    val traitComment = makeOptionalComment(e)
    val attributes = e.getAttributes.asScala
    val fields = generateTraitFields(attributes)
    val extending = generateExtendsClauseFromTypes(allSuperTypes)
    s"${traitComment}trait $name$extending {$fields}\n"
  }

  private def generateCaseClass(e: Data): String = {
    val name = e.getName
    val classComment = Option(e.getDefinition) match {
      case Some(defn) => s"""/** $defn
                            |  *
                            |""".stripMargin
      case None => "/**\n"
    }
    val attributes = getInheritedAttributes(e) ++ e.getAttributes.asScala.toVector
    val fieldComments = paramComments(attributes)
    val fields = generateCaseClassFields(attributes)
    val extending = generateExtendsClauseFromStrings(List(name))
    s"""$classComment$fieldComments
       |  */
       |final case class Default$name($fields)$extending
       |""".stripMargin
  }

  private def getInheritedAttributes(e: Data): Vector[Attribute] = {
    def loop(superTypeOpt: Option[Data]): Vector[Attribute] = superTypeOpt match {
      case Some(superType) => loop(Option(superType.getSuperType)) ++ superType.getAttributes.asScala
      case None => Vector.empty
    }
    loop(Option(e.getSuperType))
  }

  private def paramComments(attributes: Iterable[Attribute]): String = {
    val comments =
      for {
        attr <- attributes
        defnOpt = Option(attr.getDefinition)
        if defnOpt.isDefined
      } yield s"""  * @param ${attr.getName} ${defnOpt.getOrElse("")}"""
    comments.mkString("\n")
  }

  private def generateTraitFields(attributes: Iterable[Attribute]): String = {
    val fields = attributes.map { attr =>
      val comment = "  " + makeOptionalComment(attr)
       s"$comment  def ${attr.getName}: ${rosettaAttrToScalaType(attr)}"
    }
    if (fields.isEmpty) "" else fields.mkString("\n", "\n", "\n")
  }

  private def generateCaseClassFields(attributes: Iterable[Attribute]): String = {
    val fields = attributes.map(a => s"  ${a.getName}: ${rosettaAttrToScalaType(a)}")
    if (fields.isEmpty) "" else fields.mkString("\n", s",\n", "\n")
  }
}
