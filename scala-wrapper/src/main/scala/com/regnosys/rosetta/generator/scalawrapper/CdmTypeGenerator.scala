package com.regnosys.rosetta.generator.scalawrapper

import scala.jdk.CollectionConverters._

import com.regnosys.rosetta.generator.util.RosettaAttributeExtensions
import com.regnosys.rosetta.rosetta.{RosettaDefinable, RosettaType}
import com.regnosys.rosetta.rosetta.simple.{Attribute, Data}
import GeneratorFunctions._

object CdmTypeGenerator {
  def generate(enclosingTypes: Iterable[RosettaType]): Data => String = e => {
    val allSuperTypes = mixSuperTypeWithEnclosingTypes(e, enclosingTypes)
    if (shouldBeSumType(e))
      generateSealedTrait(e, allSuperTypes)
    else
      generateTrait(e, allSuperTypes) + generateCaseClass(e)
  }

  def shouldBeSumType(e: Data): Boolean =
    getInheritedAttributes(e).isEmpty &&
      e.getConditions.asScala.exists(c => Option(c.getConstraint).exists(_.isOneOf)) &&
      RosettaAttributeExtensions.getExpandedAttributes(e).asScala.forall(_.isSingleOptional)

  private def generateSealedTrait(r: RosettaType with RosettaDefinable, superTypes: Iterable[RosettaType]): String = {
    val comment = makeOptionalComment(r)
    s"""${comment}sealed trait ${r.getName}${generateExtendsClauseFromTypes(superTypes)}\n"""
  }

  private def generateTrait(e: Data, allSuperTypes: List[RosettaType]): String = {
    val name = e.getName
    val traitComment = makeOptionalComment(e)
    val fields = generateTraitFields(e.getAttributes.asScala)
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
       |
       |""".stripMargin
  }

  private def getInheritedAttributes(e: Data): Vector[Attribute] = {
    def loop(dOpt: Option[Data]): Vector[Attribute] = dOpt match {
      case Some(d) => loop(Option(d.getSuperType)) ++ d.getAttributes.asScala
      case None => Vector.empty
    }
    loop(Option(e.getSuperType))
  }

  private def paramComments(attributes: Iterable[Attribute]): String = {
    val comments =
      for {
        attr <- attributes
        defnOpt = Option(attr.getDefinition)
        if defnOpt.isDefined && defnOpt.forall(_.nonEmpty)
      } yield s"""  * @param ${attr.getName} ${defnOpt.getOrElse("")}"""
    comments.mkString("\n")
  }

  private def generateTraitFields(attributes: Iterable[Attribute]): String = {
    val fields = attributes.map { attr =>
      val comment = makeOptionalComment(attr, "  ")
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
