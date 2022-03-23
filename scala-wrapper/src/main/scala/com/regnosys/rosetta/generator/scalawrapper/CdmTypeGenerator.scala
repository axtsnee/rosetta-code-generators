package com.regnosys.rosetta.generator.scalawrapper

import com.regnosys.rosetta.rosetta.RosettaType
import com.regnosys.rosetta.rosetta.simple.{Attribute, Data}

import scala.jdk.CollectionConverters._

object CdmTypeGenerator {
  def generate(enclosingTypes: Iterable[RosettaType]): CdmType => Vector[String] = c => {
    val allSuperTypes = getAllSuperTypes(c.element, enclosingTypes)
    if (c.shouldBeSumType)
      Vector(generateSealedTrait(c.element, allSuperTypes, c.element.getAttributes.asScala))
    else
      Vector(generateTrait(c.element, allSuperTypes), generateCaseClass(c.element))
  }

  private def generateTrait(e: Data, allSuperTypes: List[RosettaType]): String = {
    val name = e.getName
    val traitComment = Option(e.getDefinition) match {
      case Some(defn) => s"/** $defn */\n"
      case None => ""
    }
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
    val attributes = e.getAttributes.asScala
    val fieldComments = paramComments(attributes)
    val fields = generateCaseClassFields(attributes)
    val extending = generateExtendsClauseFromStrings(List(name))
    s"""$classComment$fieldComments
       |  */
       |final case class Default$name($fields)$extending
       |""".stripMargin
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
      val comment = Option(attr.getDefinition) match {
        case Some(defn) => s"  /** $defn */\n"
        case None => ""
      }
       s"$comment  def ${attr.getName}: ${rosettaAttrToScalaType(attr)}"
    }
    if (fields.isEmpty) "" else fields.mkString("\n", "\n", "\n")
  }

  private def generateCaseClassFields(attributes: Iterable[Attribute]): String = {
    val fields = attributes.map(a => s"  ${a.getName}: ${rosettaAttrToScalaType(a)}")
    if (fields.isEmpty) "" else fields.mkString("\n", s",\n", "\n")
  }
}
