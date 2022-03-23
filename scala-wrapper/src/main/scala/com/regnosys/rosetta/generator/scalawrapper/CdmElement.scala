package com.regnosys.rosetta.generator.scalawrapper

import scala.jdk.CollectionConverters._

import com.regnosys.rosetta.rosetta.{RosettaEnumeration, RosettaMetaType, RosettaRootElement, RosettaType}
import com.regnosys.rosetta.rosetta.simple.{Data, Function}

sealed trait CdmElement[T <: RosettaRootElement] {
  def element: T
  def fileName: String
  def generate(enclosingTypes: Map[RosettaType, List[RosettaType]]): Vector[String]
}

final case class CdmType(
                            override val element: Data,
                          ) extends CdmElement[Data] {
  override val fileName: String = CdmType.fileName

  override def generate(enclosingTypes: Map[RosettaType, List[RosettaType]]): Vector[String] =
    CdmTypeGenerator.generate(enclosingTypes(element))(this)

  def shouldBeSumType: Boolean = element.getConditions.asScala.exists(_.getConstraint.isOneOf)
}
object CdmType {
  val fileName: String = "CdmTypes.scala"
}

final case class CdmMetaType(
                           override val element: RosettaMetaType,
                         ) extends CdmElement[RosettaMetaType] {
  override val fileName: String = CdmMetaType.fileName

  override def generate(enclosingTypes: Map[RosettaType, List[RosettaType]]): Vector[String] =
    CdmMetaTypeGenerator.generate(this)
}
object CdmMetaType {
  val fileName: String = "CdmMetaTypes.scala"
}

final case class CdmEnumeration(
                          override val element: RosettaEnumeration,
                        ) extends CdmElement[RosettaEnumeration] {
  override val fileName: String = CdmEnumeration.fileName

  override def generate(enclosingTypes: Map[RosettaType, List[RosettaType]]): Vector[String] =
    CdmEnumerationGenerator.generate(enclosingTypes(element))(this)
}
object CdmEnumeration {
  val fileName: String = "CdmEnumerations.scala"
}

final case class CdmFunction(
                               override val element: Function,
                             ) extends CdmElement[Function] {
  override val fileName: String = CdmFunction.fileName

  override def generate(enclosingTypes: Map[RosettaType, List[RosettaType]]): Vector[String] =
    CdmFunctionGenerator.generate(this)
}
object CdmFunction {
  val fileName: String = "CdmFunctions.scala"
}

final case class Unknown(override val element: RosettaRootElement) extends CdmElement[RosettaRootElement] {
  override val fileName: String = Unknown.fileName

  override def generate(enclosingTypes: Map[RosettaType, List[RosettaType]]): Vector[String] =
    Vector(element.toString)
}
object Unknown {
  val fileName: String = "Unknown.txt"
}
