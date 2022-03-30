package com.regnosys.rosetta.generator.scalawrapper

import com.regnosys.rosetta.rosetta.{RosettaMetaType, RosettaNamed, RosettaRootElement}
import GeneratorFunctions._

case class CdmMetaTypeGenerator(analysis: RootElementAnalyzer) extends AbstractCdmGenerator(analysis.metaTypes, analysis.nsToPkgs) {

  override val dependencies: RosettaMetaType => Iterable[RosettaRootElement with RosettaNamed] = _ => Nil

  override val derivePackageName: RosettaMetaType => String = CdmMetaTypeGenerator.derivePackageName

  override val translate: RosettaMetaType => String = e => {
    s"""class Unimplemented_${e.getName} {
       |  val value: ${rosettaTypeToScalaType(e.getType)} = ???
       |}
       |""".stripMargin
  }
}
object CdmMetaTypeGenerator {
  def derivePackageName(e: RosettaMetaType): String =
    s"${AbstractCdmGenerator.basePkg}.${e.getModel.getName}.metas"
}
