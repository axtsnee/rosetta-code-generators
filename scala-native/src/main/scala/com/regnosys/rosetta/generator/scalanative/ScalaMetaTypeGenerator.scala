package com.regnosys.rosetta.generator.scalanative

import com.regnosys.rosetta.rosetta.{RosettaMetaType, RosettaNamed, RosettaRootElement}
import GeneratorFunctions._

case class ScalaMetaTypeGenerator(analysis: RootElementAnalyzer)
  extends AbstractScalaGenerator(analysis.metaTypes) {

  override val dependencies: RosettaMetaType => Iterable[RosettaRootElement with RosettaNamed] =
    _ => Nil

  override val derivePackageName: RosettaMetaType => String =
    ScalaMetaTypeGenerator.derivePackageName

  override val translate: RosettaMetaType => String = e => {
    s"""  class Unimplemented_${e.getName} {
       |    def value: ${rosettaTypeToScalaType(e.getType)} = ???
       |  }
       |""".stripMargin
  }
}
object ScalaMetaTypeGenerator {
  def derivePackageName(e: RosettaMetaType): String =
    s"${AbstractScalaGenerator.deriveParentPackage(e)}.metas"
}
