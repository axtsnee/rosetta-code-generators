package com.regnosys.rosetta.generator.scalawrapper

import scala.jdk.CollectionConverters._
import com.regnosys.rosetta.generator.scalawrapper.GeneratorFunctions._
import com.regnosys.rosetta.generator.util.RosettaAttributeExtensions
import com.regnosys.rosetta.rosetta.{RosettaNamed, RosettaRootElement, RosettaType}
import com.regnosys.rosetta.rosetta.simple.Function

case class CdmFunctionGenerator(analysis: RootElementAnalyzer) extends AbstractCdmGenerator(analysis.functions) {
  override val dependencies: Function => List[RosettaType] =
    analysis.functions.foldLeft(Map.empty[Function, List[RosettaType]]) {
      case (acc, e: Function) =>
        (Option(e.getOutput).toList ++ e.getInputs.asScala).foldLeft(acc)((acc, attr) => {
          val attrType = attr.getType
          if (RosettaAttributeExtensions.toExpandedType(attrType).isBuiltInType) acc
          else
            acc.updatedWith(e) {
              case Some(list) => Some(attrType :: list)
              case None       => Some(attrType :: Nil)
            }
        })
    }.withDefaultValue(Nil)

  override val derivePackageName: Function => String = CdmFunctionGenerator.derivePackageName

  override val translate: Function => String = e => {
    Option(e.getOutput) match {
      case None => ""  // TODO Figure out if this is recoverable.
      case Some(output) =>
        val params = e.getInputs.asScala
        val classComment =
          if (params.flatMap(a => Option(a.getDefinition)).isEmpty)
            generateOptionalComment(e)
          else
            generatePartialClassComment(e) + generateParamComments(params) + "\n  */\n"
        val paramNames = params.map(_.getName).mkString(", ")
        val paramDecls = params.map { p =>
          s"${p.getName}: ${rosettaAttrToScalaType(p)}"
        }
        s"""object ${e.getName} {
           |$classComment  def apply(${paramDecls.mkString(", ")}): ${rosettaTypeToScalaType(output.getType)} =
           |    new ${e.getModel.getName}.functions.${e.getName}.${e.getName}Default().evaluate($paramNames)
           |}
           |""".stripMargin
    }
  }
}
object CdmFunctionGenerator {
  def derivePackageName(e: Function): String =
    s"${AbstractCdmGenerator.basePkg}.${e.getModel.getName}.functions"
}
