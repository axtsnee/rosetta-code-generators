package com.regnosys.rosetta.generator.scalawrapper

import scala.jdk.CollectionConverters._

import com.regnosys.rosetta.generator.scalawrapper.GeneratorFunctions._
import com.regnosys.rosetta.generator.util.RosettaAttributeExtensions
import com.regnosys.rosetta.rosetta.{RosettaRootElement, RosettaType}
import com.regnosys.rosetta.rosetta.simple.{Attribute, Function}

case class CdmFunctionGenerator(analysis: RootElementAnalyzer) extends AbstractCdmGenerator(analysis.functions) {
  private val implicitConverter = "convertToScala"

  override val dependencies: Function => Set[RosettaType] =
    analysis.functions.foldLeft(Map.empty[Function, Set[RosettaType]]) {
      case (acc, e: Function) =>
        (Option(e.getOutput).toList ++ e.getInputs.asScala).foldLeft(acc)((acc, attr) => {
          val attrType = attr.getType
          if (RosettaAttributeExtensions.isBuiltInType(attrType))
            acc
          else
            acc.updatedWith(e) {
              case Some(set) => Some(set + attrType)
              case None      => Some(Set(attrType))
            }
        })
    }.withDefaultValue(Set.empty)

  override val derivePackageName: Function => String = CdmFunctionGenerator.derivePackageName

  override val translate: Function => String = e => {
    Option(e.getOutput) match {
      case None => ""
      case Some(output) =>
        val implicits = generateImplicitParams(output)
        val params = e.getInputs.asScala
        val comment =
          if (params.flatMap(a => Option(a.getDefinition)).isEmpty)
            generateOptionalComment(e)
          else
            generatePartialClassComment(e) + generateParamComments(params) + generateOutputComment(output) + "\n  */\n"
        val paramDecls = params.map { p =>
          s"${p.getName}: ${rosettaAttrToScalaType(p)}"
        }
        val importStmt = generateImportStatement(output)
        val javaFunctionCall = generateJavaFunctionCall(e, params, output)
        s"""object ${e.getName} {
           |$importStmt$comment  def apply(${paramDecls.mkString(", ")})$implicits: ${rosettaAttrToScalaType(output)} =
           |    $javaFunctionCall
           |}
           |""".stripMargin
    }
  }

  private def generateImplicitParams(output: Attribute): String = {
    val shortName = output.getType.getName
    shortName match {
      case "string" | "int" | "boolean" | "time" | "dateTime" | "zonedDateTime" | "date" | "number" => ""
      case "productType" | "eventType" | "calculation" => ""
      case _ => s"(implicit $implicitConverter: ${rosettaTypeToJavaType(output.getType)} => $shortName)"
    }
  }

  private def generateOutputComment(output: Attribute): String =
    Option(output.getDefinition).map(d => s"\n  * @return $d").getOrElse("")

  private def generateImportStatement(output: Attribute): String =
    output.getType match {
      case x if RosettaAttributeExtensions.isBuiltInType(x) => ""
      case e: RosettaRootElement => s"  import ${AbstractCdmGenerator.deriveAnyPackageName(e)}.${e.getName}._\n"
      case _ => ""
    }

  private val specialCases = Set("ComputeCalculationPeriod", "DayCountBasis", "YearFraction", "ProcessFloatingRateReset")

  private def generateJavaFunctionCall(
      e: Function,
      params: Iterable[Attribute],
      output: Attribute
  ): String = {
    val javaClass =
      if (specialCases.contains(e.getName))
        s"${e.getModel.getName}.functions.${e.getName}"
      else
        s"${e.getModel.getName}.functions.${e.getName}.${e.getName}Default"
    val paramConversions =
      params
        .map(p => convertRosettaAttributeFromScalaToJava(p))
        .mkString(", ")
    val javaFunctionCall = s"new $javaClass().evaluate($paramConversions)"
    convertRosettaAttributeFromJavaToScala(output, Some(javaFunctionCall), Some(implicitConverter))
  }
}
object CdmFunctionGenerator {
  def derivePackageName(e: Function): String =
    s"${AbstractCdmGenerator.basePkg}.${e.getModel.getName}.functions"
}
