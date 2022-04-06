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
        val extraImplicits = generateImplicitParams(output)
        val params = e.getInputs.asScala
        val comment =
          if (params.flatMap(a => Option(a.getDefinition)).isEmpty)
            generateOptionalComment(e)
          else
            generatePartialClassComment(e) + generateParamComments(params) + generateOutputComment(output) + "\n  */\n"
        val paramDecls = params.map { p =>
          s"${p.getName}: ${rosettaAttrToScalaType(p)}"
        }
        val javaFunctionCall = generateJavaFunctionCall(e, params, output)
        val rawOutputType = rosettaAttrToJavaType(output)
        val convertedOutputType = rosettaAttrToScalaType(output)
        val convertResult = convertRosettaAttributeFromJavaToScalaTry(output, Some("result"), Some(implicitConverter))
        s"""${comment}object ${e.getName} {
           |  def apply(
           |    ${paramDecls.mkString(", ")}
           |  )(
           |    implicit injector: Injector, validate: $rawOutputType => ValidationReport$extraImplicits
           |  ): Try[$convertedOutputType] =
           |    for {
           |      result <- Try($javaFunctionCall)
           |      validation = validate(result)
           |      returnValue <-
           |        if (validation.success) $convertResult
           |        else Failure(new IllegalStateException(validation.validationFailures.asScala.mkString("; ")))
           |    } yield returnValue
           |}
           |""".stripMargin
    }
  }

  private def generateImplicitParams(output: Attribute): String = {
    output.getType.getName match {
      case "string" | "int" | "boolean" | "time" | "dateTime" | "zonedDateTime" | "date" | "number" => ""
      case "productType" | "eventType" | "calculation" => ""
      case customType =>
        val fromType = rosettaTypeToJavaType(output.getType)
        s", $implicitConverter: $fromType => Try[$customType]"
    }
  }

  private def generateOutputComment(output: Attribute): String =
    Option(output.getDefinition).map(d => s"\n  * @return $d").getOrElse("")

  private def generateJavaFunctionCall(
      e: Function,
      params: Iterable[Attribute],
      output: Attribute
  ): String = {
    val javaClass = s"${e.getModel.getName}.functions.${e.getName}"
    val paramConversions =
      params
        .map(p => convertRosettaAttributeFromScalaToJava(p))
        .mkString(", ")
    s"injector.getInstance(classOf[$javaClass]).evaluate($paramConversions)"
  }
}
object CdmFunctionGenerator {
  def derivePackageName(e: Function): String =
    s"${AbstractCdmGenerator.basePkg}.${e.getModel.getName}.functions"
}
