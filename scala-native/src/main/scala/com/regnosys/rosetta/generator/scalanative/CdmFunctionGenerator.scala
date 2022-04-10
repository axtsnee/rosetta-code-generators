package com.regnosys.rosetta.generator.scalanative

import scala.jdk.CollectionConverters._

import GeneratorFunctions._
import com.regnosys.rosetta.generator.util.RosettaAttributeExtensions
import com.regnosys.rosetta.rosetta.RosettaType
import com.regnosys.rosetta.rosetta.simple.{Attribute, Function}

case class CdmFunctionGenerator(analysis: RootElementAnalyzer) extends AbstractCdmGenerator(analysis.functions) {
  private val converterParmName = "convertToScala"

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
            generateOptionalComment(e, "  ")
          else
            generatePartialClassComment(e, "  ") +
              generateParamComments(params, "  ") +
              generateOutputComment(output) + "\n    */\n"
        val paramDecls = params.map { p =>
          s"${p.getName}: ${rosettaAttrToScalaType(p)}"
        }
        val javaFunctionCall = generateJavaFunctionCall(e, params)
        val convertedOutputType = rosettaAttrToScalaType(output)
        if (RosettaAttributeExtensions.isBuiltInType(output.getType))
          generateFunctionCall(e.getName, comment, paramDecls, output, extraImplicits, convertedOutputType, javaFunctionCall)
        else
          generateValidatingFunctionCall(e.getName, comment, paramDecls, output, extraImplicits, convertedOutputType, javaFunctionCall)
    }
  }

  private def generateFunctionCall(
      functionName: String,
      comment: String,
      paramDecls: Iterable[String],
      output: Attribute,
      extraImplicits: String,
      convertedOutputType: String,
      javaFunctionCall: String
  ): String = {
    val convertResult =
      convertRosettaAttributeFromJavaToScalaTry(
        output,
        nameOpt = Some(javaFunctionCall),
        customConverterOpt = Some((x: String) => s"$converterParmName($x)")
      )
    s"""$comment  object $functionName {
       |    def apply(
       |      ${paramDecls.mkString(", ")}
       |    )(
       |      implicit injector: Injector$extraImplicits
       |    ): Try[$convertedOutputType] =
       |      $convertResult
       |  }
       |""".stripMargin
  }

  private def generateValidatingFunctionCall(
      functionName: String,
      comment: String,
      paramDecls: Iterable[String],
      output: Attribute,
      extraImplicits: String,
      convertedOutputType: String,
      javaFunctionCall: String
  ): String = {
    val convertResult =
      convertRosettaAttributeFromJavaToScalaTry(
        output,
        nameOpt = Some("result"),
        customConverterOpt = Some((x: String) => s"$converterParmName($x)")
      )
    s"""$comment  object $functionName {
       |    def apply(
       |      ${paramDecls.mkString(", ")}
       |    )(
       |      implicit injector: Injector, validate: ${rosettaAttrToJavaType(output)} => ValidationReport$extraImplicits
       |    ): Try[$convertedOutputType] =
       |      for {
       |        result <- Try($javaFunctionCall)
       |        validation = validate(result)
       |        returnValue <-
       |          if (validation.success) $convertResult
       |          else Failure(new IllegalStateException(validation.validationFailures.asScala.mkString("; ")))
       |      } yield returnValue
       |  }
       |""".stripMargin
  }

  private def generateImplicitParams(output: Attribute): String =
    output.getType.getName match {
      case "string" | "int" | "boolean" | "time" | "dateTime" | "zonedDateTime" | "date" | "number" => ""
      case "productType" | "eventType" | "calculation" => ""
      case enum if enum.endsWith("Enum") => ""
      case customType =>
        val fromType = rosettaTypeToJavaType(output.getType)
        s", $converterParmName: $fromType => Try[$customType]"
    }

  private def generateOutputComment(output: Attribute): String =
    Option(output.getDefinition).map(d => s"\n    * @return $d").getOrElse("")

  private def generateJavaFunctionCall(
      e: Function,
      params: Iterable[Attribute]
  ): String = {
    val javaClass = s"${e.getModel.getName}.functions.${e.getName}"
    val paramConversions =
      params
        .map(p => convertRosettaAttributeFromScalaToJava(p, couldBeMeta = false, couldBeOption = true))
        .mkString(", ")
    s"injector.getInstance(classOf[$javaClass]).evaluate($paramConversions)"
  }
}
object CdmFunctionGenerator {
  def derivePackageName(e: Function): String =
    s"${AbstractCdmGenerator.deriveParentPackage(e)}.functions"
}
