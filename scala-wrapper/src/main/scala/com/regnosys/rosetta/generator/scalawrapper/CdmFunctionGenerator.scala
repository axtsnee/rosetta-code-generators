package com.regnosys.rosetta.generator.scalawrapper

import scala.jdk.CollectionConverters._
import com.regnosys.rosetta.generator.scalawrapper.GeneratorFunctions._
import com.regnosys.rosetta.generator.util.RosettaAttributeExtensions
import com.regnosys.rosetta.rosetta.{RosettaRootElement, RosettaType}
import com.regnosys.rosetta.rosetta.simple.{Attribute, Function}

case class CdmFunctionGenerator(analysis: RootElementAnalyzer) extends AbstractCdmGenerator(analysis.functions) {
  override val dependencies: Function => Set[RosettaType] =
    analysis.functions.foldLeft(Map.empty[Function, Set[RosettaType]]) {
      case (acc, e: Function) =>
        (Option(e.getOutput).toList ++ e.getInputs.asScala).foldLeft(acc)((acc, attr) => {
          val attrType = attr.getType
          if (RosettaAttributeExtensions.toExpandedType(attrType).isBuiltInType) acc
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
      case _ => s"(implicit convertToScala: ${rosettaTypeToJavaType(output.getType)} => $shortName)"
    }
  }

  private def generateOutputComment(output: Attribute): String =
    Option(output.getDefinition).map(d => s"\n  * @return $d").getOrElse("")

  private def generateImportStatement(output: Attribute): String =
    output.getType match {
      case x if RosettaAttributeExtensions.toExpandedType(x).isBuiltInType => ""
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
        .map(p => convertScalaToJava(p.getName, rosettaAttrToScalaType(p)))
        .mkString(", ")
    val javaFunctionCall = s"new $javaClass().evaluate($paramConversions)"
    val cardinality = output.getCard
    if (cardinality.getInf == 0 && cardinality.getSup == 1)
      s"Option($javaFunctionCall)${mapToScala(output)}"
    else if (cardinality.isIsMany)
      s"$javaFunctionCall.asScala.toList${mapToScala(output)}"
    else
      output.getType.getName match {
        case "string" | "int" | "boolean" | "time" | "dateTime" | "zonedDateTime" => javaFunctionCall
        case "date" => s"$javaFunctionCall.toLocalDate"
        case "number" => s"BigDecimal($javaFunctionCall)"
        case _ => s"convertToScala($javaFunctionCall)"
      }
  }

  private def mapToScala(output: Attribute): String =
    output.getType.getName match {
      case "string" | "int" | "boolean" | "time" | "dateTime" | "zonedDateTime" => ""
      case "date" => ".map(_.toLocalDate)"
      case "number" => ".map(BigDecimal.apply)"
      case _ => ".map(x => convertToScala(x))"
    }

  private def convertScalaToJava(name: String, typeName: String): String =
    typeName match {
      case s"Option[$t]" => s"$name${mapScalaToJava(t)}.orNull"
      case s"List[$t]" => s"$name${mapScalaToJava(t)}.asJava"
      case "BigDecimal" => s"$name.bigDecimal"
      case "LocalDate"  => s"com.rosetta.model.lib.records.Date.of($name)"
      case "Boolean" | "Int" | "String" | "LocalTime" | "LocalDateTime" | "ZonedDateTime" => name
      case _ => s"$typeName.toJava($name)"
    }

  private def mapScalaToJava(typeName: String): String =
    typeName match {
      case "BigDecimal" => ".map(_.bigDecimal)"
      case "LocalDate"  => ".map(com.rosetta.model.lib.records.Date.of)"
      case "Boolean" | "String" | "LocalTime" | "LocalDateTime" | "ZonedDateTime" => ""
      case "Int" => ".map(i => i: Integer)"
      case _ => s".map($typeName.toJava)"
    }
}
object CdmFunctionGenerator {
  def derivePackageName(e: Function): String =
    s"${AbstractCdmGenerator.basePkg}.${e.getModel.getName}.functions"
}
