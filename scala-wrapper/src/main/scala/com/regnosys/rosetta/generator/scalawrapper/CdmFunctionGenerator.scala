package com.regnosys.rosetta.generator.scalawrapper

import scala.jdk.CollectionConverters._
import com.regnosys.rosetta.generator.scalawrapper.GeneratorFunctions._
import com.regnosys.rosetta.rosetta.simple.Function

object CdmFunctionGenerator {
  val generate: Function => String = e => {
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
          s"${p.getName}: ${rosettaTypeToScalaType(p.getType)}"
        }
        s"""object ${e.getName} {
           |$classComment  def apply(${paramDecls.mkString(", ")}): ${rosettaTypeToScalaType(output.getType)} =
           |    new ${e.getModel.getName}.functions.${e.getName}.${e.getName}Default().evaluate($paramNames)
           |}
           |""".stripMargin
    }
  }

  private def returnType(f: Function): String =
    Option(f.getOutput).map(a => rosettaTypeToScalaType(a.getType)).getOrElse(debugFunction(f))
}
