package com.regnosys.rosetta.generator.scalanative

object ScalaProcessorGenerator {
  val generateFile: String =
    s"""package ${AbstractScalaGenerator.basePackage}
      |
      |import scala.jdk.CollectionConverters._
      |
      |import com.regnosys.rosetta.common.util.SimpleProcessor
      |import com.rosetta.lib.postprocess.PostProcessorReport
      |import com.rosetta.model.lib.RosettaModelObject
      |import com.rosetta.model.lib.meta.{FieldWithMeta, GlobalKeyFields}
      |import com.rosetta.model.lib.path.RosettaPath
      |import com.rosetta.model.lib.process.{AttributeMeta, PostProcessStep, Processor}
      |
      |class ProcessorReport extends Processor.Report {
      |  var lookupTable: Map[String, List[_]] = Map.empty
      |}
      |
      |class GlobalKeyProcessor extends SimpleProcessor with PostProcessStep {
      |  override val report = new ProcessorReport()
      |
      |  override def processRosetta[R <: RosettaModelObject](
      |      path: RosettaPath,
      |      rosettaType: Class[_ <: R],
      |      instance: R,
      |      parent: RosettaModelObject,
      |      metas: AttributeMeta*
      |  ): Boolean = {
      |    instance match {
      |      case g: GlobalKeyFields =>
      |        val keys =
      |          Option(g.getGlobalKey)
      |            .map(_ :: Nil)
      |            .orElse(Option(g.getKey.asScala).map(keys => keys.map(_.getKeyValue)))
      |            .getOrElse(Nil)
      |        val value = parent match {
      |          case f: FieldWithMeta[_] => f.getValue
      |          case _ => parent
      |        }
      |        keys.foreach { key =>
      |          report.lookupTable = report.lookupTable.updatedWith(key) {
      |            case Some(others) => Some(value :: others)
      |            case None => Some(List(value))
      |          }
      |        }
      |      case _ => ()
      |    }
      |    true
      |  }
      |
      |  override def getPriority: Integer = 1
      |
      |  override def getName: String = "GlobalKey Processor"
      |
      |  override def runProcessStep[T <: RosettaModelObject](
      |      topClass: Class[_ <: T],
      |      instance: T
      |  ): PostProcessorReport = {
      |    val path = RosettaPath.valueOf(topClass.getSimpleName)
      |    this.processRosetta(path, topClass, instance, null)
      |    instance.process(path, this)
      |    new PostProcessorReport {
      |      override def getResultObject: RosettaModelObject = instance
      |    }
      |  }
      |}
      |""".stripMargin
}
