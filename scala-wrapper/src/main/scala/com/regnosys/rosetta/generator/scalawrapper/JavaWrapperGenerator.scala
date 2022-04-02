package com.regnosys.rosetta.generator.scalawrapper

import java.util.{Collections, Collection => JCollection, List => JList, Map => JMap}
import scala.jdk.CollectionConverters._

import com.regnosys.rosetta.generator.external.AbstractExternalGenerator
import com.regnosys.rosetta.generator.java.RosettaJavaPackages
import com.regnosys.rosetta.rosetta.{RosettaModel, RosettaRootElement}

object JavaWrapperGenerator extends AbstractExternalGenerator("ScalaWrapper") {
  val enumFilename: String = "CdmEnums.scala"
  val functionFilename: String = "CdmFunctions.scala"
  val metaTypeFilename: String = "CdmMetaTypes.scala"
  val typeFilename: String = "CdmTypes.scala"

  override def generate(
      packages: RosettaJavaPackages,
      elements: JList[RosettaRootElement],
      version: String
  ): JMap[String, _ <: CharSequence] =
    Collections.emptyMap()

  override def afterGenerate(
      jmodels: JCollection[_ <: RosettaModel]
  ): JMap[String, _ <: CharSequence] = {
    val models = jmodels.asScala
    val rootElements = models.flatMap(_.getElements.asScala)
    val analysis = rootElements.foldLeft(RootElementAnalyzer.empty)((z, e) => z.nextElement(e))
    Map(
      enumFilename -> CdmEnumerationGenerator(analysis).generateFile,
      functionFilename -> CdmFunctionGenerator(analysis).generateFile,
      metaTypeFilename -> CdmMetaTypeGenerator(analysis).generateFile,
      typeFilename -> CdmTypeGenerator(analysis).generateFile
    ).asJava
  }
}
