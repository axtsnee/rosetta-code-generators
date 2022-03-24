package com.regnosys.rosetta.generator.scalawrapper

import java.util.{Collections, Collection => JCollection, List => JList, Map => JMap}
import java.io.File
import scala.jdk.CollectionConverters._
import com.regnosys.rosetta.generator.external.AbstractExternalGenerator
import com.regnosys.rosetta.generator.java.RosettaJavaPackages
import com.regnosys.rosetta.rosetta.{RosettaEnumeration, RosettaMetaType, RosettaModel, RosettaRootElement, RosettaType}
import com.regnosys.rosetta.generators.test.TestHelper
import com.regnosys.rosetta.rosetta.simple.{Data, Function}

object JavaWrapperGenerator extends AbstractExternalGenerator("ScalaWrapper") {
  override def generate(packages: RosettaJavaPackages, elements: JList[RosettaRootElement], version: String): JMap[String, _ <: CharSequence] = Collections.emptyMap()

  override def afterGenerate(jmodels: JCollection[_ <: RosettaModel]): JMap[String, _ <: CharSequence] = {
    val rootElements = extractRootElements(jmodels.asScala)
    val enclosingElements = extractEnclosingElements(rootElements).withDefaultValue(Nil)
    val result = rootElements.groupMapReduce(_.fileName)(_.generate(enclosingElements))(_ ++ _)
    result.view.mapValues(_.mkString("\n")).toMap.asJava
  }

  private def x = ???

  private def extractRootElements(models: Iterable[RosettaModel]): Iterable[CdmElement[_]] =
    models.flatMap(_.getElements.asScala).map {
      case e: Data => CdmType(e)
      case e: RosettaMetaType => CdmMetaType(e)
      case e: RosettaEnumeration => CdmEnumeration(e)
      case e: Function => CdmFunction(e)
      case e => Unknown(e)
    }

  private def extractEnclosingElements(rootElements: Iterable[CdmElement[_]]): Map[RosettaType, List[RosettaType]] =
    rootElements.foldLeft(Map.empty[RosettaType, List[RosettaType]]) {
      case (acc, c: CdmType) if CdmTypeGenerator.shouldBeSumType(c.element) =>
        c.element.getAttributes.asScala.foldLeft(acc)((acc, attr) => {
          acc.updatedWith(attr.getType) {
            case Some(list) => Some(c.element :: list)
            case None       => Some(List(c.element))
          }
        })
      case (acc, _) => acc
    }

  private def getRecursiveListOfRosettaFiles(dir: File): Vector[File] = {
    val topLevel = dir.listFiles.toVector
    val all = topLevel ++ topLevel.filter(_.isDirectory).flatMap(getRecursiveListOfRosettaFiles)
    all.filter(_.getName.endsWith(".rosetta"))
  }

  def main(args: Array[String]): Unit = {
    val runner = new TestHelper(this)
    val rosettaFiles = getRecursiveListOfRosettaFiles(new File(args.head))
    val models = rosettaFiles.map(f => runner.parse(f.toURI.toURL))
    val result = afterGenerate(models.asJava).asScala
    println(result)
  }
}
