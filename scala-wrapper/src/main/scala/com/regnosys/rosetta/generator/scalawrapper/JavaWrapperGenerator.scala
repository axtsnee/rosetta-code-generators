package com.regnosys.rosetta.generator.scalawrapper

import java.util.{Collections, Collection => JCollection, List => JList, Map => JMap}
import java.nio.file.{Files, Paths}
import java.io.File
import scala.jdk.CollectionConverters._
import com.regnosys.rosetta.generator.external.AbstractExternalGenerator
import com.regnosys.rosetta.generator.java.RosettaJavaPackages
import com.regnosys.rosetta.rosetta.{RosettaEnumeration, RosettaMetaType, RosettaModel, RosettaRootElement, RosettaType}
import com.regnosys.rosetta.generators.test.TestHelper
import com.regnosys.rosetta.rosetta.simple.{Data, Function}

object JavaWrapperGenerator extends AbstractExternalGenerator("ScalaWrapper") {
  override def generate(packages: RosettaJavaPackages, elements: JList[RosettaRootElement], version: String): JMap[String, _ <: CharSequence] = Collections.emptyMap()

  override def afterGenerate(models: JCollection[_ <: RosettaModel]): JMap[String, _ <: CharSequence] = {
    val rootElements = models.asScala.flatMap(_.getElements.asScala)
    val enclosingElements = extractEnclosingElements(rootElements)
    val extendingEnums = extractExtendingEnums(rootElements)
    val result = rootElements.groupMapReduce(elementToFileName)(translate(enclosingElements, extendingEnums))(_ ++ _)
    result.asJava
  }

  private val elementToFileName: RosettaRootElement => String = {
    case _: Data => "CdmTypes.scala"
    case _: RosettaMetaType => "CdmMetaTypes.scala"
    case _: RosettaEnumeration => "CdmEnums.scala"
    case _: Function => "CdmFunctions.scala"
    case _ => "Unknown.txt"
  }

  private def extractEnclosingElements(rootElements: Iterable[RosettaRootElement]): Map[RosettaType, List[RosettaType]] =
    rootElements.foldLeft(Map.empty[RosettaType, List[RosettaType]].withDefaultValue(Nil)) {
      case (acc, e: Data) if CdmTypeGenerator.shouldBeSumType(e) =>
        e.getAttributes.asScala.foldLeft(acc)((acc, attr) => {
          acc.updatedWith(attr.getType) {
            case Some(list) => Some(e :: list)
            case None       => Some(List(e))
          }
        })
      case (acc, _) => acc
    }

  private def extractExtendingEnums(rootElements: Iterable[RosettaRootElement]): Map[RosettaType, Set[RosettaType]] =
    rootElements.foldLeft(Map.empty[RosettaType, Set[RosettaType]].withDefaultValue(Set.empty)) {
      case (acc, e: RosettaEnumeration) =>
        getAllAncestors(e).foldLeft(acc)((acc, ancestor) => {
          acc.updatedWith(ancestor) {
            case Some(descendants) => Some(descendants + e)
            case None => Some(Set(e))
          }
        })
      case (acc, _) => acc
    }

  def translate(enclosingElements: Map[RosettaType, List[RosettaType]], extendingEnums: Map[RosettaType, Set[RosettaType]]): RosettaRootElement => String = {
      case e: Data => CdmTypeGenerator.generate(enclosingElements(e))(e)
      case e: RosettaMetaType => CdmMetaTypeGenerator.generate(e)
      case e: RosettaEnumeration => CdmEnumerationGenerator.generate(extendingEnums(e))(e)
      case e: Function => CdmFunctionGenerator.generate(e)
      case e => e.toString
    }

  private def getAllAncestors(e: RosettaEnumeration): List[RosettaEnumeration] =
    Option(e.getSuperType) match {
      case Some(superType) => superType :: getAllAncestors(superType)
      case None => Nil
    }

  private def getRecursiveListOfRosettaFiles(dir: File): Vector[File] = {
    val topLevel = dir.listFiles.toVector
    val all = topLevel ++ topLevel.filter(_.isDirectory).flatMap(getRecursiveListOfRosettaFiles)
    all.filter(_.getName.endsWith(".rosetta"))
  }

  def main(args: Array[String]): Unit = {
    require(args.nonEmpty)
    val runner = new TestHelper(this)
    val rosettaFiles = getRecursiveListOfRosettaFiles(new File(args.head))
    val models = rosettaFiles.map(f => runner.parse(f.toURI.toURL))
    val result = afterGenerate(models.asJava).asScala
    val saveDir = args.tail.headOption match {
      case Some(dirName) => dirName
      case None => "."
    }
    result.foreach {
      case (filename, contents) => Files.writeString(Paths.get(saveDir, filename), contents)
    }
  }
}
