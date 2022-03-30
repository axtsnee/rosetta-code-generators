package com.regnosys.rosetta.generator.scalawrapper

import com.regnosys.rosetta.rosetta.simple.{Data, Function}
import com.regnosys.rosetta.rosetta.{RosettaEnumeration, RosettaMetaType, RosettaRootElement, RosettaSynonym}

case class RootElementAnalyzer(
    enums: List[RosettaEnumeration],
    functions: List[Function],
    metaTypes: List[RosettaMetaType],
    types: List[Data],
    nsToPkgs: Map[String, Set[String]]
) {
  def nextElement(r: RosettaRootElement): RootElementAnalyzer =
    r match {
      case e: RosettaEnumeration => copy(
        enums = e :: enums, nsToPkgs = updateNsToPkgs(e, CdmEnumerationGenerator.derivePackageName(e))
      )
      case e: Function => copy(
        functions = e :: functions, nsToPkgs = updateNsToPkgs(e, CdmFunctionGenerator.derivePackageName(e))
      )
      case e: RosettaMetaType => copy(
        metaTypes = e :: metaTypes, nsToPkgs = updateNsToPkgs(e, CdmMetaTypeGenerator.derivePackageName(e))
      )
      case e: RosettaSynonym => copy(
        nsToPkgs = updateNsToPkgs(e, CdmSynonymGenerator.derivePackageName(e))
      )
      case e: Data => copy(
        types = e :: types, nsToPkgs = updateNsToPkgs(e, CdmTypeGenerator.derivePackageName(e))
      )
      // Unhandled: synonym, annotation, body, corpus, segment
      case _ => this
    }

  private def updateNsToPkgs(r: RosettaRootElement, pkgName: String): Map[String, Set[String]] =
    nsToPkgs.updatedWith(r.getModel.getName) {
      case Some(ms) => Some(ms + pkgName)
      case None => Some(Set(pkgName))
    }
}
object RootElementAnalyzer {
  val empty: RootElementAnalyzer =
    RootElementAnalyzer(Nil, Nil, Nil, Nil, Map.empty)
}
