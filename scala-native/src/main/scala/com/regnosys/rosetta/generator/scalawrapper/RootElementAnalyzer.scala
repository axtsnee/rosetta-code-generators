package com.regnosys.rosetta.generator.scalawrapper

import com.regnosys.rosetta.rosetta.simple.{Data, Function}
import com.regnosys.rosetta.rosetta.{RosettaEnumeration, RosettaMetaType, RosettaRootElement}

case class RootElementAnalyzer(
    enums: List[RosettaEnumeration],
    functions: List[Function],
    metaTypes: List[RosettaMetaType],
    types: List[Data]
) {
  def nextElement(r: RosettaRootElement): RootElementAnalyzer =
    r match {
      case e: RosettaEnumeration => copy(enums = e :: enums)
      case e: Function => copy(functions = e :: functions)
      case e: RosettaMetaType => copy(metaTypes = e :: metaTypes)
      case e: Data => copy(types = e :: types)
      // Unhandled: synonym, annotation, body, corpus, segment
      case _ => this
    }
}
object RootElementAnalyzer {
  val empty: RootElementAnalyzer =
    RootElementAnalyzer(Nil, Nil, Nil, Nil)
}
