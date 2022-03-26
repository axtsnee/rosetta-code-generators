package com.regnosys.rosetta.generator.scalawrapper

import com.regnosys.rosetta.rosetta.RosettaMetaType

object CdmMetaTypeGenerator {
  val generate: RosettaMetaType => String = e => {
    e.toString
  }
}
