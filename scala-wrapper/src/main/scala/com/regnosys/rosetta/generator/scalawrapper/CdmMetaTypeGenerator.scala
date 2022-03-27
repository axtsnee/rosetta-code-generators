package com.regnosys.rosetta.generator.scalawrapper

import com.regnosys.rosetta.rosetta.RosettaMetaType

object CdmMetaTypeGenerator {
  val generate: RosettaMetaType => String = e => {
    s"""//RosettaMetaType $e
       |//  getModel.getName ${e.getModel.getName}
       |//  getName ${e.getName}
       |//  getType.getName ${e.getType.getName}
       |""".stripMargin
  }
}
