package com.regnosys.rosetta.generator.scalawrapper

import com.regnosys.rosetta.rosetta.simple.Function

object CdmFunctionGenerator {
  val generate: Function => String = e => {
    s"/* $e */\n"
  }
}
