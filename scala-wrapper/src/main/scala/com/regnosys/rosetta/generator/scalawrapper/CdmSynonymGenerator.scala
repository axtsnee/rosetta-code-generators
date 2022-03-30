package com.regnosys.rosetta.generator.scalawrapper

import com.regnosys.rosetta.rosetta.RosettaSynonym

object CdmSynonymGenerator {
  def derivePackageName(e: RosettaSynonym): String =
    s"${AbstractCdmGenerator.basePkg}.cdm.base.synonym"
}
