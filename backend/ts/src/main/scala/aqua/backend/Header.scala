package aqua.backend

object Header {

  def header(isJs: Boolean, isCommonJS: Boolean): String = {
    val imports = if (isCommonJS) {
      """const { Fluence, FluencePeer } = require('@fluencelabs/fluence');
        |const {
        |    CallParams,
        |    callFunction,
        |    registerService,
        |} = require('@fluencelabs/fluence/dist/internal/compilerSupport/v2${if (isJs) ".js" else ""}');""".stripMargin
    } else {
      s"""import { Fluence, FluencePeer } from '@fluencelabs/fluence';
         |import {
         |    CallParams,
         |    callFunction,
         |    registerService,
         |} from '@fluencelabs/fluence/dist/internal/compilerSupport/v2${if (isJs) ".js" else ""}';""".stripMargin
    }
    s"""/**
       | *
       | * This file is auto-generated. Do not edit manually: changes may be erased.
       | * Generated by Aqua compiler: https://github.com/fluencelabs/aqua/.
       | * If you find any bugs, please write an issue on GitHub: https://github.com/fluencelabs/aqua/issues
       | * Aqua version: ${Version.version}
       | *
       | */
       |$imports
       |""".stripMargin
  }
}
