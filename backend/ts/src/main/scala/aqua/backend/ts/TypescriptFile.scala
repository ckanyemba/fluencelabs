package aqua.backend.ts

import aqua.model.ScriptModel
import aqua.model.transform.BodyConfig
import cats.data.Chain

case class TypescriptFile(script: ScriptModel) {
  def funcs: Chain[TypescriptFunc] = script.resolveFunctions.map(TypescriptFunc(_))

  def generateTS(conf: BodyConfig = BodyConfig()): String =
    TypescriptFile.Header + "\n\n" + funcs.map(_.generateTypescript(conf)).toList.mkString("\n\n")
}

object TypescriptFile {

  val Header: String =
    s"""/**
       | *
       | * This file is auto-generated. Do not edit manually: changes may be erased.
       | * Generated by Aqua compiler: https://github.com/fluencelabs/aqua/. 
       | * If you find any bugs, please write an issue on GitHub: https://github.com/fluencelabs/aqua/issues
       | * Aqua version: ${Option(getClass.getPackage.getImplementationVersion)
      .filter(_.nonEmpty)
      .getOrElse("Unknown")}
       | *
       | */
       |import { FluenceClient, PeerIdB58 } from '@fluencelabs/fluence';
       |import { RequestFlowBuilder } from '@fluencelabs/fluence/dist/api.unstable';
       |""".stripMargin

}
