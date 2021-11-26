package aqua.run

import scribe.Level

// `run` command configuration
case class RunConfig(
  timeout: Int,
  logLevel: Level,
  printAir: Boolean,
  secretKey: Option[Array[Byte]],
  consoleServiceId: String = "--after-callback-srv-service--",
  printFunctionName: String = "print-and-stop",
  resultName: String = "-some-unique-res-name-",
  functionWrapperName: String = "--someFuncToRun--"
)