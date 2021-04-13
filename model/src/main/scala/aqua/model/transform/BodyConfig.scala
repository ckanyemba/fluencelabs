package aqua.model.transform

import aqua.model.{LiteralModel, ValueModel}

case class BodyConfig(
  getDataService: String = "getDataSrv",
  callbackService: String = "callbackSrv",
  errorHandlingService: String = "errorHandlingSrv",
  error: String = "error",
  respFuncName: String = "response",
  relayVarName: String = "relay"
) {

  val errorId: ValueModel = LiteralModel("\"" + error + "\"")
  val errorHandlingCallback: ValueModel = LiteralModel("\"" + errorHandlingService + "\"")
  val callbackSrvId: ValueModel = LiteralModel("\"" + callbackService + "\"")
  val dataSrvId: ValueModel = LiteralModel("\"" + getDataService + "\"")

}