package aqua.builder

import aqua.backend.{ArgDefinition, PrimitiveType, ServiceDef, ServiceFunctionDef, VoidType}
import aqua.ipfs.js.IpfsApi
import aqua.js.{CallJsFunction, CallServiceHandler, FluencePeer}
import scribe.Logging
import scalajs.js

class IPFSUploader(serviceId: String, fnName: String) extends ServiceFunction with Logging {

  def registerService(peer: FluencePeer): Unit = {
    CallJsFunction.registerService(
      peer,
      serviceId,
      fnName,
      args => {

        IpfsApi
          .uploadFile(args(0), args(1), logger.info: String => Unit, logger.error: String => Unit)
          .`catch` { err =>
            js.Dynamic.literal(error = "Error on uploading file: " + err)
          }

      },
      ServiceDef(
        None,
        ServiceFunctionDef(
          fnName,
          ArgDefinition("path", PrimitiveType) :: ArgDefinition("multiaddr", PrimitiveType) :: Nil,
          PrimitiveType
        ) :: Nil
      )
    )
  }
}
