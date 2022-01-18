package aqua.model.transform.res

import aqua.model.AquaContext
import aqua.model.transform.res.*
import aqua.model.transform.{Transform, TransformConfig}
import cats.data.Chain

// TODO: doc
case class AquaRes(funcs: Chain[FuncRes], services: Chain[ServiceRes]) {
  def isEmpty: Boolean = funcs.isEmpty && services.isEmpty
}

object AquaRes {
  private val blank = AquaRes(Chain.nil, Chain.nil)

  // TODO: doc/rename
  def fromContext(ex: AquaContext, conf: TransformConfig): AquaRes =
    AquaRes(
      funcs = Chain
        .fromSeq(ex.funcs.map { case (fnName, fn) =>
          fn.copy(funcName = fnName)
        }.toSeq)
        .map(
          // TODO: keeep Eval
          Transform.funcRes(_, conf).value
        ),
      services = Chain
        .fromSeq(ex.services.map { case (srvName, srv) =>
          srv.copy(name = srvName)
        }.toSeq)
        .map(ServiceRes.fromModel)
    )

}
