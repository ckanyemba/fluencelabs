package aqua.model.transform

import aqua.model.VarModel
import aqua.model.func.{ArgDef, ArgsCall, ArgsDef, Call, FuncCallable}
import aqua.model.func.body.{FuncOp, FuncOps}
import aqua.types.ArrowType
import cats.Eval

case class ResolveFunc(
  transform: FuncOp => FuncOp,
  callback: (String, Call) => FuncOp,
  respFuncName: String,
  wrapCallableName: String = "funcAround",
  arrowCallbackPrefix: String = "init_peer_callable_"
) {

  def returnCallback(func: FuncCallable): Option[FuncOp] = func.ret.map { retArg =>
    callback(
      respFuncName,
      Call(
        retArg :: Nil,
        None
      )
    )
  }

  def arrowToCallback(name: String, arrowType: ArrowType): FuncCallable = {
    val (args, call, ret) = ArgsCall.arrowToArgsCallRet(arrowType)
    FuncCallable(
      arrowCallbackPrefix + name,
      callback(name, call),
      args,
      ret,
      Map.empty,
      Map.empty
    )
  }

  def wrap(func: FuncCallable): FuncCallable =
    FuncCallable(
      wrapCallableName,
      transform(
        FuncOps.seq(
          FuncOps
            .callArrow(
              func.funcName,
              Call(
                func.args.toCallArgs,
                None
              )
            ) ::
            returnCallback(func).toList: _*
        )
      ),
      ArgsDef(ArgDef.Arrow(func.funcName, func.arrowType) :: Nil),
      None,
      func.args.arrowArgs.collect { case ArgDef.Arrow(argName, arrowType) =>
        argName -> arrowToCallback(argName, arrowType)
      }.toList.toMap,
      func.capturedValues
    )

  def resolve(func: FuncCallable, funcArgName: String = "_func"): Eval[FuncOp] =
    wrap(func)
      .resolve(
        Call(Call.Arg(VarModel(funcArgName), func.arrowType) :: Nil, None),
        Map(funcArgName -> func),
        Set.empty
      )
      .map(_._1)
}