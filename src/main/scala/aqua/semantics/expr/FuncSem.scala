package aqua.semantics.expr

import aqua.model.{FuncModel, FuncOp, Model}
import aqua.parser.expr.FuncExpr
import aqua.parser.lexer.Arg
import aqua.semantics.{ArrowType, DataType, Prog, Type}
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.scope.PeerIdAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import cats.Applicative
import cats.free.Free
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.collection.immutable.Queue

class FuncSem[F[_]](val expr: FuncExpr[F]) extends AnyVal {
  import expr._

  def before[Alg[_]](implicit
    T: TypesAlgebra[F, Alg],
    N: NamesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg],
    P: PeerIdAlgebra[F, Alg],
    A: AbilitiesAlgebra[F, Alg]
  ): Free[Alg, ArrowType] =
    A.beginScope(name) >> Applicative[Free[Alg, *]]
      .product(
        // Collect argument types, define local variables
        args
          .foldLeft(
            // Begin scope -- for mangling
            N.beginScope(name).as[Queue[Type]](Queue.empty)
          ) { case (f, Arg(argName, argType)) =>
            // Resolve arg type, remember it
            f.flatMap(acc =>
              T.resolveType(argType).flatMap {
                case Some(t: ArrowType) =>
                  N.defineArrow(argName, t, isRoot = false).as(acc.enqueue(t))
                case Some(t) =>
                  N.define(argName, t).as(acc.enqueue(t))
                case None =>
                  Free.pure(acc)
              }
            )
          }
          .map(_.toList),
        // Resolve return type
        ret.fold(Free.pure[Alg, Option[Type]](None))(T.resolveType(_))
      )
      .map(argsAndRes => ArrowType(argsAndRes._1, argsAndRes._2))

  def after[Alg[_]](funcArrow: ArrowType, bodyGen: Model)(implicit
    T: TypesAlgebra[F, Alg],
    N: NamesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg],
    P: PeerIdAlgebra[F, Alg],
    A: AbilitiesAlgebra[F, Alg]
  ): Free[Alg, Model] =
    // Check return value type
    ((funcArrow.res, retValue) match {
      case (Some(t), Some(v)) =>
        V.resolveType(v).flatMap {
          case Some(vt) => T.ensureTypeMatches(v, t, vt).void
          case None => Free.pure[Alg, Unit](())
        }
      case _ =>
        Free.pure[Alg, Unit](())

      // Erase arguments and internal variables
    }) >> A.endScope() >> N.endScope() >> (bodyGen match {
      case bg: FuncOp if ret.isDefined == retValue.isDefined =>
        val argNames = args.map(_.name.value)

        val model = FuncModel(
          name = name.value,
          args = argNames
            .zip(funcArrow.args)
            .map {
              case (n, dt: DataType) => n -> Left(dt)
              case (n, at: ArrowType) => n -> Right(at)
            },
          ret = retValue.map(ValuesAlgebra.valueToData).flatMap(vd => funcArrow.res.map(vd -> _)),
          body = bg
        )

        N.defineArrow(
          name,
          funcArrow,
          isRoot = true
        ) as model
      case _ => Free.pure[Alg, Model](Model.error)
    })

  def program[Alg[_]](implicit
    T: TypesAlgebra[F, Alg],
    N: NamesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg],
    P: PeerIdAlgebra[F, Alg],
    A: AbilitiesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    Prog.around(
      before[Alg],
      after[Alg]
    )

}