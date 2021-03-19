package aqua.semantics

import aqua.model.Model
import aqua.parser.lexer.Token
import aqua.parser.{Ast, Expr}
import aqua.semantics.rules.ReportError
import aqua.semantics.rules.abilities.{AbilitiesAlgebra, AbilitiesInterpreter, AbilitiesState, AbilityOp}
import aqua.semantics.rules.names.{NameOp, NamesAlgebra, NamesInterpreter, NamesState}
import aqua.semantics.rules.scope.{PeerIdAlgebra, PeerIdInterpreter, PeerIdOp, PeerIdState}
import aqua.semantics.rules.types.{TypeOp, TypesAlgebra, TypesInterpreter, TypesState}
import cats.Eval
import cats.arrow.FunctionK
import cats.data.Validated.{Invalid, Valid}
import cats.data.{EitherK, NonEmptyList, State, ValidatedNel}
import cats.free.Free
import monocle.Lens
import monocle.macros.GenLens
import cats.syntax.apply._
import cats.syntax.semigroup._

import scala.collection.immutable.Queue

object Semantics {

  def folder[F[_], G[_]](implicit
    A: AbilitiesAlgebra[F, G],
    N: NamesAlgebra[F, G],
    P: PeerIdAlgebra[F, G],
    T: TypesAlgebra[F, G]
  ): (Expr[F], List[Free[G, Model]]) => Eval[Free[G, Model]] = { case (expr, inners) =>
    Eval later ExprSem
      .getProg[F, G](expr)
      .apply(
        inners
          .reduceLeftOption[Free[G, Model]]((a, b) => (a, b).mapN(_ |+| _))
          .getOrElse(Free.pure(Model.empty))
      )
  }

  type Alg0[F[_], A] = EitherK[AbilityOp[F, *], NameOp[F, *], A]
  type Alg1[F[_], A] = EitherK[PeerIdOp[F, *], Alg0[F, *], A]
  type Alg[F[_], A] = EitherK[TypeOp[F, *], Alg1[F, *], A]

  def transpile[F[_]](ast: Ast[F]): Free[Alg[F, *], Model] =
    ast.cata(folder[F, Alg[F, *]]).value

  case class CompilerState[F[_]](
    errors: Queue[(Token[F], String)] = Queue.empty[(Token[F], String)],
    names: NamesState[F] = NamesState[F](),
    abilities: AbilitiesState[F] = AbilitiesState[F](),
    peerId: PeerIdState[F] = PeerIdState[F](),
    types: TypesState[F] = TypesState[F]()
  )

  def interpret[F[_]](free: Free[Alg[F, *], Model]): State[CompilerState[F], Model] = {
    import monocle.macros.syntax.all._

    implicit val re: ReportError[F, CompilerState[F]] =
      (st: CompilerState[F], token: Token[F], hint: String) => st.focus(_.errors).modify(_.enqueue(token -> hint))

    implicit val ns: Lens[CompilerState[F], NamesState[F]] = GenLens[CompilerState[F]](_.names)

    val names = new NamesInterpreter[F, CompilerState[F]]()

    implicit val as: Lens[CompilerState[F], AbilitiesState[F]] = GenLens[CompilerState[F]](_.abilities)

    val abilities = new AbilitiesInterpreter[F, CompilerState[F]]()

    implicit val ps: Lens[CompilerState[F], PeerIdState[F]] = GenLens[CompilerState[F]](_.peerId)

    val peerId = new PeerIdInterpreter[F, CompilerState[F]]()

    implicit val ts: Lens[CompilerState[F], TypesState[F]] = GenLens[CompilerState[F]](_.types)

    val types = new TypesInterpreter[F, CompilerState[F]]()

    val interpreter0: FunctionK[Alg0[F, *], State[CompilerState[F], *]] = abilities or names
    val interpreter1: FunctionK[Alg1[F, *], State[CompilerState[F], *]] = peerId or interpreter0
    val interpreter: FunctionK[Alg[F, *], State[CompilerState[F], *]] = types or interpreter1

    free.foldMap[State[CompilerState[F], *]](interpreter)
  }

  def validate[F[_]](ast: Ast[F]): ValidatedNel[(Token[F], String), Model] =
    (transpile[F] _ andThen interpret[F])(ast)
      .run(CompilerState[F]())
      .map { case (state, gen) =>
        NonEmptyList.fromList(state.errors.toList).fold[ValidatedNel[(Token[F], String), Model]](Valid(gen))(Invalid(_))
      }
      .value
}