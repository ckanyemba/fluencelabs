package aqua.parser.expr

import aqua.parser.Ast.Tree
import aqua.parser.{Expr, Indent}
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Arg, DataTypeToken, Name, Value}
import aqua.parser.lift.LiftParser
import cats.free.Cofree
import cats.parse.Parser
import cats.Comonad

case class FuncExpr[F[_]](name: Name[F], args: List[Arg[F]], ret: Option[DataTypeToken[F]], retValue: Option[Value[F]])
    extends Expr[F]

object FuncExpr extends Expr.AndIndented(OnExpr, AbilityIdExpr, ReturnExpr, CoalgebraExpr, ParExpr, ForExpr) {

  override def p[F[_]: LiftParser: Comonad]: Parser[FuncExpr[F]] =
    ((`func` *> ` ` *> Name.p[F]) ~ comma0(Arg.p)
      .between(`(`, `)`) ~ (` -> ` *> DataTypeToken.`datatypedef`).? <* ` : \n+`).map { case ((name, args), ret) =>
      FuncExpr(name, args, ret, None)
    }

  override def ast[F[_]: LiftParser: Comonad](ps: Indent): Parser[Tree[F]] =
    super.ast(ps).flatMap { tree =>
      tree.head match {
        case funcExpr: FuncExpr[F] if funcExpr.ret.isDefined =>
          tree.tail.value.lastOption.map(_.head) match {
            case Some(re: ReturnExpr[F]) =>
              Parser.pure(
                Cofree(funcExpr.copy(retValue = Some(re.value)), tree.tail)
              )
            case _ =>
              Parser.failWith(
                "Return type is defined for function, but nothing returned. Use `<- value` as the last expression inside function body."
              )
          }

        case _ => Parser.pure(tree)
      }
    }
}