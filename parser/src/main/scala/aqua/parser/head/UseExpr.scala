package aqua.parser.head

import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Ability, Literal, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser

case class UseExpr[F[_]](
  filename: Literal[F],
  asModule: Option[Ability[F]]
) extends FilenameExpr[F]

object UseExpr extends HeaderExpr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: Parser[HeaderExpr[F]] =
    (`use` *> ` ` *> Value
      .string[F] ~ (` as ` *> Ability.ab[F]).?).map { case (filename, asModule) =>
      UseExpr(filename, asModule)
    }
}
