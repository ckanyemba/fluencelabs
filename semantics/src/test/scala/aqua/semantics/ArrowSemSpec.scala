package aqua.semantics

import aqua.parser.expr.func.ArrowExpr
import aqua.parser.lexer.{BasicTypeToken, Name}
import aqua.raw.Raw
import aqua.raw.arrow.ArrowRaw
import aqua.raw.ops.{FuncOp, FuncOps, ReturnTag}
import aqua.raw.value.LiteralRaw
import aqua.semantics.expr.func.ArrowSem
import aqua.types.*
import cats.Id
import cats.data.{NonEmptyList, State}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ArrowSemSpec extends AnyFlatSpec with Matchers with EitherValues {
  import Utils.*

  def program(arrowStr: String): Prog[State[CompilerState[cats.Id], *], Raw] = {
    import CompilerState.*

    val expr = ArrowExpr.p.parseAll(arrowStr).value.mapK(spanToId)
    val sem = new ArrowSem[Id](expr)

    sem.program[State[CompilerState[Id], *]]
  }

  "sem" should "create empty model" in {
    val model = getModel(program("(a: string, b: u32) -> u8"))
    model shouldBe (Raw.Empty("empty"))
  }

  "sem" should "create error model" ignore {
    val model = getModel(FuncOps.empty)(program("(a: string, b: u32) -> u8"))
    model shouldBe Raw.Empty(
      "Return type is defined for the arrow, but nothing returned. Use `<- value, ...` as the last expression inside function body."
    )
  }

  import aqua.types.ScalarType.*

  "arrow without return type" should "create right model" ignore {
    val model = getModel(FuncOps.empty)(program("(a: string, b: u32)"))
    model shouldBe ArrowRaw(
      ArrowType(labelled("a", string, labelled("b", u32)), NilType),
      Nil,
      FuncOps.empty
    )
  }

  "arrow with return type and correct state" should "create correct model" ignore {
    val returnValue = LiteralRaw("123", string)
    val returnTag = FuncOp.wrap(ReturnTag(NonEmptyList.one(returnValue)), FuncOps.empty)
    val model = getModel(returnTag)(program("(a: string, b: u32) -> string"))

    val arrowType = ArrowType(labelled("a", string, labelled("b", u32)), productType(string))
    val resultModel = ArrowRaw(arrowType, returnValue :: Nil, returnTag)
    model shouldBe resultModel
  }

  "arrow with return type and seq inside" should "create correct model" ignore {
    val returnValue = LiteralRaw("123", string)
    val seq = FuncOps.seq(
      FuncOps.empty,
      FuncOp.wrap(ReturnTag(NonEmptyList.one(returnValue)), FuncOps.empty)
    )
    val model = getModel(seq)(program("(a: string, b: u32) -> string"))

    val arrowType = ArrowType(labelled("a", string, labelled("b", u32)), productType(string))
    val resultModel = ArrowRaw(arrowType, returnValue :: Nil, seq)
    model shouldBe resultModel
  }

  "different types in return type and return value" should "create error model" ignore {
    val returnValue = LiteralRaw("123", string)
    val seq = FuncOps.seq(
      FuncOps.empty,
      FuncOp.wrap(ReturnTag(NonEmptyList.one(returnValue)), FuncOps.empty)
    )
    val state = getState(seq)(program("(a: string, b: u32) -> u32"))

    state.errors.headOption.get shouldBe RulesViolated[Id](
      BasicTypeToken[Id](u32),
      "Types mismatch, expected: u32, given: string"
    )

  }
}
