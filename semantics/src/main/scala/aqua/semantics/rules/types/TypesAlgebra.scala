package aqua.semantics.rules.types

import aqua.parser.lexer.*
import aqua.raw.value.{LambdaRaw, ValueRaw}
import aqua.types.{ArrowType, Type}
import cats.data.NonEmptyMap
import cats.data.NonEmptyList

trait TypesAlgebra[S[_], Alg[_]] {

  def resolveType(token: TypeToken[S]): Alg[Option[Type]]

  def resolveArrowDef(arrowDef: ArrowTypeToken[S]): Alg[Option[ArrowType]]

  def defineField(name: Name[S], `type`: Type): Alg[Boolean]

  def purgeFields(token: Token[S]): Alg[Option[NonEmptyMap[String, Type]]]

  def defineDataType(
    name: CustomTypeToken[S],
    fields: NonEmptyMap[String, Type]
  ): Alg[Boolean]

  def defineAlias(name: CustomTypeToken[S], target: Type): Alg[Boolean]

  def resolveIndex(rootT: Type, op: IntoIndex[S], idx: ValueRaw): Alg[Option[LambdaRaw]]
  def resolveField(rootT: Type, op: IntoField[S]): Alg[Option[LambdaRaw]]

  def ensureTypeMatches(token: Token[S], expected: Type, givenType: Type): Alg[Boolean]

  def expectNoExport(token: Token[S]): Alg[Unit]

  def checkArgumentsNumber(token: Token[S], expected: Int, givenNum: Int): Alg[Boolean]

  def beginArrowScope(token: ArrowTypeToken[S]): Alg[ArrowType]

  // Check return types
  def checkArrowReturn(values: NonEmptyList[(Value[S], ValueRaw)]): Alg[Boolean]

  // End scope; if return was expected but not checked, fail
  def endArrowScope(token: Token[S]): Alg[List[ValueRaw]]
}
