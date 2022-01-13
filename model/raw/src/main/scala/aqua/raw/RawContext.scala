package aqua.raw

import cats.Monoid
import cats.Semigroup
import cats.data.Chain
import cats.data.NonEmptyMap
import aqua.raw.arrow.FuncRaw
import aqua.raw.value.ValueRaw
import aqua.types.{StructType, Type}
import cats.syntax.monoid.*

import scala.collection.immutable.SortedMap

case class RawContext(
  module: Option[String] = None,
  declares: Set[String] = Set.empty,
  exports: Option[Map[String, Option[String]]] = None,
  parts: RawPart.Parts = RawPart.RPSMonoid.empty,
  abilities: Map[String, RawContext] = Map.empty
) {

  def isEmpty: Boolean = this == RawContext.blank

  def nonEmpty: Boolean = !isEmpty

  def pick(
    name: String,
    rename: Option[String],
    declared: Boolean = module.nonEmpty
  ): Option[RawContext] =
    Option
      .when(!declared || declares(name)) {
        val targetName = rename.getOrElse(name)
        RawContext.blank
          .copy(parts = parts.pick(name, rename))
      }
      .filter(_.nonEmpty)

  def pickHeader: RawContext =
    RawContext.blank.copy(module = module, declares = declares, exports = exports)

  def pickDeclared(implicit semi: Semigroup[RawContext]): RawContext =
    if (module.isEmpty) this
    else
      declares.toList
        .flatMap(pick(_, None))
        .foldLeft(pickHeader)(
          _ |+| _
        )

  private def prefixFirst[T](prefix: String, pair: (String, T)): (String, T) =
    (prefix + pair._1, pair._2)

  lazy val services: Map[String, ServiceRaw] = parts.collectMap { case srv: ServiceRaw => srv }

  lazy val allServices: Map[String, ServiceRaw] =
    all(_.services)

  lazy val types: Map[String, Type] =
    parts.collectMap { case t: TypeRaw =>
      t.`type`
    }

  lazy val allTypes: Map[String, Type] =
    all(_.types)

  lazy val funcs: Map[String, FuncRaw] =
    parts.collectMap { case f: FuncRaw =>
      f
    }

  lazy val allFuncs: Map[String, FuncRaw] =
    all(_.funcs)

  lazy val values: Map[String, ValueRaw] =
    parts.collectMap { case c: ConstantRaw =>
      c.value
    }

  private def all[T](what: RawContext => Map[String, T], prefix: String = ""): Map[String, T] =
    abilities
      .foldLeft(what(this)) { case (ts, (k, v)) =>
        ts ++ v.all(what, k + ".")
      }
      .map(prefixFirst(prefix, _))

  def allValues: Map[String, ValueRaw] = all(_.values)

  def `type`(name: String): Option[StructType] =
    NonEmptyMap
      .fromMap(
        SortedMap.from(
          parts.parts.collect {
            case rp if declares(rp.name) || module.isEmpty => rp.name -> rp.rawPartType
          }.toList.toMap
        )
      )
      .map(StructType(name, _))
}

object RawContext {
  val blank: RawContext = RawContext()
}