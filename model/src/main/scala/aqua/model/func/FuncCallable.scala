package aqua.model.func

import aqua.model.func.raw.{AssignmentTag, CallArrowTag, FuncOp, RawTag}
import aqua.model.{Model, ValueModel, VarModel}
import aqua.types.{ArrowType, Type}
import cats.Eval
import cats.data.Chain
import cats.free.Cofree
import wvlet.log.Logger

case class FuncCallable(
  funcName: String,
  body: FuncOp,
  args: ArgsDef,
  ret: Option[(ValueModel, Type)],
  capturedArrows: Map[String, FuncCallable],
  capturedValues: Map[String, ValueModel]
) extends Model {

  private val logger = Logger.of[FuncCallable]
  import logger._

  def arrowType: ArrowType =
    ArrowType(
      args.types,
      ret.map(_._2)
    )

  def findNewNames(forbidden: Set[String], introduce: Set[String]): Map[String, String] =
    (forbidden intersect introduce).foldLeft(Map.empty[String, String]) { case (acc, name) =>
      acc + (name -> LazyList
        .from(0)
        .map(name + _)
        .dropWhile(n => forbidden(n) || introduce(n) || acc.contains(n))
        .head)
    }

  // Apply a callable function, get its fully resolved body & optional value, if any
  def resolve(
    call: Call,
    arrows: Map[String, FuncCallable],
    forbiddenNames: Set[String]
  ): Eval[(FuncOp, Option[ValueModel])] = {

    debug("Call: " + call)
    println("====================================================================")
    println("Call: " + call)
    println("func name: " + funcName)
    println("captured values: " + capturedValues)
    println("captured arrows: " + capturedArrows)

    // Collect all arguments: what names are used inside the function, what values are received
    val argsFull = args.call(call)
    // DataType arguments
    val argsToData = argsFull.dataArgs
    println("args to data: " + argsToData)
    // Arrow arguments: expected type is Arrow, given by-name
    val argsToArrows = argsFull.arrowArgs(arrows)
    println("args to arrows: " + argsToArrows)

    val argsToDataShouldRename = findNewNames(forbiddenNames, argsToData.keySet)
    println("argsToDataShouldRename:")
    println(argsToDataShouldRename)
    val newArgsToData = argsToData.map { case (k, v) =>
      (argsToDataShouldRename.getOrElse(k, k), v)
    }

    // Going to resolve arrows: collect them all. Names should never collide: it's semantically checked
    val allArrows = capturedArrows ++ argsToArrows

    val arrowsShouldRename = findNewNames(forbiddenNames, allArrows.keySet)
    println("argsToArrowShouldRename:")
    println(arrowsShouldRename)
    val newAllArrows = allArrows.map { case (k, v) =>
      (arrowsShouldRename.getOrElse(k, k), v)
    }

    // Substitute arguments (referenced by name and optional lambda expressions) with values
    val treeWithValues =
      body.rename(arrowsShouldRename ++ argsToDataShouldRename).resolveValues(newArgsToData)

    // Function body on its own defines some values; collect their names
    val treeDefines = treeWithValues.definesVarNames.value -- call.exportTo.map(_.name)

    println("tree with values:")
    println(treeWithValues.definesVarNames.value)
    println("forbidden:")
    println(forbiddenNames)
    println("tree defines:")
    println(treeDefines)
    // We have some names in scope (forbiddenNames), can't introduce them again; so find new names
    val shouldRename = findNewNames(
      forbiddenNames,
      treeDefines
    )
    println("should rename:")
    println(shouldRename)
    // If there was a collision, rename exports and usages with new names
    val treeRenamed =
      if (shouldRename.isEmpty) treeWithValues else treeWithValues.rename(shouldRename)

    val allShouldRename = shouldRename ++ arrowsShouldRename ++ argsToDataShouldRename

    // Result could be derived from arguments, or renamed; take care about that
    println("RESOLVE RESULT:")
    println(ret)
    val result = ret.map(_._1).map(_.resolveWith(newArgsToData)).map {
      case v: VarModel if allShouldRename.contains(v.name) => v.copy(allShouldRename(v.name))
      case v => v
    }

    // Now, substitute the arrows that were received as function arguments
    FuncOp
      .traverseA(
        // Use the new op tree (args are replaced with values, names are unique & safe)
        treeRenamed.tree,
        // Accumulator: all used names are forbidden, if we set any more names -- forbid them as well
        (forbiddenNames ++ allShouldRename.values ++ treeDefines) ->
          // Functions may export variables, so collect them
          capturedValues
      ) {
        case ((noNames, resolvedExports), tag @ AssignmentTag(value, assignTo)) =>
          (
            noNames,
            resolvedExports + (assignTo -> value.resolveWith(resolvedExports))
          ) -> Cofree[Chain, RawTag](
            tag.mapValues(_.resolveWith(resolvedExports)),
            Eval.now(Chain.empty)
          )

        case ((noNames, resolvedExports), CallArrowTag(fName, c)) if newAllArrows.contains(fName) =>
          // Apply arguments to a function – recursion
          val callResolved = c.mapValues(_.resolveWith(resolvedExports))
          val possibleArrowNames = callResolved.args.collect { case VarModel(m, _: ArrowType, _) =>
            m
          }.toSet

          println("POSSIBLE ARROW NAMES:")
          println(possibleArrowNames)

          val (appliedOp, value) =
            newAllArrows(fName)
              .resolve(
                callResolved,
                newAllArrows.view.filterKeys(possibleArrowNames).toMap,
                noNames
              )
              .value

          // Function defines new names inside its body – need to collect them
          // TODO: actually it's done and dropped – so keep and pass it instead
          val newNames = appliedOp.definesVarNames.value
          // At the very end, will need to resolve what is used as results with the result values
          (
            noNames ++ newNames,
            resolvedExports ++ c.exportTo.map(_.name).zip(value)
          ) -> appliedOp.tree
        case (acc @ (_, resolvedExports), tag) =>
          tag match {
            case CallArrowTag(fn, _) if !newAllArrows.contains(fn) =>
              error(s"UNRESOLVED $fn in $funcName, skipping, will become (null) in AIR!")
            case _ =>
          }

          // All the other tags are already resolved and need no substitution
          acc -> Cofree[Chain, RawTag](
            tag.mapValues(_.resolveWith(resolvedExports)),
            Eval.now(Chain.empty)
          )
      }
      .map { case ((_, resolvedExports), b) =>
        // If return value is affected by any of internal functions, resolve it
        FuncOp(b) -> result.map(_.resolveWith(resolvedExports))
      }
  }

}
