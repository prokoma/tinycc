package tinycc.backend.insel

import tinycc.backend.BackendException
import tinycc.common.ir.{Insn, IrFun, IrOpcode, IrTy}
import tinycc.util.Logging

import scala.collection.mutable

/** A greedy tiling instruction selector algorithm. Impleemnts the [[getTileMapForFun]] method on [[TilingInstructionSelection]] via a top-down pass over the dataflow graph. */
trait MaximalMunch extends TilingInstructionSelection with Logging {
  /** The ruleset itself is fairly large, so we split it by the matched instruction opcode. */
  private lazy val rulesByRootOp: Map[IrOpcode, Seq[GenRule[_]]] = {
    val res = mutable.Map.empty[IrOpcode, Seq[GenRule[_]]].withDefaultValue(Seq.empty)
    for (r <- rules; rootOp <- r.rhs.rootOps)
      res(rootOp) +:= r
    res.toMap
  }

  override def name: String = "MaximalMunch"

  override def getTileMapForFun(fun: IrFun): Map[Insn, GenRule.Match[_]] = {
    /** Instructions that can only ever be at the root of a tile. */
    val tileMap = mutable.Map.empty[Insn, GenRule.Match[_]]

    var allCoveredInsns = Set.empty[Insn]
    var allRequiredInsns = Map.empty[Insn, Var[_]]

    def selectTile(rules: Seq[GenRule[_]], insn: Insn): GenRule.Match[_] = {
      val insnMatches = rules.flatMap(_(insn)).sortBy(r => (-r.size, r.cost)) // sort first by size (descending) then by cost ascending

      insnMatches.find(m => {
        // check that for every required insn, we can supply a value of the specific type
        m.requiredInsns.forall({ case (nt, insn) => tileMap.get(insn).forall(_.variable == nt) })
        // if that fails, accept anything, but that means we will have to do some casting
      }).orElse(insnMatches.headOption).getOrElse(throw new BackendException(s"Failed to select tile for $insn (${rules.size} rules)"))
    }

    def dfs(insn: Insn): Unit = {
      val filteredRules = allRequiredInsns.get(insn) match {
        case Some(variable) =>
          val prevVariable = tileMap.get(insn).map(_.variable)
          assert(prevVariable.isEmpty || prevVariable.get == variable || canCastFromTo(prevVariable.get, variable.asInstanceOf[AsmVar[_]]),
            s"cannot cover already covered $insn with different incompatible variable (prev: ${tileMap(insn).variable}, current: $variable)")
          rulesByRootOp(insn.op).filter(_.variable == variable)

        case None => rulesByRootOp(insn.op)
      }

      if (tileMap.contains(insn)) // return if this instruction is already at root of some tile, the type is checked above
        return

      val matchedTile = selectTile(filteredRules, insn)

//        log(s"covered $insn as ${matchedTile.variable}")
      tileMap(insn) = matchedTile
      allCoveredInsns ++= matchedTile.coveredInsns
      allRequiredInsns ++= matchedTile.requiredInsns.map(_.swap)
      matchedTile.requiredInsns.foreach({ case (_, insn) => dfs(insn) })
    }

    // run first pass over "root" instructions, whose result will never be used
    // we do this, so we don't constrain instructions, whose result will be used, to a particular register type
    // the loop order here doesn't matter
    fun.insns.reverse.foreach(insn => {
      if (!allCoveredInsns.contains(insn) && (insn.resultTy == IrTy.VoidTy || insn.uses.isEmpty))
        dfs(insn)
    })

    // now do a second pass over not yet covered instructions
    // loop in reverse, so there is less chance of a conflict (see above)
    fun.insns.reverse.foreach(insn => {
      if (!allCoveredInsns.contains(insn))
        dfs(insn)
    })

    tileMap.toMap
  }
}
