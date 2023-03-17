package tinycc.backend

import tinycc.common.ir.{Insn, IrFun, IrTy}
import tinycc.util.Logging

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable, tryBreakable}

trait MaximalMunch extends TilingInstructionSelection with Logging {
  // sort first by size (descending) then by cost ascending
  private lazy val sortedRules: Seq[GenRule[_]] = rules.sortBy(r => (-r.rhs.size, r.rhs.cost))
  private lazy val sortedRulesByVariable: Map[Var[_], Seq[GenRule[_]]] = sortedRules.groupBy(_.variable)

  override def name: String = "MaximalMunch"

  override def getTileMapForFun(fun: IrFun): Map[Insn, GenRule.Match[_]] = {
    /** Instructions that can only ever be at the root of a tile. */
    var frozenInsns = Set.empty[Insn]
    val tileMap = mutable.Map.empty[Insn, GenRule.Match[_]]

    var didFreeze = false
    do breakable {
      tileMap.clear()
      didFreeze = false

      var allCoveredInsns = Set.empty[Insn]
      var allRequiredInsns = Map.empty[Insn, Var[_]]

      def selectTile(rules: Seq[GenRule[_]], insn: Insn): GenRule.Match[_] = {
        val insnMatches = rules.view.flatMap(_(insn)).filter(m => {
          // check that the tile doesn't cover any frozen insn (except the root)
          !m.coveredInsns.exists(i => i != insn && frozenInsns.contains(i))
        })

        insnMatches.find(m => {
          // check that for every required insn, we can supply a value of the specific type
          m.requiredInsns.forall({ case (nt, insn) => {
            val insnMatch = tileMap.get(insn)
            insnMatch.isEmpty || insnMatch.get.variable == nt
          }
          })
          // if that fails, accept anything, but that means we will have to do some casting
        }).orElse(insnMatches.headOption).getOrElse(throw new BackendException(s"Failed to select tile for $insn (${rules.size} rules)"))
      }

      def dfs(insn: Insn): Unit = {
        val filteredRules = allRequiredInsns.get(insn) match {
          case Some(variable) =>
            val prevVariable = tileMap.get(insn).map(_.variable)
            assert(prevVariable.isEmpty || prevVariable.get == variable || canCastFromTo(prevVariable.get, variable.asInstanceOf[AsmVar[_]]),
              s"cannot cover already covered $insn with different incompatible variable (prev: ${tileMap(insn).variable}, current: $variable)")
            sortedRulesByVariable(variable)

          case None => sortedRules
        }

        if (tileMap.contains(insn)) // return if this instruction is already at root of some tile
          return

        if (allCoveredInsns.contains(insn) && !canCoverByMultipleTiles(insn)) {
          // we need result of [[insn]], but it is already covered by an inner node of some tile
          // freeze the instruction, so it is at the root in the next attempt
          // there is only a constant finite amount of instructions, so this loop must end
          log(s"freezing $insn, because it was already covered by an inner node")
          frozenInsns += insn
          didFreeze = true
          break
        }

        val matchedTile = selectTile(filteredRules, insn)

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
    } while(didFreeze)

    tileMap.toMap
  }
}
