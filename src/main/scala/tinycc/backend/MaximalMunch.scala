package tinycc.backend

import tinycc.common.ir.{Insn, IrFun, IrTy}

import scala.collection.mutable

trait MaximalMunch extends TilingInstructionSelection {
  // sort first by size (descending) then by cost ascending
  private lazy val sortedRules: Seq[GenRule[_]] = rules.sortBy(r => (-r.rhs.size, r.rhs.cost))
  private lazy val sortedRulesByVariable: Map[Var[_], Seq[GenRule[_]]] = sortedRules.groupBy(_.variable)

  override def getTileMapForFun(fun: IrFun): Map[Insn, GenRule.Match[_]] = {
    /** Instructions that can only ever be at the root of a tile. */
    val frozenInsns = Set.empty[Insn]
    var allCoveredInsns = Set.empty[Insn]
    var allRequiredInsns = Map.empty[Insn, Var[_]]

    val tileMap = mutable.Map.empty[Insn, GenRule.Match[_]]

    def selectTile(rules: Seq[GenRule[_]], insn: Insn): GenRule.Match[_] =
      rules.view.flatMap(_(insn)).find(m => {
        !m.coveredInsns.exists(i => i != insn && frozenInsns.contains(i))
      }).getOrElse(throw new BackendException(s"Failed to select tile for $insn (${rules.size} rules)"))

    def dfs(insn: Insn): Unit = {
      val filteredRules = allRequiredInsns.get(insn) match {
        case Some(variable) =>
          assert(!tileMap.contains(insn) || tileMap(insn).variable == variable, s"cannot cover already covered $insn with different variable")
          sortedRulesByVariable(variable)

        case None => sortedRules
      }

      if (tileMap.contains(insn)) // return if this instruction is already at root of some tile
        return

      assert(!allCoveredInsns.contains(insn) || canCoverByMultipleTiles(insn), s"cannot cover already covered $insn")
      // TODO: freeze and try again

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

    tileMap.toMap
  }
}
