package tinycc.backend.t86.insel

import tinycc.backend.BackendException
import tinycc.backend.t86.T86Utils.buildArgsMap
import tinycc.backend.t86._
import tinycc.common.ir._
import tinycc.util.NameGen

import scala.collection.mutable

// we need to convert an IrProgram into list of T86 instructions with virtual registers
// at first, we are doing instruction selection locally for each block - the only exceptions are allocl and allocg, but those don't emit any code
// so, take each basic block and loop over instructions in reverse
// - try to match tiles, now the tiles contain variables (register types) - what now?
//   - currently all tiles take and return values in ordinary registers, so that shouldn't be a problem
//   - later - match tiles using DP in the orther they are writeen in basic block (bottom up data flow),
//      group matches by result type and store the best for each type
// each instruction now has a matched tile, but not all of them will be used
// loop over instructions in reverse order and call code gen functions

// if a nonterminal is accessed, call code gen for that instruction
// mark all processed instructions/tiles as visited and skip already visited insns in the loop
// hrm, this won't work, because there can be multiple interleaved df trees in one basic block and we need to keep the order of execution
//   - reordering of ir insns should be part of middleend

// so loop again over the basic block in reverse and examine the chosen tiles
// mark all interior instructions as covered, those are then skipped in the loop
// mark the root and all leafs (vars) as required

// ok, now ctx.emit() has been called for each instruction in order we have ASM code for a basic block
// process a whole function like this, now we know about all allocl's and allocg's
// the result of this thing is a vector of instructions for each basic block
// use labels instead of patching
// context is reset for each basic block

// split selecting of tiles from codegen
// result of instruction selection is T86Program with T86Funs linked with the corresponding ir instructions
// T86Funs don't have prologue and epilogue yet, there are placeholder labels for that
// we need some way to create Context from T86ProgramBuilder and T86FunBuilder
// some way to keep track of labels
// calling conventions

class MaximalMunchT86InstructionSelection(program: IrProgram) extends T86InstructionSelection(program) with T86TilingInstructionSelection {
  private val tileResults = mutable.Map.empty[(Var[_], Insn), Any]
  private val programBuilder = new T86ProgramBuilder(Some(program))

  private lazy val sortedRules = rules.sortBy(r => (-r.rhs.size, r.rhs.cost)) // sort first by size (descending) then by cost ascending

  def result(): T86Program = {
    program.funs.foreach(tileIrFun)
    programBuilder.result()
  }

  def tileIrFun(fun: IrFun): Unit = {
    val funBuilder = new T86FunBuilder(Some(fun))
    val argsMap = buildArgsMap(fun.argTys, 2)

    fun.basicBlocks.foreach(bb => tileBasicBlock(bb, funBuilder, argsMap))
    programBuilder.appendFun(funBuilder.result())
  }

  def tileBasicBlock(bb: BasicBlock, funBuilder: T86FunBuilder, argsMap: IndexedSeq[Operand.MemRegImm]): Unit = {
    val bodyBuilder = IndexedSeq.newBuilder[T86ListingElement]

    val ctx = new Context {
      private val labelNameGen = new NameGen

      override def resolveVar[T](v: Var[AsmEmitter[T]], insn: Insn): T = tileResults((v, insn)).asInstanceOf[T]

      override def emit(el: T86ListingElement): Unit = bodyBuilder += el

      override def freshLabel(name: String): T86Label = T86Label(bb.uniqueName + "$" + labelNameGen(name))

      override def freshReg(): Operand.Reg = funBuilder.freshReg()

      override def freshFReg(): Operand.FReg = funBuilder.freshFReg()

      override def resolveAllocL(insn: AllocLInsn): Operand.MemRegImm = funBuilder.resolveAllocL(insn)

      override def resolveAllocG(insn: AllocGInsn): Operand.MemImm = programBuilder.resolveAllocG(insn)

      /** Can return either MemRegImm or Reg depending on calling conventions. */
      override def resolveLoadArg(insn: LoadArgInsn): Operand = argsMap(insn.index)
    }

    val allCoveredInsns = mutable.Set.empty[Insn]
    val allRequiredInsns = mutable.Set.empty[(Var[_], Insn)]
    val tileMap = mutable.Map.empty[(Var[_], Insn), GenRule.Match[_]]

    ctx.emit(ctx.getBasicBlockLabel(bb))
    if (bb.isFunEntryBlock)
      ctx.emit(T86SpecialLabel.FunPrologueMarker)

    // instructions in a basic block are topologically sorted
    // greedily cover instructions with tiles
    bb.body.reverse.foreach(insn => {
      val requiredVars = variables.filter(v => allRequiredInsns.contains((v, insn)))

      if (requiredVars.nonEmpty) {
        if (requiredVars.size > 1 && insn.hasSideEffects)
          throw new BackendException(s"Insn $insn is required by multiple tiles with different types, but has side effects.")

        requiredVars.foreach(v => {
          val varRules = sortedRules.filter(_.v == v)
          varRules.view.flatMap(_(insn)).headOption match {
            case Some(m) =>
              tileMap((m.rule.v, insn)) = m
              allCoveredInsns ++= m.coveredInsns
              allRequiredInsns ++= m.requiredInsns

            case None =>
              throw new BackendException(s"Failed to cover $insn as $v (tried ${varRules.size} rules)")
          }
        })
      } else if (!allCoveredInsns.contains(insn)) {
        sortedRules.view.flatMap(_(insn)).headOption match {
          case Some(m) =>
            tileMap((m.rule.v, insn)) = m
            allCoveredInsns ++= m.coveredInsns
            allRequiredInsns ++= m.requiredInsns

          case None =>
            throw new BackendException(s"Failed to cover $insn (tried ${sortedRules.size} rules)")
        }
      }
    })

    // now loop in the program order and generate code for the matched tiles
    bb.body.foreach(insn => {
      variables.foreach(v => tileMap.get((v, insn)) match {
        case Some(m) =>
          tileResults((v, insn)) = m.value(ctx)

        case None =>
      })
    })

    funBuilder.appendBlock(new T86BasicBlock(bodyBuilder.result(), Some(bb)))
  }
}