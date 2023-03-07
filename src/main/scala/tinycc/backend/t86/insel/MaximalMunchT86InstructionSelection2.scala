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

class MaximalMunchT86InstructionSelection2(program: IrProgram) extends T86InstructionSelection(program) with T86TilingInstructionSelection {
  private val tileResults = mutable.Map.empty[(Var[_], Insn), Any]
  private val programBuilder = new T86ProgramBuilder(Some(program))

  private lazy val sortedRules = rules.sortBy(r => -r.rhs.size)

  def result(): Either[BackendException, T86Program] = {
    program.funs.foreach(tileIrFun)
    Right(programBuilder.result())
  }

  def tileIrFun(fun: IrFun): Unit = {
    val funBuilder = new T86FunBuilder(Some(fun))
    val argsMap = buildArgsMap(fun.argTys, 2)

    funBuilder.appendBlock(new T86BasicBlock(IndexedSeq(T86SectionLabel("text"))))

    fun.basicBlocks.foreach(bb => tileBasicBlock(bb, funBuilder, argsMap))

    //    val epilogueMarker = T86Comment("EPILOGUE_HERE")
    //    val backupRegs: mutable.Buffer[Operand] = mutable.Buffer.empty
    //
    //    val ctx = new Context {
    //      private var nextReg: Long = 1 // 0 is reserved for return value
    //      private var nextFreg: Long = 0
    //      private var nextLabel = 0
    //
    //      override def resolveVar[T](v: Var[AsmEmitter[T]], insn: Insn): T = tileResults((v, insn)).asInstanceOf[T]
    //
    //      override def emit(el: T86ListingElement): Unit =
    //        codeBuilder += el
    //
    //      override def freshReg(): Operand.Reg = {
    //        nextReg += 1
    //        val reg = Operand.BasicReg(nextReg - 1)
    //        backupRegs += reg
    //        reg
    //      }
    //
    //      override def freshFReg(): Operand.FReg = {
    //        nextFreg += 1
    //        val reg = Operand.BasicFReg(nextFreg - 1)
    //        backupRegs += reg
    //        reg
    //      }
    //
    //      override def resolveAllocL(insn: AllocLInsn): Operand.MemRegImm = {
    //        val offset = localsMap.getOrElseUpdate(insn, {
    //          localsSize += getSizeWords(insn.varTy)
    //          localsSize
    //        })
    //        Operand.MemRegImm(Operand.BP, -offset)
    //      }
    //
    //      override def resolveAllocG(insn: AllocGInsn): Operand.MemImm = {
    //        val offset = globalsMap.getOrElseUpdate(insn, {
    //          val oldSize = globalsSize
    //          globalsSize += getSizeWords(insn.varTy)
    //          oldSize
    //        })
    //        Operand.MemImm(offset)
    //      }
    //
    //      /** Can return either MemRegImm or Reg depending on calling conventions. */
    //      override def resolveLoadArg(insn: LoadArgInsn): Operand = {
    //        val offset = argsMap(insn.index)
    //        Operand.MemRegImm(Operand.BP, offset + 2) // return address + stored BP
    //      }
    //
    //      override def emitFunEpilogue(): Unit =
    //        codeBuilder += epilogueMarker
    //
    //      /** Can return either MemRegImm or Reg depending on calling conventions. */
    //      override def resolveRetValueCallee(): Operand =
    //        Operand.BasicReg(0)
    //
    //      override def resolveRetValueCaller(): Operand =
    //        Operand.BasicReg(0)
    //
    //      override def freshLabel(): T86Label = {
    //        nextLabel += 1
    //        T86Label(fun.name + "$tmp" + (nextLabel - 1))
    //      }
    //    }
    //
    //    fun.basicBlocks.foreach(bb => tileBasicBlock(bb, ctx))
    //
    //    val prologueBuilder = Seq.newBuilder[T86ListingElement]
    //    prologueBuilder += T86Label(fun.name)
    //    prologueBuilder += T86Insn(PUSH, Operand.BP)
    //    prologueBuilder += T86Insn(MOV, Operand.BP, Operand.SP)
    //    prologueBuilder += T86Insn(SUB, Operand.SP, Operand.Imm(localsSize))
    //    // TODO: if arguments are passed via registers, copy them to fresh registers
    //    backupRegs.foreach({
    //      case reg: Operand.Reg =>
    //        prologueBuilder += T86Insn(PUSH, reg)
    //      case freg: Operand.FReg =>
    //        prologueBuilder += T86Insn(FPUSH, freg)
    //      case op =>
    //        throw new IllegalArgumentException(op.toString)
    //    })
    //
    //    val epilogueBuilder = Seq.newBuilder[T86ListingElement]
    //    backupRegs.reverse.foreach({
    //      case reg: Operand.Reg =>
    //        epilogueBuilder += T86Insn(POP, reg)
    //      case freg: Operand.FReg =>
    //        epilogueBuilder += T86Insn(FPOP, freg)
    //      case op =>
    //        throw new IllegalArgumentException(op.toString)
    //    })
    //    epilogueBuilder += T86Insn(MOV, Operand.SP, Operand.BP)
    //    epilogueBuilder += T86Insn(POP, Operand.BP)
    //    epilogueBuilder += T86Insn(RET)
    //
    //    val epilogue = epilogueBuilder.result()
    //    prologueBuilder.result() ++ codeBuilder.result().flatMap {
    //      case elem if elem == epilogueMarker => epilogue
    //      case elem => Seq(elem)
    //    }

    programBuilder.appendFun(funBuilder.result())
  }

  def tileBasicBlock(bb: BasicBlock, funBuilder: T86FunBuilder, argsMap: IndexedSeq[Operand.MemRegImm]): Unit = {
    val bodyBuilder = IndexedSeq.newBuilder[T86ListingElement]

    val ctx = new Context {
      private val labelNameGen = new NameGen(bb.uniqueName + "$")

      override def resolveVar[T](v: Var[AsmEmitter[T]], insn: Insn): T = tileResults((v, insn)).asInstanceOf[T]

      override def emit(el: T86ListingElement): Unit = bodyBuilder += el

      override def freshLabel(prefix: String): T86Label = T86Label(labelNameGen(prefix))

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