package tinycc.common.transform

import tinycc.common.ProgramTransform
import tinycc.common.analysis.dataflow.LiveInsnAnalysis
import tinycc.common.ir.{InsnDfg, IrProgram}
import tinycc.util.Profiler.profile

class DeadInsnElimination extends ProgramTransform[IrProgram] {
  override def transformProgram(program: IrProgram): Unit = profile("deadInsnElimination", {
    val liveInsns = new LiveInsnAnalysis(InsnDfg(program)).result()

    program.insns.foreach(insn => {
      if(!liveInsns.contains(insn)) {
        log(s"removing dead $insn")
        insn.remove(true)
      }
    })
  })
}
