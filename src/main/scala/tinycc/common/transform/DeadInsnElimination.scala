package tinycc.common.transform

import tinycc.common.ProgramTransform
import tinycc.common.analysis.dataflow.LiveInsnAnalysis
import tinycc.common.ir.{IrProgram, graph}
import tinycc.common.ir.graph.InsnDfg
import tinycc.util.Profiler.profile

class DeadInsnElimination extends ProgramTransform[IrProgram] {
  override def transformProgram(program: IrProgram): Unit = profile("deadInsnElimination", {
    val liveInsns = new LiveInsnAnalysis(graph.InsnDfg(program)).result()

    program.insns.foreach(insn => {
      if(!liveInsns.contains(insn)) {
        log(s"removing dead $insn")
        insn.remove(true)
      }
    })
  })
}
