package tinycc.common.transform

import tinycc.common.ProgramTransform
import tinycc.common.ir.IrProgram
import tinycc.util.Profiler.profile

class DeadFunElimination extends ProgramTransform[IrProgram] {
  override def transformProgram(program: IrProgram): Unit = profile("deadFunElimination", {
    program.funs.foreach(fun => {
      if(fun.uses.isEmpty) {
        log(s"removing dead $fun")
        fun.remove()
      }
    })
  })
}
