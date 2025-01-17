package tinycc.backend.t86

import tinycc.common.ProgramTransform

/** Insert function prologue and epilogue, remove markers. */
class T86FunProcessor extends ProgramTransform[T86Program] {

  import tinycc.backend.t86.T86Opcode._

  override def transformProgram(program: T86Program): Unit =
    program.funs.foreach(transformFun)

  def transformFun(fun: T86Fun): Unit = {
    val prologueBuilder = Seq.newBuilder[T86Insn]
    prologueBuilder += T86Insn(PUSH, Operand.BP)
    prologueBuilder += T86Insn(MOV, Operand.BP, Operand.SP)
    if(fun.localsSize > 0)
      prologueBuilder += T86Insn(SUB, Operand.SP, Operand.Imm(fun.localsSize))
    val prologue = prologueBuilder.result()

    val epilogueBuilder = Seq.newBuilder[T86Insn]
    epilogueBuilder += T86Insn(MOV, Operand.SP, Operand.BP)
    epilogueBuilder += T86Insn(POP, Operand.BP)
    val epilogue = epilogueBuilder.result()

    fun.basicBlocks.foreach(bb => {
      bb.body = bb.body.flatMap({
        case T86SpecialLabel.FunPrologueMarker => prologue
        case T86SpecialLabel.FunEpilogueMarker => epilogue
        case T86SpecialLabel.CallPrologueMarker => Seq.empty
        case T86SpecialLabel.CallEpilogueMarker => Seq.empty

        case elem => Seq(elem)
      })
    })
  }
}