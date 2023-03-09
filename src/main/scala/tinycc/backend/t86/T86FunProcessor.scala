package tinycc.backend.t86

/** Insert function prologue and epilogue, remove markers. */
class T86FunProcessor(program: T86Program) {

  import tinycc.backend.t86.T86Opcode._

  def result(): T86Program = {
    program.funs.foreach(processFun)
    program
  }

  def processFun(fun: T86Fun): Unit = {
    val prologueBuilder = Seq.newBuilder[T86Insn]
    prologueBuilder += T86Insn(PUSH, Operand.BP)
    prologueBuilder += T86Insn(MOV, Operand.BP, Operand.SP)
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

        case elem => Seq(elem)
      })
    })
  }
}