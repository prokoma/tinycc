package tinycc.backend.t86

package object regalloc {
  implicit class T86BasicBlockCfgOps(that: T86BasicBlock)(implicit cfg: T86BasicBlockCfg) {
    def pred: Seq[T86BasicBlock] = cfg.getPred(that)

    def succ: Seq[T86BasicBlock] = cfg.getSucc(that)
  }
}
