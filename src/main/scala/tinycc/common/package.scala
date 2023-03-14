package tinycc

package object common {
  implicit class CfgOps[T](that: T)(implicit cfg: Cfg[T]) {
    def pred: Seq[T] = cfg.getPred(that)

    def succ: Seq[T] = cfg.getSucc(that)
  }
}
