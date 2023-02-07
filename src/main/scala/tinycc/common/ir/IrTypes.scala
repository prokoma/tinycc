package tinycc.common.ir

sealed trait IrTy extends Product with Serializable {
  def sizeBytes: Int
}

sealed trait ScalarTy

object IrTy {
  case object VoidTy extends IrTy {
    override def toString: String = "void"

    override def sizeBytes: Int = 0
  }

  case object Int64Ty extends IrTy {
    override def toString: String = "i64"

    override def sizeBytes: Int = 8
  }

  case object DoubleTy extends IrTy {
    override def toString: String = "double"

    override def sizeBytes: Int = 8
  }

  case object PtrTy extends IrTy {
    override def toString: String = "ptr"

    override def sizeBytes: Int = 8
  }

  case class StructTy(fields: IndexedSeq[IrTy]) extends IrTy {
    override def toString: String = s"struct {${fields.map(f => s" $f").mkString(",")} }"

    override def sizeBytes: Int = fields.foldLeft(0)(_ + _.sizeBytes)
  }

  case class ArrayTy(baseTy: IrTy, numElem: Int) extends IrTy {
    override def toString: String = s"$baseTy[$numElem]"

    override def sizeBytes: Int = baseTy.sizeBytes * numElem
  }
}