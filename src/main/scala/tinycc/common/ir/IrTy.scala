package tinycc.common.ir

sealed trait IrTy extends Product with Serializable {
  def sizeWords: Int
}

object IrTy {
  case object VoidTy extends IrTy {
    override def toString: String = "void"

    override def sizeWords: Int = 0
  }

  case object Int64Ty extends IrTy {
    override def toString: String = "i64"

    override def sizeWords: Int = 1
  }

  case object DoubleTy extends IrTy {
    override def toString: String = "double"

    override def sizeWords: Int = 1
  }

  def PtrTy: IrTy = Int64Ty

  case class StructTy(fields: IndexedSeq[IrTy]) extends IrTy {
    override def toString: String = s"struct {${fields.map(f => s" $f").mkString(",")} }"

    override def sizeWords: Int = fields.map(_.sizeWords).sum

    def getFieldOffsetWords(fieldIndex: Int): Int = {
      require(fieldIndex < fields.size)
      fields.take(fieldIndex).map(_.sizeWords).sum
    }
  }

  case class ArrayTy(baseTy: IrTy, numElem: Int) extends IrTy {
    override def toString: String = s"$baseTy[$numElem]"

    override def sizeWords: Int = baseTy.sizeWords * numElem
  }
}