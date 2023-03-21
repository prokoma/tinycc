package tinycc.frontend

import scala.annotation.tailrec

object Types {
  sealed trait Ty {
    def isAssignableFrom(other: Ty): Boolean = this == other

    def isExplicitlyCastableFrom(other: Ty): Boolean = isAssignableFrom(other)

    def complete: Boolean
  }

  case object VoidTy extends Ty {
    override def toString: String = "void"

    override def complete: Boolean = true
  }

  /** Scalar types are arithmetic or pointer types (from C99). Scalar types do implicitly cast to bool. */
  sealed trait ScalarTy extends Ty {
    override def complete: Boolean = true
  }

  /** Arithmetic types are char, int and double. They do implicitly convert between each other. */
  sealed trait ArithmeticTy extends ScalarTy {
    override def isAssignableFrom(other: Ty): Boolean =
      super.isAssignableFrom(other) || other.isInstanceOf[ArithmeticTy] // arithmetic types are implicitly compatible
  }

  object ArithmeticTy {
    def getResultTy(leftTy: ArithmeticTy, rightTy: ArithmeticTy): ArithmeticTy = {
      // CharTy < IntTy < DoubleTy, we need to return the larger of leftTy and rightTy
      if(leftTy == DoubleTy || rightTy == DoubleTy)
        DoubleTy
      else if(leftTy == IntTy || rightTy == IntTy)
        IntTy
      else // leftTy == CharTy && rightTy == CharTy
        CharTy
    }
  }

  sealed trait IntegerTy extends ArithmeticTy {
    /** Bitmask of valid bits in 64-bit long */
    def longBitmask: Long
  }

  case object CharTy extends ArithmeticTy with IntegerTy {
    override def toString: String = "char"

    override def longBitmask: Long = 0xff
  }

  case object IntTy extends ArithmeticTy with IntegerTy {
    override def toString: String = "int"

    override def longBitmask: Long = ~(0L)

    override def isExplicitlyCastableFrom(other: Ty): Boolean =
      super.isExplicitlyCastableFrom(other) || other.isInstanceOf[PtrTyBase] // 64 bit pointers can be explicitly casted to 64 bit int
  }

  case object DoubleTy extends ArithmeticTy {
    override def toString: String = "double"
  }

  sealed trait PtrTyBase extends ScalarTy {
    def baseTy: Ty

    def toPtr: PtrTy = PtrTy(baseTy)
  }

  object PtrTyBase {
    def unapply(ty: PtrTyBase): Option[Ty] = Some(ty.baseTy)
  }

  case class PtrTy(baseTy: Ty) extends PtrTyBase {
    override def toString: String = {
      @tailrec
      def resolveNestedPtr(ty: Ty, stars: String = "*"): (Ty, String) = ty match {
        case PtrTy(baseTy) => resolveNestedPtr(baseTy, stars + "*")
        case ty => (ty, stars)
      }

      resolveNestedPtr(baseTy) match {
        case (funTy: FunTy, stars) =>
          s"${funTy.returnTy}($stars)(${funTy.argTys.map(_.toString).mkString(",")})"
        case (baseTy, stars) => s"$baseTy$stars"
      }
    }

    override def isAssignableFrom(other: Ty): Boolean = super.isAssignableFrom(other) || (other match {
      case PtrTyBase(otherBaseTy) if otherBaseTy == baseTy || baseTy == VoidTy => true
      case _ => false
    })

    override def isExplicitlyCastableFrom(other: Ty): Boolean = super.isExplicitlyCastableFrom(other) || (other match {
      case _: PtrTyBase | IntTy => true
      case _ => false
    })

    override def complete: Boolean = true

    override def toPtr: PtrTy = this
  }

  /** A pointer to the beginning of an array with a known size. */
  case class ArrayTy(elemTy: Ty, numElem: Int) extends PtrTyBase {
    override def toString: String = s"$baseTy[$numElem]"

    override def baseTy: Ty = elemTy

    override def complete: Boolean = true
  }

  case class FunTy(returnTy: Ty, argTys: IndexedSeq[Ty]) extends Ty {
    override def toString: String = s"$returnTy(${argTys.map(_.toString).mkString(",")})"

    override def complete: Boolean = true
  }

  case class StructTy(symbol: Option[Symbol] = None, var fields: Option[IndexedSeq[(Ty, Symbol)]] = None) extends Ty {
    override def toString: String = {
      val s = symbol.map(" " + _.name).getOrElse("")
//      val f = fields.map(" {" + _.map({ case (ty, name) =>
//        s" $ty $name;"
//      }).mkString + " }").getOrElse("")
//      s"struct$s$f;"
      s"struct$s"
    }

    override def complete: Boolean = fields.isDefined
  }
}
