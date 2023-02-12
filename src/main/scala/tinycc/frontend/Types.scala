package tinycc.frontend

import scala.annotation.tailrec

object Types {
  sealed trait Ty {
    def isAssignableFrom(other: Ty): Boolean = this == other

    def isExplicitlyCastableFrom(other: Ty): Boolean = isAssignableFrom(other)

    def isComplete: Boolean
  }

  case object VoidTy extends Ty {
    override def toString: String = "void"

    override def isComplete: Boolean = true
  }

  /** This type should be returned if type analysis fails for the node. */
  case object ErrorTy extends Ty {
    override def isComplete: Boolean = false
  }

  sealed trait ScalarTy extends Ty {
    override def isComplete: Boolean = true
  }

  sealed trait ArithmeticTy extends ScalarTy {
    override def isAssignableFrom(other: Ty): Boolean =
      super.isAssignableFrom(other) || other.isInstanceOf[ArithmeticTy] // arithmetic types are implicitly compatible
  }

  implicit def arithmeticTyOrdering[T <: ArithmeticTy]: Ordering[T] = new Ordering[T] {
    private val typesByRank = Seq(CharTy, IntTy, DoubleTy)

    override def compare(x: T, y: T): Int = typesByRank.indexOf(x).compare(typesByRank.indexOf(y))
  }

  sealed trait IntegerTy extends ArithmeticTy {
    def maxValueLong: Long
  }

  case object CharTy extends ArithmeticTy with IntegerTy {
    override def toString: String = "char"

    override def maxValueLong: Long = 0xff
  }

  case object IntTy extends ArithmeticTy with IntegerTy {
    override def toString: String = "int"

    override def maxValueLong: Long = Long.MaxValue
  }

  case object DoubleTy extends ArithmeticTy {
    override def toString: String = "double"
  }

  sealed trait IndexableTy extends Ty {
    def baseTy: Ty

    def toPtr: PtrTy = PtrTy(baseTy)
  }

  object IndexableTy {
    def unapply(ty: IndexableTy): Option[Ty] = Some(ty.baseTy)
  }

  case class PtrTy(baseTy: Ty) extends IndexableTy with ScalarTy {
    override def toString: String = {
      @tailrec
      def resolveNestedPtr(ty: Ty, stars: String = "*"): (Ty, String) = ty match {
        case PtrTy(baseTy) => resolveNestedPtr(baseTy, stars + "*")
        case ty => (ty, stars)
      }

      resolveNestedPtr(baseTy) match {
        case (funTy: FunTy, stars) =>
          s"${funTy.returnTy}($stars)(${funTy.argTys.map(_.toString).mkString(",")})"
        case (baseTy, stars) => s"$stars$baseTy"
      }
    }

    override def isAssignableFrom(other: Ty): Boolean = super.isAssignableFrom(other) || (other match {
      case IndexableTy(otherBaseTy) if otherBaseTy == baseTy || baseTy == VoidTy => true
      case _ => false
    })

    override def isExplicitlyCastableFrom(other: Ty): Boolean = super.isExplicitlyCastableFrom(other) || (other match {
      case IndexableTy(VoidTy) => true
      case _ => false
    })

    override def isComplete: Boolean = true

    override def toPtr: PtrTy = this
  }

  /** Static array */
  case class ArrayTy(elemTy: Ty, numElem: Int) extends IndexableTy {
    override def toString: String = s"$baseTy[$numElem]"

    override def baseTy: Ty = elemTy

    override def isComplete: Boolean = true
  }

  case class FunTy(returnTy: Ty, argTys: IndexedSeq[Ty]) extends Ty {
    override def toString: String = s"$returnTy(${argTys.map(_.toString).mkString(",")})"

    override def isComplete: Boolean = true
  }

  case class StructTy(symbol: Option[Symbol] = None, var fields: Option[IndexedSeq[(Ty, Symbol)]] = None) extends Ty {
    override def toString: String = {
      val s = symbol.map(" " + _).getOrElse("")
      val f = fields.map(" {" + _.map({ case (ty, name) =>
        s" $ty $name;"
      }).mkString + " }").getOrElse("")
      s"struct$s$f;"
    }

    override def isComplete: Boolean = fields.isDefined
  }
}
