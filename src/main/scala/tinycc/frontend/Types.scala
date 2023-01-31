package tinycc.frontend

import tinycc.frontend.Types.IntegerTy

object Types {

  sealed trait CastMode

  object CastModes {

    case object Direct extends CastMode

    case object Implicit extends CastMode

    case object Explicit extends CastMode
  }

  sealed trait Ty {
    def sizeCells: Int

    /** How many bytes to allocate for local & global variables. */
    def allocSizeCells: Int = sizeCells

    def getCastModeFrom(other: Ty): Option[CastMode]

    def isAssignableFrom(other: Ty): Boolean = getCastModeFrom(other) match {
      case Some(CastModes.Direct | CastModes.Implicit) => true
      case _ => false
    }

    def isComplete: Boolean
  }

  sealed trait ScalarTy extends Ty

  sealed trait PODTy extends Ty {
    override def isComplete: Boolean = true
  }

  case object VoidTy extends PODTy {
    val sizeCells: Int = 0

    override def toString: String = "void"

    override def getCastModeFrom(other: Ty): Option[CastMode] = other match {
      case _ if other == this => Some(CastModes.Direct)
      case _ => None
    }
  }

  /** This type should be returned if type analysis fails for the node. */
  case object ErrorTy extends Ty {
    override def sizeCells: Int = ???

    override def getCastModeFrom(other: Ty): Option[CastMode] = ???

    override def isComplete: Boolean = ???
  }

  sealed trait ArithmeticTy extends PODTy with ScalarTy

  sealed trait IntegerTy extends ArithmeticTy

  case object CharTy extends ArithmeticTy with IntegerTy {
    override def sizeCells: Int = ???

    override def toString: String = "char"

    override def getCastModeFrom(other: Ty): Option[CastMode] = ???
  }

  case object IntTy extends ArithmeticTy with IntegerTy  {
    val sizeCells: Int = 1

    override def toString: String = "int"

    override def getCastModeFrom(other: Ty): Option[CastMode] = other match {
      case _ if other == this => Some(CastModes.Direct)
      case _: PtrTy => Some(CastModes.Explicit)
      case _ => None
    }
  }

  case object DoubleTy extends ArithmeticTy {
    override def sizeCells: Int = ???

    override def toString: String = "double"

    override def getCastModeFrom(other: Ty): Option[CastMode] = ???
  }

  implicit def arithmeticTyOrdering[T <: ArithmeticTy]: Ordering[T] = new Ordering[T] {
    private val typesByRank = Seq(CharTy, IntTy, DoubleTy)

    override def compare(x: T, y: T): Int = typesByRank.indexOf(x).compare(typesByRank.indexOf(y))
  }

  sealed trait IndexableTyBase extends Ty {
    val sizeCells = 1

    def baseTy: Ty
  }

  case class PtrTy(baseTy: Ty) extends IndexableTyBase with ScalarTy {
    override def toString: String = s"*$baseTy" // TODO: correctly print function ptr

    override def getCastModeFrom(other: Ty): Option[CastMode] = other match {
      case _ if other == this => Some(CastModes.Direct)
      case IntTy => Some(CastModes.Explicit)
      case PtrTy(otherTargetTy) if baseTy == VoidTy || otherTargetTy == VoidTy => Some(CastModes.Implicit)
      case ArrayTy(elemTy, _) if baseTy == elemTy => Some(CastModes.Implicit)
      case _ => None
    }

    override def isComplete: Boolean = true
  }

  /** Static array */
  case class ArrayTy(baseTy: Ty, numElem: Int) extends IndexableTyBase {
    override val allocSizeCells: Int = baseTy.allocSizeCells * numElem

    override def toString: String = s"$baseTy[$numElem]"

    override def getCastModeFrom(other: Ty): Option[CastMode] = other match {
      case _ if other == this => Some(CastModes.Direct)
      case _ => None
    }

    override def isComplete: Boolean = true
  }

  case class FunTy(returnTy: Ty, argTys: IndexedSeq[Ty]) extends Ty {
    val sizeCells: Int = 1

    override def toString: String = s"$returnTy(${argTys.map(_.toString).mkString(",")})"

    override def getCastModeFrom(other: Ty): Option[CastMode] = other match {
      case _ if other == this => Some(CastModes.Direct)
      case _ => None
    }

    override def isComplete: Boolean = true
  }

  case class StructTy(symbol: Option[Symbol] = None, var fields: Option[IndexedSeq[(Ty, Symbol)]] = None) extends Ty {
    override def sizeCells: Int = ???

    override def toString: String = {
      val s = symbol.map(" " + _).getOrElse("")
      val f = fields.map(" {" + _.map({ case (ty, name) =>
        s" $ty $name;"
      }).mkString + " }").getOrElse("")
      s"struct$s$f;"
    }

    override def getCastModeFrom(other: Ty): Option[CastMode] = ???

    override def isComplete: Boolean = fields.isDefined
  }

}
