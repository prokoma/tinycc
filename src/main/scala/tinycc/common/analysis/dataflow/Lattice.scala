package tinycc.common.analysis.dataflow

trait Lattice[T] {
  type Elem = T

  def top: T

  def bot: T

  // ⊔
  def lub(x: T, y: T): T

  // ⊓
  def glb(x: T, y: T): T
}

object Lattice {
  type PowersetLattice[T] = Lattice[Set[T]]

  type MapLattice[K, V] = Lattice[Map[K, V]]

  def lazyPowersetLattice[T](domain: => Set[T]): Lattice[Set[T]] = new Lattice[Set[T]] {
    override lazy val top: Elem = domain

    override def bot: Elem = Set.empty

    override def lub(x: Elem, y: Elem): Elem = x.union(y)

    override def glb(x: Elem, y: Elem): Elem = x.intersect(y)
  }

  def lazyMapLattice[K, V](_domain: => Iterable[K], sub: Lattice[V]): Lattice[Map[K, V]] = new Lattice[Map[K, V]] {
    private lazy val domain = _domain

    override lazy val top: Elem = domain.map(key => key -> sub.top).toMap

    override lazy val bot: Elem = domain.map(key => key -> sub.bot).toMap

    override def lub(x: Elem, y: Elem): Elem = {
      require(x.keys.size == y.keys.size)
      x.transform((key, xVal) => sub.lub(xVal, y(key)))
    }

    override def glb(x: Elem, y: Elem): Elem = {
      require(x.keys.size == y.keys.size)
      x.transform((key, xVal) => sub.glb(xVal, y(key)))
    }
  }

  def invLattice[T](sub: Lattice[T]): Lattice[T] = new Lattice[T] {
    override def top: Elem = sub.bot

    override def bot: Elem = sub.top

    override def lub(x: Elem, y: Elem): Elem = sub.glb(x, y)

    override def glb(x: Elem, y: Elem): Elem = sub.lub(x, y)
  }
}