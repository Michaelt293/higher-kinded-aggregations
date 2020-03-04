trait Monoid[T] extends SemiGroup[T]
  def unit: T

object Monoid
  def apply[T](using m: Monoid[T]) = m

  given Monoid[String]
    def combine(x: String, y: String): String = x + y
    def unit: String = ""

  given numericMonoid[N](using N: Numeric[N]) as Monoid[N]
    def combine(x: N, y: N): N = N.plus(x, y)
    def unit: N = N.zero

  given optionMonoid[A](using M: Monoid[A]) as Monoid[Option[A]]
    def combine(x: Option[A], y: Option[A]): Option[A] =
      (x, y) match
        case (Some(x_), Some(y_)) => Some(M.combine(x_, y_))
        case (x_, y_) => x_.orElse(y_)

    def unit: Option[A] = None

  given listMonoid[A] as Monoid[List[A]]
    def combine(x: List[A], y: List[A]): List[A] =
      x ++ y

    def unit: List[A] = List.empty[A]

  given setMonoid[A] as Monoid[Set[A]]
    def combine(x: Set[A], y: Set[A]): Set[A] =
      x ++ y

    def unit: Set[A] = Set.empty[A]

  given tuple2Monoid[A, B](
    using
    MA: Monoid[A],
    MB: Monoid[B]
    ) as Monoid[(A, B)]
    def combine(x: (A, B), y: (A, B)): (A, B) =
      (MA.combine(x._1, y._1), MB.combine(x._2, y._2))

    def unit: (A, B) = (MA.unit, MB.unit)

  given tuple3Monoid[A, B, C](
    using
    MA: Monoid[A],
    MB: Monoid[B],
    MC: Monoid[C]
    ) as Monoid[(A, B, C)]
    def combine(x: (A, B, C), y: (A, B, C)): (A, B, C) =
      (MA.combine(x._1, y._1), MB.combine(x._2, y._2), MC.combine(x._3, y._3))

    def unit: (A, B, C) = (MA.unit, MB.unit, MC.unit)
