trait Monoid[T] extends SemiGroup[T]
  def unit: T

object Monoid
  def apply[T](using m: Monoid[T]) = m

  given Monoid[String]
    def combine(x: String, y: String): String = x + y
    def unit: String = ""

  given Monoid[Int]
    def combine(x: Int, y: Int): Int = x + y
    def unit: Int = 0

  given Monoid[Double]
    def combine(x: Double, y: Double): Double = x + y
    def unit: Double = 0.0D

  given optionMonoid[A](using M: Monoid[A]) as Monoid[Option[A]]
    def combine(x: Option[A], y: Option[A]): Option[A] =
      (x, y) match
        case (Some(x_), Some(y_)) => Some(M.combine(x_, y_))
        case (Some(x_), None) => Some(x_)
        case (None, Some(y_)) => Some(y_)
        case (None, None) => None

    def unit: Option[A] = None

  given listMonoid[A] as Monoid[List[A]]
    def combine(x: List[A], y: List[A]): List[A] =
      x ++ y

    def unit: List[A] = List.empty[A]

  given setMonoid[A] as Monoid[Set[A]]
    def combine(x: Set[A], y: Set[A]): Set[A] =
      x ++ y

    def unit: Set[A] = Set.empty[A]
