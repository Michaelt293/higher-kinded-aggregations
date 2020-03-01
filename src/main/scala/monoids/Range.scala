case class Range[A](minValue: Option[A], maxValue: Option[A])

object Range
  import Monoid.{given _}

  given maxMonoid[A](using O: Ordering[A]) as Monoid[Range[A]]
    def combine(x: Range[A], y: Range[A]): Range[A] =
      val minValue =
        (x.minValue, y.minValue) match
          case (Some(x_), Some(y_)) => Some(O.min(x_, y_))
          case (x_, y_) => x_.orElse(y_)

      val maxValue =
        (x.maxValue, y.maxValue) match
          case (Some(x_), Some(y_)) => Some(O.max(x_, y_))
          case (x_, y_) => x_.orElse(y_)

      Range(minValue, maxValue)

    def unit = Range(None, None)

  given FunctionK[Id, Range]
    def apply[A](id: Id[A]): Range[A] =
      Range(Some(id), Some(id))

  given FunctionK[Option, Range]
    def apply[A](option: Option[A]): Range[A] =
      Range(option, option)
