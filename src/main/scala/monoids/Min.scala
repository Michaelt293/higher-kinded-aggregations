case class Min[A](getMin: Option[A]) extends AnyVal

object Min
  given maxMonoid[A](using O: Ordering[A]) as Monoid[Min[A]]
    def combine(x: Min[A], y: Min[A]): Min[A] =
      (x.getMin, y.getMin) match
        case (Some(x_), Some(y_)) => Min(Some(O.min(x_, y_)))
        case (x_, y_) => Min(x_.orElse(y_))

    def unit = Min(None)

  given FunctionK[Id, Min]
    def apply[A](id: Id[A]): Min[A] =
      Min(Some(id))

  given IsoK[Option, Min]
    def apply[A](option: Option[A]): Min[A] =
      Min(option)

    def from: FunctionK[Min, Option] =
      new FunctionK[Min, Option] {
        def apply[A](min: Min[A]): Option[A] =
          min.getMin
      }
