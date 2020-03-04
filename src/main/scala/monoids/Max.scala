case class Max[A](getMax: Option[A]) extends AnyVal

object Max
  given maxMonoid[A](using O: Ordering[A]) as Monoid[Max[A]]
    def combine(x: Max[A], y: Max[A]): Max[A] =
      (x.getMax, y.getMax) match
        case (Some(x_), Some(y_)) => Max(Some(O.max(x_, y_)))
        case (x_, y_) => Max(x_.orElse(y_))

    def unit = Max(None)

  given FunctionK[Id, Max]
    def apply[A](id: Id[A]): Max[A] =
      Max(Some(id))

  given IsoK[Option, Max]
    def apply[A](option: Option[A]): Max[A] =
      Max(option)

    def from: FunctionK[Max, Option] =
      new FunctionK[Max, Option] {
        def apply[A](max: Max[A]): Option[A] =
          max.getMax
      }
