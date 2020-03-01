opaque type Min[N] = Option[N]

object Min
  import Monoid.{given _}

  given maxMonoid[A](using O: Ordering[A]) as Monoid[Min[A]]
    def combine(x: Min[A], y: Min[A]): Min[A] =
      (x, y) match
        case (Some(x_), Some(y_)) => Some(O.min(x_, y_))
        case (x_, y_) => x_.orElse(y_)

    def unit = None

  given FunctionK[Id, Min]
    def apply[A](id: Id[A]): Min[A] =
      Some(id)

  given IsoK[Option, Min]
    def apply[A](option: Option[A]): Min[A] =
      option

    def from: FunctionK[Min, Option] =
      new FunctionK[Min, Option] {
        def apply[A](min: Min[A]): Option[A] =
          min
      }
