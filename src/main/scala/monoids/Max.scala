opaque type Max[N] = Option[N]

object Max
  import Monoid.{given _}

  given maxMonoid[A](using O: Ordering[A]) as Monoid[Max[A]]
    def combine(x: Max[A], y: Max[A]): Max[A] =
      (x, y) match
        case (Some(x_), Some(y_)) => Some(O.max(x_, y_))
        case (x_, y_) => x_.orElse(y_)

    def unit = None

  given FunctionK[Id, Max]
    def apply[A](id: Id[A]): Max[A] =
      Some(id)

  given IsoK[Option, Max]
    def apply[A](option: Option[A]): Max[A] =
      option

    def from: FunctionK[Max, Option] =
      new FunctionK[Max, Option] {
        def apply[A](max: Max[A]): Option[A] =
          max
      }
