opaque type Sum[N] = Option[N]

object Sum
  // import Monoid.{given _}

  given sumMonoid[N](using N: Numeric[N]) as Monoid[Sum[N]]
    def combine(x: Sum[N], y: Sum[N]): Sum[N] =
      (x, y) match
        case (Some(x_), Some(y_)) => Some(N.plus(x_, y_))
        case (x_, y_) => x_.orElse(y_)

    def unit = None

  given FunctionK[Id, Sum]
    def apply[A](id: Id[A]): Sum[A] =
      Some(id)

  given IsoK[Option, Sum]
    def apply[A](option: Option[A]): Sum[A] =
      option

    def from: FunctionK[Sum, Option] =
      new FunctionK[Sum, Option] {
        def apply[A](sum: Sum[A]): Option[A] =
          sum
      }
