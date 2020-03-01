opaque type First[N] = Option[N]

object First
  import Monoid.{given _}

  given firstMonoid[A] as Monoid[First[A]]
    def combine(x: First[A], y: First[A]): First[A] =
      x.orElse(y)

    def unit = None

  given FunctionK[Id, First]
    def apply[A](id: Id[A]): First[A] =
      Some(id)

  given IsoK[Option, First]
    def apply[A](option: Option[A]): First[A] =
      option

    def from: FunctionK[First, Option] =
      new FunctionK[First, Option] {
        def apply[A](first: First[A]): Option[A] =
          first
      }
