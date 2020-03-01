opaque type Last[N] = Option[N]

object Last
  import Monoid.{given _}

  given lastMonoid[A] as Monoid[Last[A]]
    def combine(x: Last[A], y: Last[A]): Last[A] =
      y.orElse(x)

    def unit = None

  given FunctionK[Id, Last]
    def apply[A](id: Id[A]): Last[A] =
      Some(id)

  given IsoK[Option, Last]
    def apply[A](option: Option[A]): Last[A] =
      option

    def from: FunctionK[Last, Option] =
      new FunctionK[Last, Option] {
        def apply[A](last: Last[A]): Option[A] =
          last
      }
