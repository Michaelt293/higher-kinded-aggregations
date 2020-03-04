case class Last[A](getLast: Option[A]) extends AnyVal

object Last
  given lastMonoid[A] as Monoid[Last[A]]
    def combine(x: Last[A], y: Last[A]): Last[A] =
      Last(y.getLast.orElse(x.getLast))

    def unit = Last(None)

  given FunctionK[Id, Last]
    def apply[A](id: Id[A]): Last[A] =
      Last(Some(id))

  given IsoK[Option, Last]
    def apply[A](option: Option[A]): Last[A] =
      Last(option)

    def from: FunctionK[Last, Option] =
      new FunctionK[Last, Option] {
        def apply[A](last: Last[A]): Option[A] =
          last.getLast
      }
