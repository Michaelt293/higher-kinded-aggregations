case class First[A](getFirst: Option[A]) extends AnyVal

object First
  given firstMonoid[A] as Monoid[First[A]]
    def combine(x: First[A], y: First[A]): First[A] =
      First(x.getFirst.orElse(y.getFirst))

    def unit = First(None)

  given FunctionK[Id, First]
    def apply[A](id: Id[A]): First[A] =
      First(Some(id))

  given IsoK[Option, First]
    def apply[A](option: Option[A]): First[A] =
      First(option)

    def from: FunctionK[First, Option] =
      new FunctionK[First, Option] {
        def apply[A](first: First[A]): Option[A] =
          first.getFirst
      }
