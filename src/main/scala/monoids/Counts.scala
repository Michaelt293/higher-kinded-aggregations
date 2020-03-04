case class Counts[A](getCounts: Map[A, Int]) extends AnyVal

object Counts
  given countsMonoid[A] as Monoid[Counts[A]]
    def combine(x: Counts[A], y: Counts[A]): Counts[A] =
      x.getCounts.foldLeft(y) { case (acc, (k, v)) =>
        Counts(acc.getCounts + ((k, v + acc.getCounts.getOrElse(k, 0))))
      }

    def unit = Counts(Map.empty[A, Int])

  given FunctionK[Id, Counts]
    def apply[A](id: Id[A]): Counts[A] =
      Counts(Map(id -> 1))

  given FunctionK[Option, Counts]
    def apply[A](option: Option[A]): Counts[A] =
      option.fold(Counts(Map.empty[A, Int])) { v =>
        Counts(Map(v -> 1)) 
      }
