type Id[A] = A

object Id
  given idMonoid[A](using M: Monoid[A]) as Monoid[Id[A]]
    def combine(x: Id[A], y: Id[A]): Id[A] =
      M.combine(x, y)

    def unit: Id[A] = 
      M.unit
