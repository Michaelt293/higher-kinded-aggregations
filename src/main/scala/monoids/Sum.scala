case class Sum[N](getSum: N)

object Sum
  given sumMonoid[N](using N: Numeric[N]) as Monoid[Sum[N]]
    def combine(x: Sum[N], y: Sum[N]): Sum[N] =
      Sum(N.plus(x.getSum, y.getSum))

    def unit = Sum(N.zero)

  given IsoK[Id, Sum]
    def apply[A](id: Id[A]): Sum[A] =
      Sum(id)

    def from: FunctionK[Sum, Id] =
      new FunctionK[Sum, Id] {
        def apply[A](sum: Sum[A]): Id[A] =
          sum.getSum
      }
