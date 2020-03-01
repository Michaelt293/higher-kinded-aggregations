opaque type Product[N] = Option[N]

object Product
  import Monoid.{given _}

  given sumMonoid[N](using N: Numeric[N]) as Monoid[Product[N]]
    def combine(x: Product[N], y: Product[N]): Product[N] =
      (x, y) match
        case (Some(x_), Some(y_)) => Some(N.times(x_, y_))
        case (x_, y_) => x_.orElse(y_)

    def unit = None

  given FunctionK[Id, Product]
    def apply[A](id: Id[A]): Product[A] =
      Some(id)

  given IsoK[Option, Product]
    def apply[A](option: Option[A]): Product[A] =
      option

    def from: FunctionK[Product, Option] =
      new FunctionK[Product, Option] {
        def apply[A](product: Product[A]): Option[A] =
          product
      }
