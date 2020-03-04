case class Product[N](getProduct: N)

object Product
  given productMonoid[N](using N: Numeric[N]) as Monoid[Product[N]]
    def combine(x: Product[N], y: Product[N]): Product[N] =
      Product(N.times(x.getProduct, y.getProduct))

    def unit = Product(N.plus(N.zero, N.fromInt(1)))

  given IsoK[Id, Product]
    def apply[A](id: Id[A]): Product[A] =
      Product(id)

    def from: FunctionK[Product, Id] =
      new FunctionK[Product, Id] {
        def apply[A](product: Product[A]): Id[A] =
          product.getProduct
      }
