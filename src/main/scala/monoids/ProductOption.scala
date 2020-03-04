case class ProductOption[N](getProductOption: Option[N]) extends AnyVal
  def getProduct(using N: Numeric[N]): N =
    getProductOption.getOrElse(N.plus(N.zero, N.fromInt(1)))

object ProductOption
  given productOptionMonoid[N](using N: Numeric[N]) as Monoid[ProductOption[N]]
    def combine(x: ProductOption[N], y: ProductOption[N]): ProductOption[N] =
      (x.getProductOption, y.getProductOption) match
        case (Some(x_), Some(y_)) => ProductOption(Some(N.times(x_, y_)))
        case (x_, y_) => ProductOption(x_.orElse(y_))

    def unit = ProductOption(None)

  given FunctionK[Id, ProductOption]
    def apply[A](id: Id[A]): ProductOption[A] =
      ProductOption(Some(id))

  given IsoK[Option, ProductOption]
    def apply[A](option: Option[A]): ProductOption[A] =
      ProductOption(option)

    def from: FunctionK[ProductOption, Option] =
      new FunctionK[ProductOption, Option] {
        def apply[A](product: ProductOption[A]): Option[A] =
          product.getProductOption
      }
