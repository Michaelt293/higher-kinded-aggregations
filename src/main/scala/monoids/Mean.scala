case class Mean[N](numerator: Option[N], denominator: Int)
  def getMean(given N: Numeric[N]): Option[Float] =
    if (denominator != 0) numerator.map(n => N.toFloat(n) / denominator)
    else None

object Mean
  given meanMonoid[N](using N: Numeric[N]) as Monoid[Mean[N]]
    def combine(x: Mean[N], y: Mean[N]): Mean[N] =
      val numerator = 
        (x.numerator, y.numerator) match
          case (Some(x_), Some(y_)) => Some(N.plus(x_, y_))
          case (x_, y_) => x_.orElse(y_)

      Mean(
        numerator,
        x.denominator + y.denominator
      )

    def unit: Mean[N] = Mean(Some(N.zero), 0)

  given FunctionK[Id, Mean]
    def apply[A](id: Id[A]): Mean[A] =
      Mean(Some(id), 1)

  given FunctionK[Option, Mean]
    def apply[A](option: Option[A]): Mean[A] =
      Mean(option, if option.isDefined then 1 else 0)
