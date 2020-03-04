case class SumOption[N](getSumOption: Option[N]) extends AnyVal
  def getSum(using N: Numeric[N]): N = 
    getSumOption.getOrElse(N.zero)

object SumOption
  given sumOptionMonoid[N](using N: Numeric[N]) as Monoid[SumOption[N]]
    def combine(x: SumOption[N], y: SumOption[N]): SumOption[N] =
      (x.getSumOption, y.getSumOption) match
        case (Some(x_), Some(y_)) => SumOption(Some(N.plus(x_, y_)))
        case (x_, y_) => SumOption(x_.orElse(y_))

    def unit = SumOption(None)

  given FunctionK[Id, SumOption]
    def apply[A](id: Id[A]): SumOption[A] =
      SumOption(Some(id))

  given IsoK[Option, SumOption]
    def apply[A](option: Option[A]): SumOption[A] =
      SumOption(option)

    def from: FunctionK[SumOption, Option] =
      new FunctionK[SumOption, Option] {
        def apply[A](sum: SumOption[A]): Option[A] =
          sum.getSumOption
      }
