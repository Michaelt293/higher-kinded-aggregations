trait FunctorK[HK[_[_]]]
  def [F[_], G[_]](fa: HK[F]) mapK (f: FunctionK[F, G]): HK[G]

object FunctorK
  def apply[HK[_[_]]](using ev: FunctorK[HK]) = ev
