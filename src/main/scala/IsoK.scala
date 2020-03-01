trait IsoK[F[_], G[_]] extends FunctionK[F, G]
  def from: FunctionK[G, F]

object IsoK
  def apply[F[_], G[_]](using ev: IsoK[F, G]) = ev

  given identityIsoK[F[_]] as IsoK[F, F]
    def apply[A](fa: F[A]): F[A] =
      fa

    def from: FunctionK[F, F] =
      new FunctionK[F, F] {
        def apply[A](fa: F[A]): F[A] =
          fa
      }

// extension seqHKOps on [HK[_[_]], F[_]](xs: Seq[HK[F]])
//   // extension method cannot have type parameters since some were already given previously
//   def foldMapK[G[_]](f: FunctionK[F, G]): HK[G] = ???

implicit class SeqHKOps[HK[_[_]], F[_]](xs: Seq[HK[F]]) extends AnyVal
  def foldMapK[G[_]](f: FunctionK[F, G])(
    given M: Monoid[HK[G]], F: FunctorK[HK]): HK[G] =
      xs.foldLeft(M.unit)((acc, x) => M.combine(acc, (x mapK f)))

  def mconcat[G[_]](
    given 
    M: Monoid[HK[G]], 
    F: FunctorK[HK],
    FK: FunctionK[F, G]): HK[G] =
      foldMapK(FK)

  def mconcatVia[G[_]](
    given 
    M: Monoid[HK[G]], 
    F: FunctorK[HK],
    I: IsoK[F, G]): HK[F] =
      foldMapK(I) mapK I.from
