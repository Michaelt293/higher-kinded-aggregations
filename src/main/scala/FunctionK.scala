trait FunctionK[F[_], G[_]]
  def apply[A](fa: F[A]): G[A]

object FunctionK
  def apply[F[_], G[_]](using ev: FunctionK[F, G]) = ev

  given FunctionK[Id, Option]
    def apply[A](id: Id[A]): Option[A] =
      Some(id)

  given FunctionK[Id, List]
    def apply[A](id: Id[A]): List[A] =
      List(id)

  given FunctionK[Option, List]
    def apply[A](option: Option[A]): List[A] =
      option.fold(List.empty[A])(List(_))

  given FunctionK[Id, Set]
    def apply[A](id: Id[A]): Set[A] =
      Set(id)

  given FunctionK[Option, Set]
    def apply[A](option: Option[A]): Set[A] =
      option.fold(Set.empty[A])(Set(_))

  given tuple2IdFunctionK[F[_], G[_]](
    using
    FF: FunctionK[Id, F],
    FG: FunctionK[Id, G]
    ) as FunctionK[Id, [x] =>> (F[x], G[x])]
    def apply[A](id: Id[A]): (F[A], G[A]) =
      (FF(id), FG(id))

  given tuple2OptionFunctionK[F[_], G[_]](
    using
    FF: FunctionK[Option, F],
    FG: FunctionK[Option, G]
    ) as FunctionK[Option, [x] =>> (F[x], G[x])]
    def apply[A](option: Option[A]): (F[A], G[A]) =
      (FF(option), FG(option))

  given tuple3IdFunctionK[F[_], G[_], H[_]](
    using
    FF: FunctionK[Id, F],
    FG: FunctionK[Id, G],
    FH: FunctionK[Id, H]
    ) as FunctionK[Id, [x] =>> (F[x], G[x], H[x])]
    def apply[A](id: Id[A]): (F[A], G[A], H[A]) =
      (FF(id), FG(id), FH(id))

  given tuple3OptionFunctionK[F[_], G[_], H[_]](
    using
    FF: FunctionK[Option, F],
    FG: FunctionK[Option, G],
    FH: FunctionK[Option, H]
    ) as FunctionK[Option, [x] =>> (F[x], G[x], H[x])]
    def apply[A](option: Option[A]): (F[A], G[A], H[A]) =
      (FF(option), FG(option), FH(option))
