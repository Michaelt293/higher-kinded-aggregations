trait FunctionK[F[_], G[_]]
  def apply[A](fa: F[A]): G[A]

object FunctionK
  def apply[F[_], G[_]](using ev: FunctionK[F, G]) = ev

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
