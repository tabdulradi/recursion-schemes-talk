package recursionless

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

final case class Fix[F[_]](unfix: F[Fix[F]]) {
  def cata[A](algebra: F[A] => A)(implicit F: Functor[F]): A = {
    val partial = F.map(unfix)(_.cata(algebra))
    algebra(partial)
  }
}
// yup, that's it. Check examples/Json.scala