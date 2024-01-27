import scala.annotation.tailrec
import scala.util.Either

enum Either[+E, +A]:
  case Left(value: E)
  case Right(value: A)


  def map[B](f: A => B): Either[E, B] =
    this match
      case Right(a) => Right(f(a))
      case Left(t) => Left(t)

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match
      case Right(v) => f(v)
      case Left(v) => Left(v)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match
      case Left(_) => b
      case Right(v) => Right(v)

  def map2[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for
      a <- this
      b <- that
    yield f(a, b)


  def traverseEither[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))((a: A, b: Either[E, List[B]]) => f(a).map2(b)(_ :: _))


  def map2All[E, A, B, C](
                           a: Either[List[E], A],
                           b: Either[List[E], B],
                           f: (A, B) => C
                         ): Either[List[E], C] =
    (a, b) match
      case (Right(aa), Right(bb)) => Right(f(aa, bb))
      case (Left(es), Right(_)) => Left(es)
      case (Right(_), Left(es)) => Left(es)
      case (Left(es1), Left(es2)) => Left(es1 ++ es2)


