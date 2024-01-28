import scala.annotation.tailrec
import scala.util.Either
import scala.util.control.NonFatal

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

  def map2Both[E, A, B, C](
                            a: Either[E, A],
                            b: Either[E, B],
                            f: (A, B) => C
                          ): Either[List[E], C] =
    (a, b) match
      case (Right(aa), Right(bb)) => Right(f(aa, bb))
      case (Left(e), Right(_)) => Left(List(e))
      case (Right(_), Left(e)) => Left(List(e))
      case (Left(e1), Left(e2)) => Left(List(e1, e2))

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



object Either:
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if xs.isEmpty then
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] =
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match
      case Nil => Right(Nil)
      case h :: t => (f(h).map2(traverse(t)(f)))(_ :: _)

  def traverse_1[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)
