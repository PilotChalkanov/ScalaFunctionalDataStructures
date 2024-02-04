import scala.annotation.tailrec
import scala.collection.immutable.List

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] =
    this match
      case None => None
      case Some(v) => Some(f(v))

  def flatMap[B](f: A => Option[B]): Option[B] =
    this match
      case None => None
      case Some(a) => f(a)

  def getOrElse[B >: A](default: => B): B =
    this match
      case None => default
      case Some(v) => v

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)


  def filter(f: A => Boolean): Option[A] =
    flatMap(
      a => if f(a) then Some(a)
      else None
    )

object Option:
  def lift[A, B](f: A => B): Option[A] => Option[B] =
    _.map(f)

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    @tailrec
    def aux(as: List[Option[A]], list: List[A]): List[A] =
      as match
        case Nil => list
        case None :: t => List()
        case Some(x) :: Nil => x :: list
        case Some(x) :: t => aux(t, x :: list)

    val list = aux(as, List())
    list match
      case List() => None
      case _ => Some(list.reverse)

  def sequence_1[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight[Option[List[A]]](Some(Nil))((a, acc) => map2(a, acc)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)

  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((a,acc) => map2(f(a), acc)(_ :: _))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)