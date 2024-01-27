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
    map(f).getOrElse(None)


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

  def lift[A, B](f: A => B): Option[A] => Option[B] =
    _.map(f)

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