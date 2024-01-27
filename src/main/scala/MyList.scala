import scala.annotation.tailrec

enum MyList[+A]:
  case Nil
  case Cons(head: A, tail: MyList[A])

object MyList:
  def tail[A](list: MyList[A]): MyList[A] = list match
    case Nil => throw new NoSuchElementException("MyList is empty")
    case Cons(_, tail) => tail

  def setHead[A](element: A, list: MyList[A]): MyList[A] = list match
    case Nil => Cons(element, MyList.Nil)
    case Cons(_, tail) => Cons(element, tail)

  @tailrec
  def drop[A](list: MyList[A], n: Int): MyList[A] = list match
    case Nil => MyList.Nil
    case Cons(_, tail) =>
      if (n < 1) list
      else drop(MyList.tail(list), n - 1)

  def map[A, B](list: MyList[A], f: A => B): MyList[B] =
    foldRightViaFoldLeft(list, Nil: MyList[B], (acc: MyList[B], h: A) => Cons(f(h), acc))

  def zipWith[A, B, C](a: MyList[A], b: MyList[B], f: (A, B) => C): MyList[C] =
    @tailrec
    def loop(a: MyList[A], b: MyList[B], acc: MyList[C]): MyList[C] =
      (a, b) match
        case (Nil, _) => acc
        case (_, Nil) => acc
        case (Cons(h1, t1), Cons(h2, t2)) => loop(t1, t2, Cons(f(h1, h2), acc))

    reverse(loop(a, b, Nil))

  def flatMap[A, B](list: MyList[A], f: A => MyList[B]): MyList[B] =
    foldRightViaFoldLeft(list, Nil: MyList[B], (acc: MyList[B], h: A) => append(f(h), acc))

  def filter[A](list: MyList[A], f: A => Boolean): MyList[A] =
    foldRight(list, Nil: MyList[A], (h, acc) => if f(h) then Cons(h, acc) else acc)

  def filterViaFlatMap[A](list: MyList[A], f: A => Boolean): MyList[A] =
    flatMap(list, h => if f(h) then Cons(h, Nil) else Nil: MyList[A])

  def dropWhile[A](list: MyList[A], predicate: A => Boolean): MyList[A] = list match
    case Cons(hd, tl) if predicate(hd) => dropWhile(tl, predicate)
    case _ => list

  def append[A](list1: MyList[A], list2: MyList[A]): MyList[A] =
    list1 match
      case Nil => list2
      case Cons(h, t) => Cons(h, append(t, list2))

  def appendViaFoldLeft[A](list1: MyList[A], list2: MyList[A]): MyList[A] =
    list1 match
      case Nil => list2
      case Cons(h, t) => MyList.foldRight(list2, list1, Cons(_, _))

  def concatNested[A](list: MyList[MyList[A]], acc: MyList[A] = Nil): MyList[A] =
    foldRight(list, Nil: MyList[A], append)

  def init[A](list: MyList[A]): MyList[A] =
    list match
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(hd, tl) => Cons(hd, init(tl))

  def sum(ints: MyList[Int]): Int = ints match
    case Nil => 0
    case Cons(h, t) => h + sum(t)

  def incrementAll(ints: MyList[Int]): MyList[Int] =
    foldRight(ints, Nil: MyList[Int], (h, t) => Cons(h + 1, t))

  def stringify(list: MyList[Double]): MyList[String] =
    foldRight(list, Nil: MyList[String], (h, t) => Cons(h.toString, t))

  def product(doubles: MyList[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(h, t) => h * product(t)

  def foldRight[A, B](list: MyList[A], acc: B, f: (A, B) => B): B =
    list match
      case Nil => acc
      case Cons(h, t) => f(h, foldRight(t, acc, f))

  def foldRightViaFoldLeft[A, B](list: MyList[A], acc: B, f: (B, A) => B) =
    foldLeft(MyList.reverse(list), acc, f)

  @tailrec
  def foldLeft[A, B](list: MyList[A], acc: B, f: (B, A) => B): B =
    list match
      case Nil => acc
      case Cons(head, tail) => foldLeft(tail, f(acc, head), f)

  def prepend[A, B](list: MyList[A], el: A): MyList[A] =
    Cons(el, list)

  def reverse[A](list: MyList[A]): MyList[A] =
    foldLeft(list, Nil: MyList[A], (acc, a) => Cons(a, acc))

  def sumViaFoldRight(ns: MyList[Int]) =
    foldRight(ns, 0, (x, y) => x + y)

  def productViaFoldRight(ns: MyList[Double]) =
    foldRight(ns, 1.0, _ * _)

  def sumViaFoldLeft(ns: MyList[Int]) =
    foldLeft(ns, 0, (x, y) => x + y)

  def productViaFoldLeft(ns: MyList[Double]) =
    foldLeft(ns, 1.0, _ * _)

  def length[A](list: MyList[A]): Int =
    foldRight(list, 0, (_, acc) => 1 + acc)

  @tailrec
  def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean = sup match
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h, t) => hasSubsequence(t, sub)

  @tailrec
  def startsWith[A](l: MyList[A], prefix: MyList[A]): Boolean = (l, prefix) match
    case (_, Nil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false

  def apply[A](as: A*): MyList[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail *))