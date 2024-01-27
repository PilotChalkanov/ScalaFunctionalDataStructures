import Tree.{Branch, Leaf}

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (l.depth.max(r.depth))

  def map[B>:A](f: A => B): Tree[B] =
    this match
      case Leaf(x) => Leaf(f(x))
      case Branch(l,r) => Branch(l.map(f), r.map(f))

  def fold[B>:A](acc: B)(f: (B, B) => B): B =
    this match
      case Leaf(v) => f(v, acc)
      case Branch(l, r) => f(l.fold(acc)(f), r.fold(acc)(f))

//  def sizeViaFold: Int =
//    fold(1)((_, _) => _ + _)

object Tree:
  extension (t: Tree[Int]) def firstPositive: Int = t match
    case Leaf(i) => i
    case Branch(l, r) =>
      val lpos = l.firstPositive
      if lpos > 0 then lpos else r.firstPositive

  extension (t: Tree[Int]) def maximum: Int = t match
    case Leaf(x) => x
    case Branch(l, r) => l.maximum.max(r.maximum)


