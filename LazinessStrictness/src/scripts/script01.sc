def maybeTwice(b: Boolean, i: => Int) = if b then i + i else 0

val x = maybeTwice(true, { println("hi"); 1 + 41 })

def maybeTwice2(b: Boolean, i: => Int) =
  lazy val j = i
  if b then j + j else 0


val y = maybeTwice2(true, { println("hi"); 1 + 41 })

