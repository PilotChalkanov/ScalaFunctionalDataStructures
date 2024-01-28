
import MyList.{Cons, Nil}
object playground extends App {
 val nestedList: MyList[MyList[Int]] = MyList(
  MyList(1, 2, 3),
  MyList(4, 5),
  MyList(6, 7, 8)
 )
 println(nestedList)
 val flattenedList: MyList[Int] = MyList.concatNested(nestedList)

 // Output: Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Cons(8, Nil)))))))))
 println(flattenedList)

}