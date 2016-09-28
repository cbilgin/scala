import Ordering.Implicits._

val l1 = Seq(10,20,30,60,10,40)
val l2 = Seq(20,20,30,60,10,40)
val l3 = Seq(5,20,30,60,10,40)
val l4 = Seq(5,20,30,60,10)

def sortList(left: Seq[Int], right: Seq[Int]): Boolean = {
  left < right
}

sortList(l1, l2)
sortList(l1, l3)
sortList(l1, l4)











