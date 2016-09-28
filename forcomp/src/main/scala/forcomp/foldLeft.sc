
def sum(l: List[Int]): Int ={
  if(l.isEmpty)
    0
  else
    l.head + sum(l.tail)
}
val l = List(10,20,30,40,150,90)
/*
val s1 = l.sum
val s2 = sum(l)
val s3 = l.foldLeft(0)((a,b) => a+b)
val s4 = l.foldLeft(List[Int](20))((a,b) => b::a)

val s5 = l.foldLeft(0)((sum,a) => sum+1)
val s6 = l.foldLeft(l.head)((a,b) => b)
val s7 = l.foldLeft(l.head)((a,b) => a)

def penul(l: List[Int]): Int ={
  l.foldLeft(l.head,l.tail.head)((r,c) => (r._2,c))._1
}

def contains(l: List[Int], el: Int): Boolean ={
  l.foldLeft(false)((a,b)=> a || b==el)
}

def get(l: List[Int], ind:Int): Int ={
  l.foldLeft((l.head, 0))((a,b) =>
    if(a._2==ind) a else (b, a._2+1))._1
}

contains(l, 40)
penul(l)
get(l,3)
*/

def duplicate(l: List[Any]): List[Any] = l match {
  case Nil => List()
  case xs::rest => xs :: xs :: duplicate(rest)
}
duplicate(l)

def makeString(l: List[Any]) : String ={
    l.foldLeft("List("+l.head)((r,c) => r+','+c)+')'
}
makeString(l)

def reverse(l: List[Any]): List[Any] = {
  l.foldLeft(List[Any]())((r,c) => c :: r)
}


reverse(List(1,2,3,4,5,6))




