val someValue: Option[String] = Some("I am wrapped in something")



val emptyValue: Option[String] = None


def addWithSyntaxSugar(x: Int) = (y: Int) ⇒ x + y

addWithSyntaxSugar(1).isInstanceOf[Function1[Int, Int]]


addWithSyntaxSugar(2)(3)

def fiveAdder = addWithSyntaxSugar(5)
fiveAdder(5)



val pairs = Map(3 -> 'c, 1 -> 'a, 4 -> 'd, 2 -> 'b)
val keys_vals = pairs.map(k_v => (k_v._1, k_v._2)).unzip
keys_vals._1














