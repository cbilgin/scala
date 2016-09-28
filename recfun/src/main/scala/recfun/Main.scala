package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) 1
    else if (r == c) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    val numPar = 0
    balance(chars, numPar)
  }

  def balance(chars: List[Char], numPar: Int): Boolean = {
    if (numPar < 0) false
    else if (chars.isEmpty) numPar == 0
    else if (chars.head == '(') balance(chars.tail, numPar + 1)
    else if (chars.head == ')') balance(chars.tail, numPar - 1)
    else balance(chars.tail, numPar)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}


