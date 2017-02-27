package recfun
import common._

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
  def pascal(c: Int, r: Int): Int =
    if (c < 0 || r < 0 || c > r) throw new IllegalArgumentException("Column and row should be positive and column always less than row")
    else if (c == 0 || r == c) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceHelper(chars: List[Char], balanceNumber: Int): Boolean =
      if (balanceNumber < 0)
        false
      else if (chars.isEmpty)
        balanceNumber == 0
      else if (chars.head == '(')
        balanceHelper(chars.tail, balanceNumber + 1)
      else if (chars.head == ')')
        balanceHelper(chars.tail, balanceNumber - 1)
      else
        balanceHelper(chars.tail, balanceNumber)
    balanceHelper(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money < 0 || (money > 0 && coins.length == 0)) 0
    else if (money == 0) 1
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
}
