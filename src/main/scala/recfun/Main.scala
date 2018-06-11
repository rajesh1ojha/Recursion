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
  def pascal(c: Int, r: Int): Int = fact(r) / (fact(c) * fact(r - c))

  def fact(x: Int, acc: Int = 1): Int = if (x == 0) acc else fact(x - 1, x * acc)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balanceBrackets(charList: List[Char], res: Int = 0): Int = {
      if (charList.isEmpty) res
      else if (charList.head == '(') balanceBrackets(charList.tail, res + 1)
      else if (charList.head == ')') balanceBrackets(charList.tail, res - 1)
      else balanceBrackets(charList.tail, res)
    }

    balanceBrackets(chars) == 0
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def change(m: Int, coinList: List[Int], count: Int): Int =
      m match {
        case _ if m < 0 => count
        case _ if coinList.isEmpty => {
          m match {
            case 0 => count + 1
            case _ => count
          }
        }
        case _ => change(m, coinList.tail, count) + change(m - coinList.head, coinList, count)
      }
    change(money, coins, 0)
  }
}
