package recfun
import common._

/** https://class.coursera.org/progfun-005/assignment/view?assignment_id=2 **/
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
   * Exercise 1: Pascal's Triangle.
   * Calculates the entry at row r and column c in Pascal's Triangle.
   */
  def pascal(c: Int, r: Int): Int = {
    if(c == 0 || c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2: Parentheses Balancing.
   * Determines if the parentheses are balanced in a list of characters.
   */  
  def balance(chars: List[Char]): Boolean = {
  
	def aux_balance(left_parens: Int, chars: List[Char]): Boolean = {
	  
	  if (chars.isEmpty) {
	    left_parens == 0
	    
	  } else {
	    val l =
	      if (chars.head == '(') left_parens + 1
	        else if (chars.head == ')') left_parens - 1
	        else left_parens
	      
	    if (l >= 0) 
	      aux_balance(l, chars.tail)
	    else false										/* too many right parens */
	  }
	}
	  
    aux_balance(0, chars)
  }

  /**
   * Exercise 3: Counting Change.
   * Determines the number of combinations of coins that can be given as change for the value money.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    
    def aux_countChange(ways: Array[Int], money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty)
      	ways(money)
      else {
    	for (j <- coins.head to money)
    	  ways(j) = ways(j) + ways(j - coins.head)
    	aux_countChange(ways, money, coins.tail)
      }
    }
    
    val ways = Array.fill(money + 1)(0)
    ways(0) = 1
	aux_countChange(ways, money, coins)
  }       
}
