def countChange(money: Int, coins: List[Int]): Int =
  if (money == 0) 1
  else if (coins.isEmpty || money < 0) 0
  else countChange(money - coins.head, coins) + countChange(money, coins.tail)
