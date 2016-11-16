// Part 1 about the 3n+1 conceture
//=================================


//(1) Complete the collatz function below. It should
//    recursively calculate the number of steps needed 
//    until the collatz series reaches the number 1.
//    If needed you can use an auxilary function that
//    performs the recursion. The function should expect
def collatz(n: Long): Long = {
  if (n == 1) return 1
  else if (n % 2 == 0) {
    1+collatz(n / 2)
  }
  else {
    1+ collatz(3 * n + 1)
  }
}
assert(collatz(97) == 119)
collatz(97)



//(2)  Complete the collatz bound function below. It should
//     calculuate how many steps are needed for each number 
//     from 1 upto a bound and returns the maximum number of
//     steps and the corresponding number that needs that many 
//     steps. You should expect bounds in the range of 1
//     upto 1 million. The first component of the pair is
//     the maximum number of steps and the second is the 
//     corresponding number.

def collatz_max(bnd: Int): (Int, Int) = {
  val maxL = for(n<-(1 to bnd).toList) yield collatz(n)
  return  (maxL.max.toInt,(maxL.indexOf(maxL.max).toInt+1))
  //  (step, bnd)
}
collatz_max(9)



