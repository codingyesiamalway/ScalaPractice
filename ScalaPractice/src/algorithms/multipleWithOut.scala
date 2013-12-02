package algorithms 
object test{
  
  def main(args: Array[String]) {
      println(result(List(1,2,3,4)))
      println(result(List(1,2,3,4,0)))
      println(result(List(1,2,3,4,0,0)))
  }
  
  // case when there is no 0. O(N)  
  def sub(a : List[Int]) : List[Int] = {
      val m = a.fold(1)(_ * _)
      def helper(at: List[Int]) : List[Int] = {
          if (at.isEmpty) List()
          else m / at.head :: helper(at.tail)
      }
      helper(a)
  }                                               
  
  // case when there is 0. O(N) 
  def subWithZero(a : List[Int]) : List[Int] = {
      val tmp = mulWithOut(a, a.indexOf(0))
      def helper(at: List[Int], i : Int) : List[Int] = {
          if (at.isEmpty) List()
          else if (at.head == 0) tmp :: helper(at.tail, i+ 1)
          else 0:: helper(at.tail, i+ 1)
      }
      helper(a, 0)
  }                                              
  
  def result(a : List[Int]) : List[Int] = {
      if (a.contains(0)) subWithZero(a)
      else sub(a)
  }                                              
  
  def mulWithOut(a : List[Int], i : Int) : Int = {
	  if (a.isEmpty) 1
	  else if (i == 0) mulWithOut(a.tail, i - 1)
	  else a.head * mulWithOut(a.tail, i - 1)
  }
 
}