
import math._
import scala.collection.mutable._
import scala.collection.immutable.List

object Main extends App{


  //1
  //Define a Scala function called circleArea that will calculate the area of a circle.
  // The function will be passed the radius of the circle.  (A = π*r2)

  def circleArea ( radius: Double) = {
    var result = math.Pi * radius * radius
    println(result)
  }


  //2
  //Define a Scala function called hypotenuse, which takes two arguments.
  // The two arguments are numbers, which represent the lengths of the legs of a right triangle.
  // The function returns a number representing the length of the hypotenuse of the right triangle.
  //Your answer should be a decimal, NOT a fraction.

  def hypotenuse(sideA: Double, sideB:Double) = {
    var result = sqrt((sideA * sideA) + (sideB * sideB))
    println(result)
  }


  //3
  //Write a Scala function called reverseDigits, which will take in an integer (positive or negative) and
  // return it as an integer with the digits reversed. The sign should not be changed. (Hint: Use remainder and quotient.)

  def reverseDigits(num: Int) = {

    def reverSet(x : Int, acc : Int =0):Int = {
      if(x == 0)
        acc
      else{
        if(Math.abs(Int.MaxValue / 10)  < Math.abs(acc)) 0 else
          reverSet(x/10,acc*10+(x%10))
      }
    }
    println(reverSet(num))
  }




  //4
  //Write a Scala function called conditionalRemove that takes a list and an element and
  // returns a list with the last element removed if it equals the element passed in as a parameter.
  // If the list does not end in the element passed as a parameter, the list is returned.

  def conditionalRemove(removeString: String,list: List[String]) = {
    val length = list.length
    if(removeString.equals(list(length-1))){
      list.patch(length-1, Nil, 1)

    }
    else{
      list
    }
    println(list)

  }



//5
  //Write a Scala function called isPalindrome that takes a String, Int, or List and determines if it is a palindrome (i.e., the same forwards and backwards).
  // The comparison should be case-insensitive (i.e., case does NOT matter). Spaces and punctuation should be ignored.

  def isPalindrome( s: String) = {
    val filter = s.toLowerCase.replace(" ","").replaceAll("""[\p{Punct}]""", "")
    if (filter.reverse == filter){
      println(true)
    }
    else{
      println(false)
    }
  }

  //Function for the case of an Int being passed in.
  def isPalindrome( num: Int) = {

    var IntToString = num.toString()
    var reverseIntToString = IntToString.reverse

    if (IntToString == reverseIntToString){
      println(true)
    }
    else{
      println(false)
    }
  }
   // isPalindrome(12321)
   // isPalindrome(1232)

  //Function for the case of a List being passed in.
  def isPalindrome(list: List[Any]) = {
    def reverse[A](l: List[A]): List[A] = {
      def _reverse(res: List[A], rem: List[A]): List[A] = rem match {
        case Nil => res
        case h :: tail => _reverse(h :: res, tail)
      }
      _reverse(Nil, l)
    }
    val rev = reverse(list)
    if (rev == list){
      println(true)
    }
    else{
      println(false)
    }
  }

//6
  //Write a Scala function called isPerfectNumber that takes an integer and returns a tuple containing a Boolean and a List of Ints.
  // The Boolean represents the “perfectness” of the number. (i.e., the sum of its divisors except itself is the number).
  // The list should be the the divisors excluding the numbers. For example 6 is a perfect number.
  // Its divisors are {1, 2, 3, 6}. Excluding itself the sum of 1, 2, and 3 is 6.

  def isPerfectNumber(num: Int) = {
    //append to list easily
    var list = ListBuffer[Int]()

    var sum = 0
    for (i <- 1 to num -1 ){
      if((num % i) == 0){
        sum = sum + i
        list += i
      }
    }
    if(sum == num){
      println ((true, list.toList))
    }
    else{
      println ((false, list.toList))
    }
  }




  //7
  //Write a Scala class to represent a ComplexNumber, i.e., a number with a real and an imaginary part.
  //Define the standard arithmetic operation on the class: Add, Subtract, Multiply, Divide, Conjugate, Reciprocal, and Abs.


  class ComplexNumber (val realNumber: Double, val complexNumber: Double) {

    //Addition: (a +bi)+(c +di) = (a +c) + (b +d)i
    def add(addition: ComplexNumber) = new ComplexNumber(this.realNumber + addition.realNumber, this.complexNumber + addition.complexNumber)

    //Subtraction: (a +bi) – (c +di) = (a-c) + (b-d)i
    def subtract(subtraction: ComplexNumber) = new ComplexNumber(this.realNumber - subtraction.realNumber, this.complexNumber - subtraction.complexNumber)

    // Multiplication: (a +bi) (c +di) = ac + bci + adi + bdi22 = (ac - bd) + (bc + ad) i
    def multiply(multiplication: ComplexNumber) = {
      val realPart = (this.realNumber * multiplication.realNumber) - (this.complexNumber * multiplication.complexNumber)
      val complexPart = (this.realNumber * multiplication.complexNumber) + (this.complexNumber * multiplication.realNumber)
      new ComplexNumber(realPart, complexPart)
    }

    //Division: (a+bi)(c+di)(a+bi)(c+di) = (ac+bd)(c2+d2)(ac+bd)(c2+d2) + (bc−ad)(c2+d2)i
    def divide(divisor: ComplexNumber) = {
      val numerator = this multiply divisor.conjugate
      val denominator = divisor multiply divisor.conjugate
      new ComplexNumber(numerator.realNumber / denominator.realNumber, numerator.complexNumber / denominator.realNumber)
    }

    //Conjugate
    //Multiply by (-1) then do opearation
    def conjugate = new ComplexNumber(this.realNumber, this.complexNumber * -1)

    //Reciprocal
    // 1/ given complex number : 2-3i == 1/2-3i
    def reciprocal = this.conjugate divide new ComplexNumber(pow(this.abs, 2), 0)

    //Abs ( Absolute Value )
    // \ 3-4i \  = 5
    // 3^2 + 4^2 = \ 3-4i \^2
    // 25 = \ 3-4i \^2
    // 5 \ 3-4i \
    def abs = sqrt(pow(this.realNumber, 2) + (pow(this.complexNumber, 2)))

    override def toString = {
      if (complexNumber > 0) {
        s"$realNumber + ${complexNumber}i"
      }
      else {
        s"$realNumber - ${complexNumber * -1}i"
      }
    }
  }

  //problem 1
  //circleArea(8)
  //201.06

  //problem 2
  //hypotenuse(3, 4)
  //5.0
  // hypotenuse (9, 16)
  //18.35755975068582

  //problem 3
  // reverseDigits( 654)
  //456
  //reverseDigits( -85)
  //-58

  //problem 4
  //conditionalRemove("a", List("b", "c", "e", "a", "g", "a"))
  //List("b", "c", "d", "e", "a","g")
  //conditionalRemove("a", List("b", "c", "e"))
  //List("b", "c", "e")

  // problem 5
  //isPalindrome(List ("a", "b", "b", "a"))
  //isPalindrome(List ("a", "b", "c", "a"))

  //isPalindrome( "12321")
  //true
  //isPalindrome( "racecar")
  //true
  //isPalindrome( "mom")
  //true
  //isPalindrome( "Mammam")
  //true
  //isPalindrome( "Yo Momma")
  //false
  // isPalindrome( "rats! live on no evil star." )
  //true


  // problem 6
  //isPerfectNumber(6)
  //(true, List( 1, 2, 3))
  //isPerfectNumber(7)
  //(false,List(1))

  //problem 7
  val x = new ComplexNumber(8, 12)
  //println(x)
  //println(x.add(new ComplexNumber(3,5)))
  //println(x.subtract(new ComplexNumber(2,5)))
  //println(x.multiply(new ComplexNumber(2,5)))
  //println(x.divide(new ComplexNumber(2,6)))
  //println(x.abs)
  //println(x.reciprocal)


  //problem 8
  //println(Stats.mean(List(6.0, 7.0, 8.0)))
  //println(Stats.min(List(6.0, 7.0, 8.0)))
  //println(Stats.max(List(7.0, 8.0, 9.0)))
  //println(Stats.mode(List(1.0, 2.0, 3.0, 3.0)))

  //problem 9
  //println(foldStats.max(List(0.2, 3.5, 0.3, 3.0, 3.0)))
  //println(foldStats.min(List(0.2, 3.5, 0.3, 3.0, 3.0)))
  //println(foldStats.mean(List(0.2, 3.5, 0.3, 3.0, 3.0)))





  }

//8
//Write a Scala object named Stats with the following functions that accepts a list of Doubles.
// (You may not use built-in functions that compute these functions for you.)

object Stats {

  def mean(list: List[Double]) = {
    list.sum / list.length
  }

  def min(list: List[Double]) = {
    //val mininimumNumber = list.reduceLeft(FindLeast())
    var minValue = Double.MaxValue
    for (i <- 0 to list.length - 1) {
      if (list(i) < minValue) {
        minValue = list(i)
      }
    }
    minValue
  }

  def max(list: List[Double]) = {
    var maxValue = -Double.MaxValue
    for (i <- 0 to list.length - 1) {
      if (list(i) > maxValue) {
        maxValue = list(i)
      }
    }
    maxValue
  }

  def mode (liOne: List[Double]) : Double = {
    var elements = collection.mutable.Map[Double, Int]()
    var i = 0
    var mostOccurances = 0.0
    while ( i <= (liOne.length - 1)){
      var n = 0
      var acc = 0
      while ( n <= (liOne.length - 1)){
        if (liOne(i) == liOne(n)){
          acc = acc + 1
        }
        n = n + 1
      }
      elements = elements + (liOne(i) -> acc)
      i = i + 1
    }

    mostOccurances = elements.maxBy(_._2)._1
    //println(mostOccurances)

    return mostOccurances
  }
  def median(lst: List[Double]): Double = {
    var median = 0.0
    if (lst.length % 2 == 0) {
      median = ((lst((lst.length / 2) - 1) + lst(lst.length / 2)) / 2)
    }
    else {
      median = (lst(round(lst.length / 2)))
    }
    return median
  }
}



  //mean(List(Doubles…))
  // min(List(Doubles…))
  // max(List(Doubles…))
  //mode(List(Doubles…))
  //median(List(Doubles…))
//9
  //Write an object named FoldStats with the following functions implemented using the fold functions of Scala.
  // (You may not use built-in functions that compute these functions for you.)

object foldStats {

  def mean(list: List[Double]): Double = {
    var total = list.fold(0.0) {
      (z, i) => z + i
    }
    var mean = (total / list.length)
    return mean
  }

  def min(list: List[Double]): Double = {
    var min = Double.MaxValue
    list.fold(Double.MaxValue) {
      (z, i) =>
        if (min > i) {
          min = i
          i
        }
        else {
          i
        }
    }
    return min
  }

  def max(list: List[Double]): Double = {
    var max = Double.MinValue
    list.fold(Double.MinValue) {
      (z, i) =>
        if (max < i) {
          max = i
          i
        }
        else {
          i
        }
    }
    return max
  }
}


