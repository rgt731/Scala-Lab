
import math._
import scala.collection.mutable.ListBuffer

object Main extends App{


  //1
  //Define a Scala function called circleArea that will calculate the area of a circle.
  // The function will be passed the radius of the circle.  (A = π*r2)

  def circleArea ( radius: Double) = {
    var result = math.Pi * radius * radius
    println(result)
  }

  //circleArea(8)
  //201.06

  //2
  //Define a Scala function called hypotenuse, which takes two arguments.
  // The two arguments are numbers, which represent the lengths of the legs of a right triangle.
  // The function returns a number representing the length of the hypotenuse of the right triangle.
  //Your answer should be a decimal, NOT a fraction.

  def hypotenuse(sideA: Double, sideB:Double) = {
    var result = sqrt((sideA * sideA) + (sideB * sideB))
    println(result)
  }

  //hypotenuse(3, 4)
  //5.0
  // hypotenuse (9, 16)
  //18.35755975068582

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


 // reverseDigits( 123)
  //321
  //reverseDigits( -42)
  //-24

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

  //conditionalRemove("a", List("b", "c", "e", "a", "g", "a"))
  //List("b", "c", "d", "e", "a","g")
  //conditionalRemove("a", List("b", "c", "e"))
  //List("b", "c", "e")

//5 ******** Check this *******
  //Write a Scala function called isPalindrome that takes a String, Int, or List and determines if it is a palindrome (i.e., the same forwards and backwards).
  // The comparison should be case-insensitive (i.e., case does NOT matter). Spaces and punctuation should be ignored.

  def isPalindrome( s: String) = {
    val sanitized = s.toLowerCase.replace(" ","").replaceAll("""[\p{Punct}]""", "")
    if (sanitized.reverse == sanitized){
      println(true)
    }
    else{
      println(false)
    }


  }

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

  isPerfectNumber(6)
  //(true, List( 1, 2, 3))
  isPerfectNumber(7)
  //(false,List(1))

//7
  //Write a Scala class to represent a ComplexNumber, i.e., a number with a real and an imaginary part.
  //Define the standard arithmetic operation on the class: Add, Subtract, Multiply, Divide, Conjugate, Reciprocal, and Abs.


  class ComplexNumber(realNumber: Int, imaginaryNumber : Int){

    //real number     //imaginary number
    def  get{
      val newNumber = realNumber + imaginaryNumber;
      newNumber.toString();
      return newNumber;
    }


    //Addition: (a +bi)+(c +di) = (a +c) + (b +d)i
   /* def addition(c:) = (realNumber, imaginaryNumber ) {

        //val additionOperation = realNumber + imaginaryNumber;
        //return additionOperation;

    }*/
    def add(c: ComplexNumber) = new ComplexNumber(realNumber + c.realNumber, imaginaryNumber + c.imaginaryNumber)

    //Subtraction: (a +bi) – (c +di) = (a-c) + (b-d)i
    def subtraction = (realNumber,imaginaryNumber ) : Unit{

    }

    // Multiplication: (a +bi) (c +di) = ac + bci + adi + bdi22 = (ac - bd) + (bc + ad) i
    def multiplication = (realNumber,imaginaryNumber ) : Unit{

    }

    //Division: (a+bi)(c+di)(a+bi)(c+di) = (ac+bd)(c2+d2)(ac+bd)(c2+d2) + (bc−ad)(c2+d2)i
    def division = (realNumber,imaginaryNumber ) : Unit{

    }

    //Conjugate
    //Multiply by (-1) then do opearation
    def conjugate = (realNumber,imaginaryNumber ) : Unit{

    }

    //Reciprocal
    // 1/ given complex number : 2-3i == 1/2-3i
    def reciprocal = (realNumber,imaginaryNumber ) : Unit{

    }

    //Abs ( Absolute Value )
    // \ 3-4i \  = 5
    // 3^2 + 4^2 = \ 3-4i \^2
    // 25 = \ 3-4i \^2
    // 5 \ 3-4i \
    def abs = (realNumber,imaginaryNumber ) : Unit{

    }

  }

  


//8
  //Write a Scala object named Stats with the following functions that accepts a list of Doubles.
  // (You may not use built-in functions that compute these functions for you.)


  def stats() ={


  }
  val list = List(1.0,2.2,3.3)

  def median() : List[Double] ={

    

  }


  //mean(List(Doubles…))
  // min(List(Doubles…))
  // max(List(Doubles…))
  //mode(List(Doubles…))
  //median(List(Doubles…))
//9
  //Write an object named FoldStats with the following functions implemented using the fold functions of Scala.
  // (You may not use built-in functions that compute these functions for you.)

    def FoldStats() ={


    }


//mean(List(Doubles…))
//min(List(Doubles…))
//max(List(Doubles…))
}