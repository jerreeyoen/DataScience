import java.io.IOException
//Author: Hanjie Yang
//Date: 02/25/2019
object Demo1 extends App {
  def sayHello(name: String) = "Hello, " + name
  println(sayHello("Jerry"))
  // compare with unit
  def sayHello1(name: String) :Unit = "Hello, " + name
  println(sayHello1("Jerry"))
  println("return type is unit, so there is nothing to return.")

  //trying with lazy value
  import scala.io.Source._
  lazy val lines = fromFile("C:/Users/jerre/Desktop/ScalaTest.txt").mkString
  println(lines)
  //use normal val to assign a document that doesn't exist.
  //val lines1 = fromFile("C:/Users/jerre/Desktop/ScalaTest1.txt").mkString
  //lazy with a document that doesn't exist.
  lazy val lines2 = fromFile("C:/Users/jerre/Desktop/ScalaTest1.txt").mkString
  //println(lines2)
  println("lazy function will not execute until you call it.")
//try and catch in Scala.
  try {
    throw new IllegalArgumentException("x should not be negative")
  } catch{
    case _:IllegalArgumentException => print("sorry, error!")
  }finally {
    print("\n release io resources !!")
  }

  try {
    throw new IOException("User defined Exception")
  }
  catch{
    case e1:IllegalArgumentException => print("\nillegal argument")
    case e2:IOException => print("\nIOexception")
  }
}
