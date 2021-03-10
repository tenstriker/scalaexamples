package com.scalap.scalaexamples

import scala.util.Sorting
import scala.util.Try
import org.apache.commons.lang3.time.StopWatch



class MyClass[+T <: Number] {
  
}




object ScalaTest extends App {

  println("Inside scala Test")
  
  def something() = {
    println("calling something")
    1
  }
  
  def callSomething(x: Int) = {
    println("x1=" + x)
    println("x2=" + x)
  }
  
  def callSomething2(x: => Int) = {
    println("x1=" + x)
    println("x2=" + x)
  }  
  
  callSomething2(something())
  
  def add2(a: Int) = (b: Int) => a + b
  println(add2(20)(19))
  
  val dataPath = "/mds/.snapshot/nir0304/hive/warehouse/incentint.db"
  val parts = dataPath.split("/")
  val dbPath = dataPath.substring(dataPath.indexOf(parts(4)))
  println("volume: "+parts(1))
  println("dbPath: "+dbPath)
  
  val stopwatch = new StopWatch
  
  println(stopwatch.getTime)
  
  stopwatch.start()
  Thread.sleep(5000)
  println(stopwatch.getTime)
  
  stopwatch.reset()
  println(stopwatch.getTime)
  
  val exec = Try("-1h").filter(str => str.toInt > 0 ).getOrElse("infinity")
  println("exec: "+exec)
  
  var str = "abc"
  //str = 123
  
  var abc = "test"
  abc = "xyz"
  //abc = 1
 
  println("abc: "+abc)

  val testBus = "".split(",").toList
  val testBusinesses: List[String] = List[String]()
  println("testBusinesses.isEmpty: "+testBusinesses.isEmpty)
  println("testBus.isEmpty: "+testBus.isEmpty)

  case class wall(size: Int, color: String)

  val ordering = new Ordering[wall] {
	  override def compare(a: wall, b: wall) = {

	    import scala.math.Ordered.orderingToOrdered
      (a.size, a.color) compare (b.size, b.color)
	  }
	}

  val t10 = Tuple10[Int, Int, String, Int, Int, Double, Long, String, Double, Double](1,2,"ab",4,5,6.0,7l,"de",1.0,4)
  val ordering2 = implicitly[Ordering[Tuple9[Int, Int, String, Int, Int, Double, Long, String, Double]]].compare((1,2,"ab",4,5,6.0,7l,"de",1.0), (1,2,"ab",4,5,6.0,7l,"de",1.0))

  import scala.math.Ordered.orderingToOrdered
  //(1,2,"ab",4,5,6.0,7l,"de",1.0,2) compare (1,2,"ab",4,5,6.0,7l,"de",1.0,2)

  val walls = Array(wall(10,"black"), wall(12,"black"), wall(10,"amber"))
  Sorting.quickSort(walls)(ordering)

  walls.foreach(wall => println(wall.size+" "+wall.color))

  println(Math.round(12.299999))
  println(Math.round(12.499999))
  println(Math.round(12.500000))
  println(Math.round(12.51111))
  println(Math.round(12.60111))

  val itr = List(1,2,3).iterator
  val iterable: Iterable[Int] = 1 to 4

  println(iterable.head)

  iterable.foreach(println(_))

  val results2 =  Array[Int](0)
  results2.foldLeft(0)((c,r) => {
    println("inside foldleft")
    c + r
    })

  val seqa = collection.mutable.Seq.range(1, 12)
  println(0 to 12)
  println(seqa)

  Option(abc) match {

    case None => println("none")
    case Some(a) => println(a)

  }

  val line = "\t1021\t201401\t201401\t531955974\t545651999\t"
  val arr = line.split("\t",-1)
  arr.foreach(i => println("element "+ i))
  val Array(v1, v2, v3, v4, v5, v6, v7) = line.split("\t",-1)

  println(new StringBuilder(1).append(2).append(3))

  val l1 = Set(1,2,3,4)
  val l2 = Set(1,2,3,4)
  val l3 = Set(1,2,3,4)

  var tuples = collection.mutable.ArrayBuffer

  l1.foreach { i =>
    l2.foreach{ j =>
      l3.foreach{ k =>
        println(i, j, k)
      }
    }
  }

  var myArray : Array[String] = new Array[String](10);
  for(value : String <- myArray ) {
    println(value);
  }

  for(value : String <- myArray
    if value.endsWith("5");
    if value.indexOf("value") != -1 ) {

    println(value);
  }

  //val planJobs = new PlanJobs
  //val sparkJobs = new SparkJobs

  println("Filter.ROLE_FILTER_ID.id = "+Filter.ROLE_FILTER_ID.id)

}

object Filter extends Enumeration {

  	//Filter Constants ID
	val ROLE_FILTER_ID, HIERARCHY_FILTER_ID = Value

}

abstract class JobConf

class PlanConf extends JobConf {

  var str = "123"
}

object PlanConf{

  def parse = new PlanConf

}

class CreditConf //extends JobConf

abstract class Jobs {
  def init: Unit
  val contents: Array[String]
}

class SparkJobs[T <: JobConf](val contents: Array[String]) extends Jobs {

  def prepEnv(args: String) = {}

  override def init: Unit = {}

}

/*class PlanJobs(contents: Array[Int]) extends SparkJobs[PlanConf](contents)  {

  override def prepEnv(args: String) = {

    val planConf = PlanConf.parse

  }

}*/

class Func1[-T,+R]

class A
class B extends A

class D
class C extends D

abstract class Car{
  val model: String
}
abstract class Mazda extends Car{

}
