package com.scalap.scalaexamples

import java.util.Arrays
import java.util.regex.Pattern

import scala.collection.mutable.ArrayBuffer

import org.apache.commons.lang3.StringUtils

import me.lemire.integercompression._
import me.lemire.integercompression.differential._
import scala.util.Random

object Measure extends Enumeration(1) {

  type Measure = Value
  val Average = Value("false") //, Percentile_25, Median, Percentile_75, Percentile_90, Percentile = Value
  val Percentile_25 = Value("false")
  //class MyVal(hasVariation: Boolean) extends Val(nextId, hasVariation)
  //protected final def Value(name: String, x : String): MyVal = new MyVal(name,x)


}

package myPackage {
  private[myPackage] class Test {
    def testmethod = println("Insided testmethod")
  }
  private[myPackage] object A extends Test {
    
  }
  object B extends myPackage.Test //Compile error: private class Test escapes its defining scope as part of type myPackage.Test
  private object C extends myPackage.Test // works since C is also private

  object Test {
     def apply() = new Test //error: private class Test escapes its defining scope as part of type myPackage.Test
   } 
   object Test2 {
      def apply() = new Test //works as ITest is public 
    }
}

package otherPackage {
  
  class Test3 {
    //val test = new myPackage.Test
  }
}

object ScalaTest2 {

  def mergeMutableMaps() {

    val map1 = collection.mutable.Map(1 -> Set(1, 11) , 2 -> Set(3, 12), 3 -> Set(4, 5))
    val map2 = collection.mutable.Map(1 -> Set(2, 11) , 2 -> Set(1, 3) )

    map1 ++= map2.map{ case (k,v) => k -> (v ++ map1.getOrElse(k, Set())) }

    map1.foreach(t2 => println(t2._1+","+t2._2))

  }

  def mergeMutableArray() {

    val comb1 = ArrayBuffer(1,2,4)
    val comb2 = ArrayBuffer(2,7,4,6)
    comb1 ++= comb2.flatMap(short => if(!comb1.contains(short)) Iterable(short) else Iterable())

    //comb1.foreach(println)
    println(comb1.mkString(","))

  }

  def mergeImmutableMaps() {

    val map1 = Map(1 -> Set(1, 11) , 2 -> Set(3, 12), 3 -> Set(4, 5))
    val map2 = Map(1 -> Set(2, 11) , 2 -> Set(1, 3) )

    val map3 = map1 ++ map2.map{ case (k,v) => k -> (v ++ map1.getOrElse(k, Set())) }

    map3.foreach(t2 => println(t2._1+","+t2._2))

  }

  private def getCobmbinationForBits(bitsCombination: String,
                                     sortedValues: List[Int]) = {

    var set = collection.mutable.Set[Int]()
    for( i <- 0 until bitsCombination.size) {
      if("1".equals(bitsCombination.charAt(i).toString())) {
        set += sortedValues(i)
      }
    }
    set
  }

  private def iteratorTest() {

    val list = List(1,2,3)

    val iterable = list.toIterable

    val newItr = iterable.drop(2)

    //newItr.foreach(println)
    //list.foreach(println)

    val iterator = list.toIterator
    iterator.drop(2)
    list.foreach(println)
    println("end of iteratorTest")

  }

  def testMutable(arr: collection.mutable.ArrayBuffer[Int]) {

    arr.remove(1)
  }

  def testRowkeyregex() {

    val roles = List(401,404)
    
    (0 /: roles)(_ + _)
    roles./:(0)(_  + _)
    val hies = List(301,302)
    val rowkey = "000028|200801|401|302|805|1999"
    val regexp = "[0-9\\|]{13}\\|4(01|02|04)\\|30(1|2)\\|80(1|2|3|4|5)\\|1(000|999)"
    val pattern = Pattern.compile(regexp)
    println("rowkey matched: " + rowkey.matches(regexp))

  }

  def testBenchRowkeyregex() {

    val roles = List(401,404)
    val hies = List(301,302)
    val rowkey = "200801|114|204|402|301|601|701|805|1999"
    val regexp = "[0-9]{6}\\|(111|112|114)\\|(201|204)\\|(401|402)\\|(301|302)\\|(601|602)\\|(701|705)\\|80(1|2|3|4|5)\\|1(000|999)"
    val pattern = Pattern.compile(regexp)
    println("rowkey matched: " + rowkey.matches(regexp))

  }

  def testThreshold {

    def strong(probs : Array[Double], threshold : Double = 0.5) = {
      probs.exists(_ > threshold)
    }

    def strongLeader(probs : Array[Double], epsilon: Double = 0.01) = {

    }

  }

  /**
   * http://stackoverflow.com/questions/27595868/java-split-string-which-is-alphanumeric
   *
   */
  def alpahNumericSplit {

    val strarr = Array("abac","123","123abc345def","CC23QQ21HD32", "123abc", "abc123")
    val p1 = "(?<=\\D)(?=\\d)|(?<=\\d)(?=\\D)"
    val p2 = "((?<=[a-zA-Z])(?=[0-9]))|((?<=[0-9])(?=[a-zA-Z]))"
    println(strarr.map(_.split(p2).mkString(",")).mkString("\n"))

    println(strarr.map(_.split(p2).map(_.replaceAll("\\d+", "*")).mkString(",")).mkString("\n"))
  }
  
  def randomPickFromLists(lists: Iterable[String]) = {
    
  }
  
  def randomPicker(list: List[String]) {
    
    val rand = new Random(System.currentTimeMillis())
    val random_index = rand.nextInt(list.length)
    val result = list(random_index)
  }

  class Rational(n: Int, d: Int) {

    println("Created"+n+"/"+d)
  }
  
  case class WeightedList(list: Iterable[String], weight: Double)
  
  def main(args: Array[String]) {

    val abc = collection.mutable.ArrayBuffer(1)
    def de = collection.mutable.ArrayBuffer(1)
    
    val someNumbers = List(-11, -10, -5, 0, 5,10)
    someNumbers.filter((x) => x>0).foreach(println)
    someNumbers.filter((x: Int) => x>0).foreach(println)
  
    new Rational(2, 3)
    
    val maps = 1 -> "orange"
    
    val lvl1 = ("Ujjwal Kumar",
                "Steve Eddy",
                 "Seema",
                 "Lauren Ruhstorfer",
                  "Karthik Dosapati",
                  "Karthik Baskaran",
                  "Himanshu",
                  "Ryan",
                  "Rajan",
                  "Pramod",
                  "Prashanthi",
                  "Gowri"
                )
    val lvl2 = ("Ravi Kumar Gaddabathini",
                "Mukul",
                "Anthony", 
                "Himanshu",
                "Ryan",
                "Kandarp Desai",
                "Gurtek Singh",
                "Justin Khuc",
                "Peter Tsan"
                )
    val lvl3 = ("Sumit nair",
                "Tino Granados",
                "Daniel Martinez",
                "Jaspal Bhamra",
                "Nirav Patel"
                )

    for(i <- 0 to 5) {
      de += i
    }
    println(de.mkString(","))

    val names = Array[String]("chris", "ed", "maurice")

    val capNames = for (e <- names) yield  e.capitalize

    println(capNames)

    println("159248851:2627".split(",").size)

    alpahNumericSplit

    val db = Array(0.9999999999906084, 9.087182546043768E-12, 3.044033058260346E-13,  0.002466529744795271)
    println(db)
    println(db.map(num =>  scala.math.BigDecimal(num).setScale(3, scala.math.BigDecimal.RoundingMode.FLOOR).toDouble).mkString(","))

    val clzz = "AnnualContractValue"
    val recall = 0.8723
    val stopPatterns = Seq("\\d{3,}", "[0-9]{1,3},[0-9].*")

    val arr1 = Array("annual", "test")//Array("annual", "test", "annual", "annual") //Array("annual", "contract", "value", "annual")

    val arr4 = Array(1, 4,3,8)
    val arr5 = arr4.sorted(Ordering[Int].reverse)
    println(arr5.mkString(","))

    val itr = arr1.sliding(2)
    val arr2 = itr.map(arr => arr.mkString("_")) ++ arr1 ++ Array(arr1.mkString("_"))
    println(arr2.toList.mkString(","))

    val arr3 = "	2010 * on-target 100 p1 a j1 perf. n bonus ? 12 asda 4 12ds 12klkll 1jjkhjk 123213jk h j k n ".split("\\s")
    println(arr3.filter(token => stopPatterns.forall(regex => !token.matches(regex))).mkString(","))
    println(arr3.filter(t => t.length() > 1 || StringUtils.isNumeric(t)).mkString(","))

    println("above,1%,recruitment:jobs,above-all,abc_def,xy-def".replaceAll("[:_-]+", " "))

    println("above,1%,recruitment:jobs,above-all,abc_def,xy-def,churn/retention".replaceAll("[/:_\\-]+", " "))

    val txt2 = "above,above'all, he's, 1%,recruitment:jobs,above-all,abc_def,xy-def,churn/retention, this text., this.text".replaceAll("[,/:_\\-]+", " ")
    println(txt2)
    println(txt2.replaceAll("\\p{Punct}+", ""))

    println(new StringBuilder("	2010 on-target perf. bonus (license) (finamount)").replaceAllLiterally("(", ""))

    println("%-30s%.3f".format(clzz, recall))

    println("%-20s %-10s %-10s %-10s %-10s".format("LabelName","LabelId","Precision","Recall","fMeasures"))
    println("%-20s %-10.2f %-10.2f %-10.2f %-10.2f".format("InvalidLabel", -1.0, 0.00, 0.00, 0.00))

    println("12,00,000".matches("[0-9]{1,3},[0-9].*"))

    mergeMutableArray

    val dob = 1.678827642999999E7
    //println(dob.round)

    //println("40|201501|805|1999|114|203|601|705|01011000010|01|0100".hashCode())
    //println("40|201501|805|1999|114|203|601|705|01011010010|01|010".hashCode())
    val arrb = collection.mutable.ArrayBuffer[Int](1,2,3,4,5,6,7,8)

    arrb.grouped(100).foreach(println)

    val sarr = "1,2,3,4,5".split(",")
    val (a,b,c) = (sarr(0),sarr(1),sarr(2))

    //testMutable(arrb)

    //println(arrb.mkString(","))
    //iteratorTest

    val arr = new Array[Byte](5)

    println("arr: ")
    arr.foreach(println)

   // val set0 = com.test.scalaexamples.getRoleSet.map(_.toInt).toList.sorted

    //val set1 = getCobmbinationForBits("00011000000", set0)
    //set1.foreach(println)

    var d1 = 4.0
    d1 += 2.0

    println(d1)

    mergeMutableMaps

    //println("2234".substring(3))
    val short = "2234".substring(3).toShort
    val seq = for(i <- 1 to short) yield 3
    println(seq)
    val shortlist = List[Short](12,124,5634,23,12312,512)
    println(shortlist.sorted)
    /*val sortedOrders = Array(1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
                                                     ,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                                     2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
                                                     ,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                                     2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
                                                     ,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                                     2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
                                                     ,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                                     2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,4,4,4,4
                                                     ,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
                                                     4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6
                                                     ,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
                                                     6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6
                                                     ,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
                                                     6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6
                                                     ,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
                                                     6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6
                                                     ,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6
                                                     ,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
                                                     6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6
                                                     ,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
                                                     6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6
                                                     ,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
                                                     6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6
                                                     ,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
                                                     6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
                                                     6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6
                                                     ,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
                                                     6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6
                                                     ,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
                                                     6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6)

    val compress = sortedOrders.map(int => (int,1)).groupBy(_._1).map{ case (k,v) => (k,v.map(_._2).reduce(_+_))}.toSeq.sortBy(_._1)
      .map(t => t._1.toString().length()+""+t._1+""+t._2).mkString(",")//.sortBy(_._1)
    println(compress)*/

    val sortedCreditAmnt = Array(656,135800,147282,162992,174000,184000,199000,201706,210000,222400,240000,251200,289800,295000,306000,320000,320600,331400,338166,380000,398000,402520,423620,424000,431400
                                 ,440800,442500,453954,456576,458600,462000,468000,475800,479180,482800,486000,487116,489584,503844,509600,510904,512414,519506,540800,574044,602000,602376,612400,646070,661724
                                 ,678000,679400,687050,688316,700390,719500,728000,774600,786848,788800,808560,814384,830000,834000,834000,868098,902300,903442,912000,948682,952000,973656,1027770,1031950,1036200
                                 ,1088356,1098000,1131236,1206800,1210000,1232680,1255172,1277714,1286764,1329500,1350238,1360424,1406400,1408152,1432106,1521586,1534540,1558236,1561544,1609100,1657684
                                 ,1688997,1706454,1751596,1755022,1758000,1769043,1786000,1835660,1886400,1889370,1935180,1976200,1994462,2017780,2068134,2082360,2141568,2166444,2249746
                                 ,2265500,2336740,2396608,2425000,2435146,2518300,2602050,2607460,2714874,2786200,2792920,2868500,2919188,2999154,3039056,3255964,3306778,3321204,3478350
                                 ,3745226,3750752,3902034,3924436,3955092,4055050,4100680,4129400,4389400,4694706,4809302,5373910,5736406,5814456,5859698,5938386,5946682,5961566,5968658,
                                                     5995704,6141094,6236624,6305402,6317328,6404788,6442568,6581432,6667130,6675746,6831966,7055324,7260520,7474424,7641560,7659030,7697924,7764826,7856598
                                                     ,7898340,8026758,8266454,8295574,8381572,8406384,8458804,8557408,8573233,8812239,8902866,8948998,9012418,9436628,9482434,9593534,9613334,9703020
                                                     ,9718234,9809756,9815600,9897030,9944132,9973352,10093530,10215462,10232136,10360847,10593488,10623542,10923766,11191378,11209058,11254630,11523632,11563042
                                                     ,11828884,11983230,12191866,12191888,12317982,12522488,12724860,12870616,12905796,13087140,13162882,13480000,13740854,13789346,13849754,13868572,13885002
                                                     ,14084758,14343550,14506320,14650784,14792606,15001164,15086132,15136602,15293272,15323468,15376590,15423722,15454828,15823025,17241488,17667023
                                                     ,17955248,18775233,20004584,21026894,21850144,22010453,22120045,22316988,22759188,23143262,23190578,23777528,24646712,34065597,37440458 )

    val iic = new IntegratedIntCompressor()
    val start = System.currentTimeMillis()
    val compressed = iic.compress(sortedCreditAmnt)
    val end = System.currentTimeMillis()
    val millis = (end - start)
    println("total time in millis: " + millis)
    println("compressed from "+sortedCreditAmnt.length*4+"B to "+compressed.length*4+"B");
    println(compressed.mkString(","))

    val start2 = System.currentTimeMillis()
    val uncompressed = iic.uncompress(compressed)
    println("total time in millis: " + (System.currentTimeMillis() - start2))
    println(uncompressed.mkString(","))

    println()
    println("Specialized sorting for sorted array")

    // next we compose a CODEC. Most of the processing
    // will be done with binary packing, and leftovers will
    // be processed using variable byte
    val codec =  new IntegratedComposition(new IntegratedBinaryPacking(), new IntegratedVariableByte());

    // output vector should be large enough...
    var compressed2 = new Array[Int](sortedCreditAmnt.length+1024)
    // compressed might not be large enough in some cases
    // if you get java.lang.ArrayIndexOutOfBoundsException, try allocating more memory

    /**
    *
    * compressing
    *
    */
    val inputoffset = new IntWrapper(0);
    val outputoffset = new IntWrapper(0);

    val start3 = System.currentTimeMillis()
    codec.compress(sortedCreditAmnt,inputoffset,sortedCreditAmnt.length,compressed2,outputoffset);
    println("total time in millis: " + (System.currentTimeMillis() - start3))
    System.out.println("compressed from "+sortedCreditAmnt.length*4+"B to "+outputoffset.intValue()*4+"B");

    // we can repack the data: (optional)
    compressed2 = Arrays.copyOf(compressed2,outputoffset.intValue());

    /**
    *
    * now uncompressing
    *
    */
    val recovered = new Array[Int](sortedCreditAmnt.length)
    val recoffset = new IntWrapper(0);
    val start4 = System.currentTimeMillis()
    codec.uncompress(compressed2,new IntWrapper(0),compressed2.length,recovered,recoffset);
    println("total time in millis: " + (System.currentTimeMillis() - start4))
    if(Arrays.equals(sortedCreditAmnt,recovered))
      System.out.println("data is recovered without loss");
    else
      throw new RuntimeException("bug"); // could use assert

    testRowkeyregex()
    testBenchRowkeyregex

  }
}
