package com.scalap.scalaexamples

import scala.util.Try
import scala.util.Success
import scala.util.Failure

object ParCollTest {
  
  
    def main(args: Array[String]): Unit = {
    
      println("Starting Main")
      Try(testExceptions()) match {
        case Success(s) => println("everything fine" + s)
        case Failure(t) => {
          println("exc happened: " + t.getMessage)
          t.printStackTrace()
        }
      }
      
    }  
    
    def testExceptions() {
      val lst = Array(12,41,263,26,123,132,823,72,1)
      lst.par.foreach { num =>
        if(num == 123) {
          throw new Exception("got number 123")
        }
      }
    }
  
}