package com.scalap.scalaexamples

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class FutureTest {
  
}

object FutureTest {
  
  def taskA(): Future[Unit] = Future {
    println("Starting taskA")
    Thread.sleep(1000) // wait 1secs
    taskA1()
    taskA2()
    taskA3()
    println("Finished taskA")
  }
 
  def taskB(): Future[Unit] = Future {
    println("Starting taskB")
    Thread.sleep(2000) // wait 2secs
    println("Finished taskB")
  }
  
  def taskA1(): Future[Unit] = Future {
    println("Starting taskA1")
    Thread.sleep(1000) // wait 1secs
    println("Finished taskA1")
  }
  
  def taskA2(): Future[Unit] = Future {
    println("Starting taskA2")
    Thread.sleep(1000) // wait 1secs
    println("Finished taskA2")
  }
  
  def taskA3(): Future[Unit] = Future {
    println("Starting taskA3")
    Thread.sleep(1000) // wait 1secs
    println("Finished taskA4")
  }
 
  def main(args: Array[String]): Unit = {
    
    println("Starting Main")
    val futureA = taskA()
    //val futureB = taskB()
    println("Continuing Main")
    // wait for both future to complete before exiting
    //Await.result(futureA zip futureB, Duration.Inf)
    Await.result(futureA, Duration.Inf)
  }  
}