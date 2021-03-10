package com.scalap.scalaexamples

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random
import scala.util.Failure
import scala.util.Success
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.Promise

class FutureTest {
  
}

object FutureTest {
  
  var atomicnt = new AtomicInteger()
  var failurecnt = new AtomicInteger()
  
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
 
  def normalTask() =  {
    println("Starting normaltask")
    Thread.sleep(2000 + Random.nextInt(5000))
    if(Random.nextDouble() > 0.5) {
      println("Throwing random exception..")
      failurecnt.getAndIncrement
      throw new RuntimeException("Random exception from normalTask")
    }
    atomicnt.getAndIncrement
    println("Atomic count:"+ atomicnt.get)
    Thread.sleep(2000 + Random.nextInt(5000))
    println("Finished normaltask")
  }
  
  def testAwait() {
    val future = Future { 
      println("Inside future task")
      Thread.sleep(30000)
      println("Exiting future task")
    }
    Await.result(future, Duration.Inf)
    println("Exiting testAwait")
  }
  // Test when exception is thrown back from sequence of futures; and what happens to other futures
  def testException() = {
    val rg = (0 until 500)
    val futures = rg.map(i =>{
      Future(normalTask)
    })
    val lst = List("1", "failure", "3")
    //Await throws an exceptions but other futures continues to get submitted and run
    
    //throws an exception for failed ones but the other keep continue running
    //futures.map(Await.result(_, Duration.Inf))
    
    //Doesns't throw exception; just a failed future 
    //Await.ready(Future.sequence(futures), Duration.Inf)
    //Await.result(Future.sequence(futures), Duration.Inf)
    
     //throws an exceptions but other futures continues to get submitted and run
/*    Future.sequence(futures) onComplete {
      case Success(s) => {println("future completed successfully")}
      case Failure(t) => {
        println("An error has occurred: " + t.getMessage)
        println("Atomic count:"+ atomicnt.get)
        throw t
      }
    }*/
    
    //throws an exceptions but other futures continues to get submitted and run
    /*val futSum = Future.traverse(rg.toList)(i => Future(normalTask)).map(_.size)
    Await.result(futSum, Duration.Inf)*/
    
    //throws an exceptions but other futures continues to get submitted and run
    //val ft = Future.firstCompletedOf(futures).flatMap(s => Future.sequence(futures))
    
    //throws an exception and but other futures keeps running 
    //val res = failFast(futures)
    val se = futures.map(f => cancellable(f) {
          throw new RuntimeException
        })
    try {
      //Await.result(Future.sequence(futures), Duration.Inf)
      
      futures.map(Await.result(_, Duration.Inf))
      //Await.result(res, Duration.Inf)  
    } catch {
      case t: Throwable => {
        println("future failed")
        se.foreach( t => t._1())
      }
    }
    
    
  }
  
  def failFast[T](futures: Seq[Future[T]]): Future[Seq[T]] = {
    val promise = Promise[Seq[T]]
    futures.foreach{f => f.onFailure{case ex => promise.failure(ex)}}
    val res = Future.sequence(futures)
    promise.completeWith(res).future
  }
  
  def cancellable[T](f: Future[T])(customCode: => Unit): (() => Unit, Future[T]) = {
    val p = Promise[T]
    val first = Future firstCompletedOf Seq(p.future, f)
    val cancellation: () => Unit = {
      () =>
        println("canceling this future")
        first onFailure { case e => customCode}
        p failure new Exception
    }
    (cancellation, first)
  }
  
  
  def testFutures {
    
      // wait for both future to complete before exiting
      //Await.result(futureA zip futureB, Duration.Inf)
      //Await.result(futureA, Duration.Inf)
  }
  
  def batchProcessing() {
    
    try {
      println("Starting batchProcessing")
      testException()
      Thread.sleep(5000)
      println("Atomic count:"+ atomicnt.get)
      println("Exiting batchProcessing")      
    } catch {
      case t: Throwable => {
        println("Error in main")
        Thread.sleep(10000)
        t.printStackTrace()
        // retry logic goes here based on failure or entire batch will be skipped 
      }
    }
    
  }
  
  def main(args: Array[String]): Unit = {
    
    try {
      println("Starting Main")
      batchProcessing()
      
      println("Continuing Main")

      Thread.sleep(120000)
      println("Atomic count:"+ atomicnt.get)
      println("Failure count:"+ failurecnt.get)
      println("Exiting Main")      
    } catch {
      case t: Throwable => {
        println("Error in main")
        Thread.sleep(10000)
        t.printStackTrace()
        // retry logic goes here based on failure or entire batch will be skipped 
      }
    }

  }  
}