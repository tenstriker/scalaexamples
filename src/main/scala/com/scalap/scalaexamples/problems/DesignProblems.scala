package com.scalap.scalaexamples.problems

import scala.util.Random
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.locks.Lock

object DesignProblems {
  
  /**
   * https://leetcode.com/problems/design-hashmap/solution/
   */
  class MyHashMap {
    
    case class Pair[U, V](key: Int, var value: Int)
    
    class Bucket(lst: collection.mutable.ListBuffer[Pair[Int, Int]]) {
      
      def get(key: Int) : Int = {
        for(pair <- lst) {
          if(pair.key.equals(key)) return pair.value
        }
        -1
      }
      
      def put(key: Int, value: Int) {
        var found = false
        for(pair <- lst) {
          if(pair.key.equals(key)) {
            pair.value = value
            found = true
          }
        }
        if(!found) this.lst += Pair(key, value)
      }
      
      def remove(key: Int) {
        for(pair <- lst) {
          if(pair.key.equals(key)) {
            lst -= pair
          }
        }          
      }
    }

    val table = collection.mutable.ArrayBuffer[Bucket]()
    
    /** Initialize your data structure here. */
    val bucketSize: Int = 2069
    for(i <- 0 until bucketSize) {
      table += new Bucket(collection.mutable.ListBuffer[Pair[Int, Int]]())
    }
    
    /** value will always be non-negative. */
    def put(key: Int, value: Int) {
      val hash = key % this.bucketSize
      this.table(hash).put(key, value)
    }

    /** Returns the value to which the specified key is mapped, or -1 if this map contains no mapping for the key */
    def get(key: Int): Int = {
      val hash = key % this.bucketSize
      this.table(hash).get(key)
    }

    /** Removes the mapping of the specified value key if this map contains a mapping for the key */
    def remove(key: Int) {
      val hash = key % this.bucketSize
      this.table(hash).remove(key)
    }
  
  }
  
  class MyHashMap2 {
    
    import collection.mutable._
    private val size = 1024
    private val array: Array[ListBuffer[(Int, Int)]] = Array.fill(size)(ListBuffer.empty)
    private def getIndex(key: Int): Int = key % size
    
    def get(key: Int) : Int = {
      val lb = array(getIndex(key))
      lb.find(_._1.equals(key)).map(_._2).getOrElse(-1)
    }
    
    def put(key: Int, value: Int) {
      
      val lb = array(getIndex(key))
      val pair = lb.find(_._1.equals(key))
      if(pair.isDefined) lb -= pair.get
      lb += (key -> value)
      
    }
    
    def remove(key: Int) {
      val lb = array(getIndex(key))
      val pair = lb.find(_._1.equals(key))
      if(pair.isDefined) lb -= pair.get
    }
    
    
  }
  
  
  /**
   * https://leetcode.com/problems/lru-cache/
   * The cache is initialized with a positive capacity.
   * Both get and put are O(n) as List takes O(n) to find and remove key 
   * 
   */
  class LRUCache(_capacity: Int) {
    
    
      val map = collection.mutable.Map[Int, Int]()
      val list = collection.mutable.ListBuffer[Int]()
      
      def get(key: Int): Int = {
        if(map.contains(key)) updateList(key)
        map.getOrElse(key, -1)
      }
  
      def put(key: Int, value: Int) {
        if(!map.contains(key) && map.size == _capacity){
          map.remove(list.last)
          list -= list.last
        } 
        map.put(key, value)
        updateList(key)
      }
      
      private def updateList(key: Int) {
        list -= key
        key +=: list //prepend key to list
      }
  
  }
  
  /**
   * Scala Linkedhashmap
   */
  class LRUCache2(_capacity: Int) {
    
    
      val map = collection.mutable.LinkedHashMap[Int, Int]()
      
      def get(key: Int): Int = {
        val out = map.getOrElse(key, -1)
        if(map.contains(key)) {
          map.remove(key)
          map.put(key, out)
        }
        out
      }
  
      def put(key: Int, value: Int) {
        if(!map.contains(key) && map.size == _capacity){
          map.remove(map.head._1)
        }
        map.remove(key)
        map.put(key, value)
      }
      
  
  }
  
  /**
   * Java Linkedhashmap
   */
  class LRUCache3(_capacity: Int) {
    
    val map = new java.util.LinkedHashMap[Int, Int](_capacity, 0.75f, true) {
      override def removeEldestEntry(eldest: java.util.Map.Entry[Int, Int]) : Boolean = {
            return size() > _capacity
        }
    }
    
    def get(key: Int): Int = map.getOrDefault(key, -1)
    def put(key: Int, value: Int)  = map.put(key, value)
    
    
  }
  
  /**
   * LinkedHashMap- Doubly LInked list created manually
   */
  class LRUCache4(_capacity: Int) {
    
    case class DLNode(key: Int, var value: Int, var next: DLNode = null, var prev: DLNode = null)
    
    val map = collection.mutable.Map[Int, DLNode]()
    val capacity = _capacity
    
    val head = DLNode(0, 0)
    val tail = DLNode(0, 0)
    
    head.next = tail
    tail.prev = head
    
    head.prev = null
    tail.next = null
    
    private def prepend(node: DLNode) {
      
      node.next = head.next
      node.prev = head
      
      head.next.prev = node
      head.next = node
    }
    
    private def remove(node: DLNode) {
      
      node.prev.next = node.next
      node.next.prev = node.prev
      
    }
    
    
    private def moveToHead(node: DLNode) {
      
      remove(node)
      prepend(node)
    }
    
    def get(key: Int): Int = {
      
      val node = map.get(key)
      if(node.isDefined) {
        val res = node.get.value
        moveToHead(node.get)
        return res
      }
      -1
    }
    
    def put(key: Int, value: Int)  = {
      
      val node = map.get(key)
      
      if(node.isDefined) {
        node.get.value = value  
        moveToHead(node.get)
      } else {
        val node = DLNode(key, value)
        map.put(key, node)
        if(map.size > capacity) {
          map.remove(tail.prev.key)
          remove(tail.prev)
        } 
        prepend(node)
      }
      
    }
    
  }
  
  
    
  def testLRUCache() {
    val cache = new LRUCache(2)
    cache.put(2, 1)
    cache.put(1, 1)
    cache.get(2)
    cache.put(4, 1)
    cache.get(1)
    cache.get(2)
    
    println(cache)
  }
  
  /**
   * https://leetcode.com/problems/insert-delete-getrandom-o1/
   */
  class RandomizedSet() {

    /** Initialize your data structure here. */
    val map = collection.mutable.Map[Int, Int]()
    val lst = collection.mutable.ArrayBuffer[Int]()
    val rand = scala.util.Random
    
    /** Inserts a value to the set. Returns true if the set did not already contain the specified element. */
    def insert(`val`: Int): Boolean = {
        if(map.contains(`val`)) return false
        map += (`val` -> lst.size)
        lst += `val`
        true
    }

    /** Removes a value from the set. Returns true if the set contained the specified element. */
    def remove(`val`: Int): Boolean = {
      if(!map.contains(`val`)) return false
      val idx = map(`val`)
      val lastele = lst.last
      lst(idx) = lastele
      map.put(lastele, idx)
      map.remove(`val`)
      lst.remove(lst.size - 1)
      true
    }

    /** Get a random element from the set. */
    def getRandom(): Int = {
      lst(rand.nextInt(lst.size))
    }

  }
  /**
   * simpler but not optimized remove
   */
  class RandomizedSet2() {

    /** Initialize your data structure here. */
    val set = collection.mutable.Set[Int]()

    /** Inserts a value to the set. Returns true if the set did not already contain the specified element. */
    def insert(`val`: Int): Boolean = {
        if(set.contains(`val`)) return false
        set += `val`
        true
    }

    /** Removes a value from the set. Returns true if the set contained the specified element. */
    def remove(`val`: Int): Boolean = {
        if(set.contains(`val`)) {
          set.remove(`val`)
          true
        } else false
    }

    /** Get a random element from the set. */
    def getRandom(): Int = {
      val arr = set.toArray
      arr(scala.util.Random.nextInt(arr.size))
    }

  }
  
  /**
   * https://leetcode.com/problems/design-log-storage-system/
   */
  class LogSystem() {

    val map = collection.mutable.Map[String, Int]()
    
    //O(1)
    def put(id: Int, timestamp: String) {
      map.put(timestamp, id)    
    }

    //O(n) scan all entries
    def retrieve(start: String, end: String, granularity: String): List[Int] = {
    
      
      val idx = granularity match {
        
        case "Year" => 4
        case "Month" => 7
        case "Day" => 10
        case "Hour" => 13
        case "Minute" => 16
        case "Second" => 19
      }
      val frmKey = start.substring(0, idx)
      val toKey = end.substring(0, idx)
      
      val lst = collection.mutable.ArrayBuffer[Int]()
      map.foreach { case(k, v) =>
        val curkey = k.substring(0, idx)
        if(curkey.compareTo(frmKey) >= 0 && curkey.compareTo(toKey) <= 0) lst += v
      }
      lst.toList
    }  

  }  
  
  //Effecient than above
  class LogSystem2() {

    val map = new java.util.TreeMap[String, Int]()
    val max = "2017:12:31:23:59:59"
    
    //O(log(n))
    def put(id: Int, timestamp: String) {
      map.put(timestamp, id)    
    }

    //O(m) scan; findin position takes O(1)
    def retrieve(start: String, end: String, granularity: String): List[Int] = {
          
      val idx = granularity match {
        
        case "Year" => 4
        case "Month" => 7
        case "Day" => 10
        case "Hour" => 13
        case "Minute" => 16
        case "Second" => 19
      }
      
      val frmKey = start.toCharArray.zipWithIndex.map { t =>
        if(t._2 <= idx) t._1
        else if(t._1 == ':') ':'  
        else '0'
      }.mkString("")
      
      val toKey = end.substring(0, idx) + max.substring(idx)
      
      import scala.collection.JavaConverters._
      
      map.subMap(frmKey, true, toKey, true).values().asScala.toList
      
    }  

  }
  
  /**
   * https://leetcode.com/problems/time-based-key-value-store/
   */
  class TimeMap {

    /**
     * TreeMap is useful when order of arrival (timestamp) is not guaranteed; 
     * If order is guaranteed then List can be used as all timestamps will be naturally sorted for given key
     */
    val map = collection.mutable.Map[String, java.util.TreeMap[Int, String]]()
    
    /** Initialize your data structure here. */
    def set(key: String, value: String, timestamp: Int) {
        val tm = map.getOrElse(key, new java.util.TreeMap[Int, String]())
        tm.put(timestamp, value)
        map.put(key, tm)
    }

    def get(key: String, timestamp: Int): String = {
        if(!map.contains(key)) return ""
        
        val ts = map(key).floorEntry(timestamp)
        if(ts != null) ts.getValue else ""
    }

 }
  /**
   * https://leetcode.com/problems/time-based-key-value-store/solution/
   * Using Collections.BinarySearch
   * 
   * Assumes events timestamps are in order
   */
  class TimeMap2 {
    
  }
  
  /**
   * Implemetn own binary search
   * 
   * Assumes events timestamps are in order
   */
  class TimeMap3 {
    
  }  
  
  /**
   * https://leetcode.com/problems/design-hit-counter/
   * Return no of hits in past 5 mins
   * Think of continuous stream of hits 
   */
  object HitCounter {
    
    val hitlist = collection.mutable.Queue[Int]()
    var count = 0  //atomic count for multi threads
    
    def hit(timestamp: Int) {
      hitlist.enqueue(timestamp)
    }
    
    def getHits(timestamp: Int) : Int = {
      
      while(hitlist.nonEmpty && timestamp - hitlist.front >= 300) {
        hitlist.dequeue()
      }
      hitlist.size
    }
    
  }
  
  object HitCounter2 {
    
    val hitlist = new java.util.LinkedList[(Int, Int)]()
    var count = 0  //atomic count for multi threads
    
    def hit(timestamp: Int) {
      
      if(hitlist.isEmpty() || hitlist.getLast._1 != timestamp) {
        // Insert the new timestamp with count = 1
        hitlist.add((timestamp, 1))
      } else {
        // Update the count of latest timestamp by incrementing the count by 1
        val last = hitlist.getLast
        hitlist.removeLast()
        hitlist.add((timestamp, last._2 + 1))
      }
      count += 1
    }
    
    def getHits(timestamp: Int) : Int = {
      
      //cleanup and adjusting count
      while(!hitlist.isEmpty() && timestamp - hitlist.getFirst._1 >= 300) {
        count -= hitlist.getFirst._2
        hitlist.removeFirst()
      }
      count
    }
    
  }  
  
  /**
   * https://leetcode.com/problems/design-hit-counter/discuss/83483/Super-easy-design-O(1)-hit()-O(s)-getHits()-no-fancy-data-structure-is-needed!
   * hit[] array is wrapped around by modulo operation. 
   */
  object HitCounter3 {
    
    val times = Array.ofDim[Int](300) 
    val hits = Array.ofDim[Int](300)
    
    def hit(timestamp: Int) {
      val idx = timestamp % 300
      if(times(idx) != timestamp) {
        times(idx) = timestamp
        hits(idx) = 1
      } else {
        hits(idx) += 1
      }
    }
    
    def getHits(timestamp: Int) : Int = {
      var total = 0
      for(i <- 0 until 300) {
        if(timestamp - times(i) < 300) total += hits(i)
      }
      total
    }
    
  }
  
  /**
   * https://leetcode.com/problems/logger-rate-limiter/
   * Each unique message should only be printed at most every 10 seconds 
   * All messages will come in chronological order. Several messages may arrive at the same timestamp.
   */
  object LoggerLimiter {
    
    val map = collection.mutable.Map[String, Int]()
    
    def shouldPrintMessage(timestamp: Int, message: String): Boolean = {
      
      val curts = map.get(message)
      if(!curts.isDefined || timestamp - curts.get >= 10) {
        map.put(message, timestamp)
        return true
      } 
      false
    }
  }
  
  /**
   * https://leetcode.com/problems/moving-average-from-data-stream/
   */
  class MovingAverage(_size: Int) { 
    
    val que = new java.util.LinkedList[Int]()
    var sumsofar: Double = 0
    
    def next(`val`: Int): Double = {
    
      if(que.size() == _size) {
        sumsofar -= que.remove()
      }
      
      sumsofar += `val`
      que.add(`val`)
      sumsofar / que.size()
    }
  }
  
  class MovingAverage2(_size: Int) {
    
  }
  
  /**
   * https://leetcode.com/problems/design-bounded-blocking-queue/
   * soln:
   * https://www.javacodemonk.com/blocking-queue-implementation-in-java-044ee033#:~:text=A%20blocking%20queue%20allows%20multiple,primarily%20for%20producer%2Dconsumer%20queues.
   */
  class BoundedBlockingQueue[T] {
        
    import java.util.concurrent._
    
    val queue = new java.util.LinkedList[Int]()
    var capacity: Int = _
    var lock: Lock = new ReentrantLock
    val notFull = lock.newCondition()
    val notEmpty = lock.newCondition()
    
    def BoundedBlockingQueue(capacity: Int) {
      this.capacity = capacity
    }
    
    def enqueue(element: Int) {
      lock.lock()
      try {
        
        while(queue.size == capacity) {
          notFull.await()
        }
        queue.add(element)
        notEmpty.signal()
        
      } finally {
        lock.unlock()
      }
    }
    
    def dequeue() : Int = {
      lock.lock()
      
      try {
        while(queue.isEmpty()) {
          notEmpty.await()
        }
        val item = queue.remove()
        notFull.signal()
        item
      } finally {
        lock.unlock()
      }
    }
    
    def size() : Int = {
      lock.lock()      
      try {
        queue.size()
      } finally {
        lock.unlock()
      }
    }    
    
  }
  
  
  
  /* 
   * TODO: define inner class MergingIterator which inherits the methods of an Iterator
   * TODO: implement methods boolean hasNext, Integer next and void remove
   *
   * An Example 
   *
   * Input:
   *   Iterator<Integer>: [0, 1, 6] <=== Guaranteed to be independently sorted.
   *   Iterator<Integer>: [4, 5, 7]
   *   Iterator<Integer>: [2, 5, 8]
   * 
   * Output:
   *   =>>>> 0, 1, 2, 3, 4, 5, 6, 7, 8 ...
   */  
  class MergingIterator(itrArr: Array[Iterator[Int]]) extends scala.collection.Iterator[Int] {
   
    val indexes = Array.ofDim[Int](itrArr.size)
    
    val sortedlst = collection.mutable.PriorityQueue[Int]()(Ordering[Int].reverse)
    
    override def hasNext() : Boolean = {
      
      itrArr.exists(_.hasNext == true)
    }
    
    override def next() : Int = {
      
      itrArr.foreach { itr =>
        if(itr.hasNext) sortedlst.enqueue(itr.next)
      }
      
      sortedlst.dequeue()
    }
    
    def remove() {
      
      //Remove top element of queue if there are already sorted elements there
      if(sortedlst.nonEmpty) {
       
        sortedlst.dequeue() 
        
      } else { //iterate input iterators; add them to queue and then remove from que        
        next()        
      }
    }
  }
  
  class RequestWeightedLoadbalancer(servers: Array[String]) {
    
    /*implicit val serverOrdering = new Ordering[(Double, String)] {
     
      def compare(a: (Double, String), b: (Double, String)) = {
        a._1 compare b._1
      }
    }*/
    
    implicit def orderingByWeight: Ordering[(Double, String)] = Ordering.by(t => t._1)
    
    val serverLst = collection.mutable.PriorityQueue[(Double, String)]()(Ordering.by[(Double, String), Double](_._1).reverse)
    
    servers.foreach(server => serverLst.enqueue((0, server)))
    
    println("serverLst: " + serverLst.mkString(","))
    
    def selectServer(weight: Double) : String = {
      
      val server = serverLst.dequeue()
      serverLst.enqueue((server._1 + weight, server._2))
      
      println("serverLst: " + serverLst.mkString(","))
      server._2
      
    }
    
  }
  
  
  
  object Hard {
          
    /**
     * https://leetcode.com/problems/lfu-cache/
     * Soln:
     * https://leetcode.com/problems/lfu-cache/discuss/94521/JAVA-O(1)-very-easy-solution-using-3-HashMaps-and-LinkedHashSet
     * https://leetcode.com/problems/lfu-cache/discuss/94547/Java-O(1)-Solution-Using-Two-HashMap-and-One-DoubleLinkedList
     */
    class LFUCache(_capacity: Int) {

        def get(key: Int): Int = {
            0
        }
    
        def put(key: Int, value: Int) {
            
        }
    
    }
      
  }
  

/**
 * Your RandomizedSet object will be instantiated and called as such:
 * var obj = new RandomizedSet()
 * var param_1 = obj.insert(`val`)
 * var param_2 = obj.remove(`val`)
 * var param_3 = obj.getRandom()
 */
  def main(args: Array[String]) {
    
    val start = "2017:01:01:23:00:00"
    val idx = 4
    val frmKey = start.toCharArray.zipWithIndex.map { t =>
        if(t._2 <= idx) t._1
        else if(t._1 == ':') ':'  
        else '0'
      }.mkString("")
    //frmKey.foreach(print)  
    println("frmKey: "+ frmKey)
    
    val servers = Array("A", "B", "C")
    val lb = new RequestWeightedLoadbalancer(servers)
      
    println(lb.selectServer(2.0))
    println(lb.selectServer(1.0))
    println(lb.selectServer(3.0))
    println(lb.selectServer(2.0))
  }
    
}