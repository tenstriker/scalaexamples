package com.scalap.scalaexamples.problems

import scala.util.Random

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
        key +=: list 
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
      lst.remove(lst.size - 1)
      map.put(lastele, idx)
      map.remove(`val`)
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
 * Your RandomizedSet object will be instantiated and called as such:
 * var obj = new RandomizedSet()
 * var param_1 = obj.insert(`val`)
 * var param_2 = obj.remove(`val`)
 * var param_3 = obj.getRandom()
 */
  def main(args: Array[String]) {
    assert(1==2, "assertion failed")
  }
    
}