package com.scalap.scalaexamples.problems

import scala.collection.mutable.ListBuffer
import com.scalap.scalaexamples.ListNode


object SolutionMed {
  


  /**
   * https://leetcode.com/problems/daily-temperatures/
   */
  object DailyTemperature  {
    
    
    def dailyTemperatures1(T: Array[Int]): Array[Int] = {
      
      //java stack is used as scala stack doesnt have peek; 
      //scala last returns constant last value even if 
      //its removed during while loop which is incorrect for our case
      var stk = new java.util.Stack[Int]()
      val res = Array.ofDim[Int](T.length)
      for(i <- 0 until T.length) {
        while(!stk.isEmpty && T(i) > T(stk.peek())) {
           val idx = stk.pop()
           res(idx) = i - idx
        }
        stk.push(i)
      }     
      res      
    }     
    
    //Modify existing array
    def dailyTemperatures2(T: Array[Int]): Array[Int] = {
        
      var stk = new java.util.Stack[Int]()
      
      for(i <- 0 until T.length) {
        while(!stk.isEmpty() && T(i) > T(stk.peek)) {
           val idx = stk.pop()
           T(idx) = i - idx
        }
        stk.push(i)
      }
      while(!stk.isEmpty()) {
        T(stk.pop()) = 0
      }
      T
    }    
  }

  /**
   * https://leetcode.com/problems/number-of-islands/
   */
  object NumberOfIslands {
    
    def numberOfIslands(grid: Array[Array[Char]]) : Int = {
      
      if(grid.isEmpty) return 0
      
      var count = 0
      val m = grid.length
      val n = grid(0).length
      
      def dfs(i: Int, j: Int) {
        if(i < 0 || j < 0 || i >= m || j >= n || grid(i)(j) == '0') {
          return
        }
        grid(i)(j) = '0'
        dfs(i, j + 1)
        dfs(i + 1, j)
        dfs(i, j - 1)
        dfs(i - 1, j)
      }
      
      for(i <- 0 until m) {
        for(j <- 0 until n) {
          if(grid(i)(j) == '1') {
            count += 1
            dfs(i, j)
          }
        }
      }
      count
    }
  }
  
  /**
   * https://leetcode.com/problems/number-of-closed-islands/
   */
  object ClosedIslands {
    
    /**
     * First pass - Fill the border 0 with 1's as they are not eligible
     * Second pass - find remaining 0, count it, fill them with 1's to avoid recounting 
     * Time: O(n)
     * Memory: O(n) for stack
     */
    def closedIsland(grid: Array[Array[Int]]): Int = {
      
      var count = 0
      if(grid.isEmpty) return count
      
      val m = grid.length
      val n = grid(0).length
      
      def expandAndFill(i: Int, j: Int) {
        if(i < 0 || j < 0 || i >= m || j >= n || grid(i)(j) == 1) return
        grid(i)(j) = 1
        expandAndFill(i, j + 1)
        expandAndFill(i + 1, j)
        expandAndFill(i, j - 1)
        expandAndFill(i - 1, j)
      }
      
      //First pass - Fill the border 0 with 1's as they are not eligible
      for(i <- 0 until m) {
        for(j <- 0 until n) {
          if(i == 0 || j == 0 || i == m - 1 || j == n - 1)  expandAndFill(i, j)
        }
      }
      
      //Second pass - find remaining 0, count it, fill them with 1's to avoid recounting
      for(i <- 0 until m) {
        for(j <- 0 until n) {
          if(grid(i)(j) == 0) {
            count += 1
            expandAndFill(i, j)
          }
        }
      }
      count
      
    }
    
    /**
     * One pass only: return true if expansion doesn't start from boarder and contains 1 in all surroundings
     */
    def closedIsland2(grid: Array[Array[Int]]): Int = {
      
      var count = 0
      if(grid.isEmpty) return count
      
      var m = grid.length
      var n = grid(0).length
      
      def expandAndFill(i: Int, j: Int) : Boolean = {
        if(i < 0 || j < 0 || i >= m || j >= n) return false
        if(grid(i)(j) == 1) return true
        grid(i)(j) = 1
        //NOTE: bitwise & operator; so that it can continue expanding all surrounding nodes even in case 
        //first one returns false; this is necessary to fill all boarder islands with 1s 
        expandAndFill(i, j + 1) & expandAndFill(i + 1, j) & expandAndFill(i, j - 1) & expandAndFill(i - 1, j) 
      }
      
      for(i <- 0 until m) {
        for(j <- 0 until n) {
          if(grid(i)(j) == 0) {              
            if(expandAndFill(i, j)) count += 1
          }
        }
      }
      count
      
    }
    
  }
  
  /**
   * https://leetcode.com/problems/battleships-in-a-board/
   */
  object Battleships {
    
    def countBattleships(board: Array[Array[Char]]): Int = {
    
      if(board.isEmpty) return 0
      
      val rows = board.length
      val columns = board(0).length
      
      var count = 0
      for(i <- 0 until rows) {
        
        for(j <- 0 until columns) {
          
          if(board(i)(j) == 'X' && (j == 0 || board(i)(j-1) != 'X') && (i == 0 || board(i - 1)(j) != 'X')) {
            count += 1
          }
        }
      }
      
      count
    }    
    
  }
  
  /**
   * https://leetcode.com/problems/diagonal-traverse-ii/
   * matrix rows can have different length
   */
  object DiagonalTraverse2 {
    
    //https://leetcode.com/problems/diagonal-traverse-ii/discuss/597698/JavaC%2B%2B-HashMap-with-Picture-Clean-code-O(N)
    // Alternative: https://leetcode.com/problems/diagonal-traverse-ii/discuss/609299/Java-list-of-stacks-explained
    def findDiagonalOrder(nums: List[List[Int]]): Array[Int] = {
      
      if(nums == null || nums.isEmpty) return Array.emptyIntArray
      
      val map = collection.mutable.Map[Int, collection.mutable.ArrayBuffer[Int]]()
      var maxIndexSum = 0
      var n = 0 //count number of Ints to use Array later (optimization)
      
      for(i <- nums.length - 1 to 0 by -1) {
        
        for(j <- 0 until nums(i).length) {
          
          val indexSum = i + j
          val diagonalLst = map.getOrElse(indexSum, collection.mutable.ArrayBuffer[Int]())
          diagonalLst += nums(i)(j)
          map.put(indexSum, diagonalLst)
          
          maxIndexSum = Math.max(maxIndexSum, indexSum) //track max index sum for later use
          n += 1 //count number of Ints
        }
        
      }
      
      var arr = Array.ofDim[Int](n)
      //val arr = collection.mutable.ArrayBuffer[Int]()
      var arrIdx = 0
      for(i <- 0 to maxIndexSum) {
        val diagonalLst = map.get(i)
        if(diagonalLst.isDefined) {
          //diagonalLst.get.foreach(arr += _)
          diagonalLst.get.foreach { n =>
            arr(arrIdx) = n
            arrIdx += 1
          }
        }
      }
      
      arr.toArray
    }
    
  }
  
  /**
   * https://leetcode.com/problems/hand-of-straights/
   */
  object HandsOfStraights {
    
    /**
     * https://leetcode.com/problems/hand-of-straights/discuss/136200/Simple-Java-solution-using-priority-queue
     * Using PriorityQueue
     * T: O(nLogn)
     * S: O(n)
     */
    def isNStraightHand(hand: Array[Int], W: Int): Boolean = {
      
      if(hand.isEmpty) return false
      val que = new java.util.PriorityQueue[Int]()  
      
      for(h <- hand) {
        que.add(h)
      }
      
      while(!que.isEmpty()) {
        
        val i = que.remove()
        
        for(j <- 1 until W) {
          if(!que.remove(i + j)) //logN operation 
            return false
        }
      }
      true
    } 
    
    def isNStraightHand2(hand: Array[Int], W: Int): Boolean = {
      
      
      if(hand == null || hand.isEmpty) return false
      val map = scala.collection.mutable.TreeMap[Int, Int]()
      
      hand.foreach(n => map.put(n, map.getOrElse(n, 0) + 1))
      
      map.foreach { case(k, v) =>
        if(v != 0) {
          for(j <- k until k + W) {
            val jc = map.get(j)
            if(!jc.isDefined || jc.get < v) return false
            map.put(j, jc.get - v)
          }
        }
      }
      true
      
    }
    
  } 
  
  /**
   * https://leetcode.com/problems/count-good-meals/
   */
  object GoodMeals {
    

    def countPairs(deliciousness: Array[Int]): Int = {
      
      var res = 0
      val map = collection.mutable.Map[Int, Int]()
      val mod = 1000000007
      
      for(num <- deliciousness) {
        
        var power = 1
        for(i <- 0 until 22) {
          if(map.contains(power - num)) {
            res += map(power - num)
            res %= mod
          }
          power *= 2
        }
        map.put(num, map.getOrElse(num, 0) + 1)
      }
      res  
    }    
  }
  
  /**
   * https://leetcode.com/problems/robot-bounded-in-circle/
   */
  object RobotInCircle {

    /**
     * https://leetcode.com/problems/robot-bounded-in-circle/discuss/850969/My-Java-Solution-with-the-thought-process
     */
    def isRobotBounded(instructions: String): Boolean = {
      var x = 0
      var y = 0
      var direction = "North"
        
      for(ch <- instructions) {
        ch match {
          case 'G' => direction match {
            case "North" => y += 1
            case "South" => y -= 1
            case "East" => x += 1
            case "West" => x -= 1
          }
          case 'L' => direction match {
            case "North" => direction = "West"
            case "West" => direction = "South"
            case "South" => direction = "East"
            case "East" => direction = "North"
          }
          case 'R' => direction match {
            case "North" => direction = "East"
            case "East" => direction = "South"
            case "South" => direction = "West"
            case _ => direction = "North"
          }
        }
      }
      if(x == 0 && y == 0) return true
      if(direction.equals("North")) return false
      true
    }
  }
  
  /**
   * https://leetcode.com/problems/decode-string/
   */
  object DecodeString {
    
    /**
     * DFS using Stack
     * T: O(maxK * n)  maxK is the maximum value of kk 
     * S: O(n)
     */
    def decodeString(s: String): String = {
       
      var curString = new StringBuilder()
      val countStk = collection.mutable.Stack[Int]()
      val strStk = collection.mutable.Stack[StringBuilder]()
      
      var k = 0
      
      for(ch <- s) {
        
        if(ch.isDigit) {
          k = k * 10 + ch - '0'
          
        } else if(ch == '[') {
          
          // push the number k to countStack
          countStk.push(k)
          // push the currentString to stringStack
          strStk.push(curString)
          // reset currentString and k
          curString = new StringBuilder()
          k = 0
          
        } else if(ch == ']') {
          
          val decodedStr = strStk.pop()
          val k = countStk.pop()
          
          for(i <- 0 until k) {
            decodedStr.append(curString)
          }
          curString = decodedStr
          
        } else {
          curString.append(ch)
        }
        
      }
      
      curString.toString()
    }    
    
    /**
     * https://leetcode.com/problems/decode-string/discuss/210284/Java-Recursive
     */
    def decodeStringRecur(s: String): String = {
      ""
    }
  }
    
  
  def main(args: Array[String]) {
    
    val l1 = List(5,6,7)
    val l2 = List(5,8,7,9,4)
    
    DiagonalTraverse2.findDiagonalOrder(null)
    
    println("1.1.1.1".split("\\.").mkString("[.]"))
    
    val a = true
    val b = true
    val c = true
    val d = true
    println(a && b && c && d)
    
    /*println("Atoi: " + Atoi.myAtoi(""))
    println("Atoi: " + Atoi.myAtoi(null))
    println("Atoi: " + Atoi.myAtoi("2"))
    println("Atoi: " + Atoi.myAtoi("-23"))
    println("Atoi: " + Atoi.myAtoi("  12  "))
    println("Atoi: " + Atoi.myAtoi("   -65474 "))
    println("Atoi: " + Atoi.myAtoi("   -654743749827987298749832 "))
    println("Atoi: " + Atoi.myAtoi("   -  A 654 "))
    println("Atoi: " + Atoi.myAtoi("   -    654 "))
    println("Atoi: " + Atoi.myAtoi("   +    654 "))
    println("Atoi: " + Atoi.myAtoi("    654D akdsal as "))*/
    //println("Atoi: " + Atoi.myAtoi("   -91283472332 "))
    
    //println(ThreeSum.threeSum(Array(-1,0,1,2,-1,-4)))
  }

  
}