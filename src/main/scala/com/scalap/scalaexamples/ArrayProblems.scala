package com.scalap.scalaexamples

object ArrayProblems {
  
  object Easy {
    
    /**
     * https://leetcode.com/problems/sort-array-by-parity/
     * return an array consisting of all the even elements of A, followed by all the odd elements of A.
     * Input: [3,1,2,4]
     * Output: [2,4,3,1]
     */
    def sortArrayByParity(A: Array[Int]): Array[Int] = {
      
      A.sortWith((a,b) => a%2 < b%2)
        
    }      
    def sortArrayByParity2(A: Array[Int]): Array[Int] = {
      
      var i = 0
      var j = A.length 
      
      while(i < j) {
        val left = A(i) % 2
        val right = A(j) % 2
        
        if(left > right) {
          val tmp = A(i)
          A(i) = A(j)
          A(j) = tmp            
        }
        if(left == 0) i += 1
        if(right == 1) j -= 1  
      }
      
      A
    }    
    
    /**
     * https://leetcode.com/problems/squares-of-a-sorted-array/
     * Given an array of integers A sorted in non-decreasing order, return an array of the squares of each 
     * number, also in sorted non-decreasing order.
     * Input: [-4,-1,0,3,10]
     * Output: [0,1,9,16,100]
     * 
     */
    def sortedSquares(A: Array[Int]): Array[Int] = {
      A.map(n => n * n).sorted
    }
    
    //Two-pointer approach
    //https://leetcode.com/problems/squares-of-a-sorted-array/solution/
    def sortedSquares2(A: Array[Int]): Array[Int] = {
      
      var left = 0 
      var right = A.length - 1
      
      while(left <= right) {
        
        val mid = left + (right - left) / 2
        if(A(mid) < 0) left = mid + 1
        if(A(mid) >= 0) right = mid - 1
      }
      var i = left - 1
      var j = left 
      
      val res = new Array[Int](A.length)
      var t = 0
      while(i >= 0 && j < A.length) {
        if(A(i) * A(i) < A(j) * A(j)) {
          res(t) = A(i) * A(i)
          t += 1
          i -= 1
        } else {
          res(t) = A(j) * A(j)
          t += 1
          j += 1
        }
      }
      while(i >= 0) {
        res(t) = A(i) * A(i)
        t += 1
        i -= 1
      }
      while(j < A.length){
        res(t) = A(j) * A(j)
        t += 1
        j += 1          
      }
      res
    }
    
    /**
     * https://leetcode.com/problems/array-partition-i/
     */
    def arrayPairSum(nums: Array[Int]): Int = {
      
      val sorted = nums.sorted
      var sum = 0;
      for(i <- 0 until sorted.length by 2){
        sum += sorted(i)
      }
      sum
    }
    
    def bubbleSort(nums: Array[Int]): Array[Int] = {
      
      if(nums == null || nums.length ==  1) return nums
      for(i <- 0 until nums.length) {
        for(j <- 0 until nums.length - 1){
          if(nums(j) > nums(j+1)) {
            val tmp = nums(j)
            nums(j) = nums(j+1)
            nums(j+1) = tmp
          }
        }
      }
      nums
    }
    
    def insertionSort(nums: Array[Int]): Array[Int] = {
      
      if(nums == null || nums.length ==  1) return nums
      
      for(i <- 1 until nums.length){
        
        val key = nums(i)
        var j = i - 1
        while(j >= 0 && nums(j) > key) {
          nums(j+1) = nums(j)
          j -= 1
        }
        nums(j+1) = key
      }
      nums
    }
    
    /**
     * https://leetcode.com/problems/height-checker/
     * Return number of elements which are not in correct sorted order of height
     * Input: [1,1,4,2,1,3]
     * Output: 3
     * Explanation: 
     * Students with heights 4, 3 and the last 1 are not standing in the right positions.
     */
    //O(nlogn)
    def heightChecker(heights: Array[Int]): Int = {
       
      val sorted = heights.sorted
      var count = 0
      for(i <- 0 until heights.length) {
        if(heights(i) != sorted(i)) count += 1
      }
      count
    }
    //https://leetcode.com/problems/height-checker/discuss/300472/Java-0ms-O(n)-solution-no-need-to-sort
    //Countin Sort
    //O(n)
    def heightChecker2(heights: Array[Int]): Int = {
      
      val heightFreq = new Array[Int](101) //heights is limited to 100
      
      for(height <- heights) {
        heightFreq(height) += 1
      }
    
      var curHeight = 0
      var cnt = 0
      
      for(i <- 0 until heights.length) {
        while(heightFreq(curHeight) == 0) curHeight += 1
        if(curHeight != heights(i)) cnt += 1
        heightFreq(curHeight) -= 1
      }
      cnt
    }
    
    //https://leetcode.com/problems/remove-duplicates-from-sorted-array/
    object RemoveDuplicateFromSorted {
      
      def removeDuplicates(nums: Array[Int]): Int = {
        
        var i, j = 0
        while(j < nums.length) {
           nums(i) = nums(j)
           while(j < nums.length && nums(j) == nums(i)) {
             j += 1
           }
           i += 1
        }
        i 
      }    
    
      def removeDuplicatesumsSlowFastPointers(nums: Array[Int]): Int = {
        
        if(nums.length == 0 ) return 0
        var i = 0
        
        for(j <- 1 until nums.length) {
          if(nums(j) != nums(i)) {
            i += 1
            nums(i) = nums(j)
          }
        }
        i + 1
      }
      
      def removeDuplicatesumsSlowFastPointers2(nums: Array[Int]): Int = {
        
        if(nums.length == 0 ) return 0
        var i = 0
        for(j <- 1 until nums.length) {
          if(nums(j) == nums(j-1)) i += 1 
          else nums(j-i) = nums(j)
        }
        nums.length - i 
      }
      
    }
    
    //https://leetcode.com/problems/remove-element/
    object RemoveElement {
      
      def removeElement(nums: Array[Int], `val`: Int): Int = {
        if(nums == null ) return 0
        var i = 0
        val k = `val`
        for(j <- 0 until nums.length) {
          if(nums(j) != k) {
            nums(i) = nums(j)
            i += 1
          }
        }
        i + 1  
      }
      
      //Only replaces elements to be deleted
      /**
       * Given an array nums and a value val, remove all instances of that value in-place and return the new length.
       * Given nums = [0,1,2,2,3,0,4,2], val = 2,
       * Your function should return length = 5, with the first five elements of nums containing 0, 1, 3, 0, and 4.
       */
      def removeElement2(nums: Array[Int], `val`: Int): Int = {
        if(nums == null ) return 0
        var i = 0
        var n = nums.length
        val k = `val`
        while(i < n){
          if(nums(i) == k){
            nums(i) = nums(n-1)
            n -= 1
            //dont increment i so it can be checked again in case n-1 element is also k 
          } else {
            i += 1            
          }
        }
        n
      }
    }
    
    //https://leetcode.com/problems/plus-one/
    /**
     * Given a non-empty array of digits representing a non-negative integer, plus one to the integer.
     * Input: [1,2,3]
     * Output: [1,2,4]
     * Explanation: The array represents the integer 123.
     */
    object PlusOne {
      
      def plusOne(digits: Array[Int]): Array[Int] = {
        var j = digits.length - 1
        var lastSum = 0
        do {
          lastSum = digits(j) + 1
          digits(j) = lastSum % 10
          j -= 1
        } while( j >= 0 && lastSum > 9)
        
        //when all digit sums are greather then 9  
        if(lastSum > 9) {
          1 +: digits
        } else digits
        
      }
      
      def plusOneRec(digits: Array[Int]): Array[Int] = {
        
        @annotation.tailrec
        def plusOne(idx: Int): Array[Int] = {
          if(idx == -1) {
            1 +: digits
          } else if (digits(idx) < 9) {
            digits(idx) += 1
            digits
          } else {
            digits(idx) = 0
            plusOne(idx - 1)
          }
        }
        plusOne(digits.length - 1)
      }
      
    }   
    
    
  }
  
  object Medium {
    
      object FindDuplicate {
  
      /**
       * https://leetcode.com/problems/find-the-duplicate-number/
       * 
       * Given an array nums containing n + 1 integers where each integer is between 1 and n (inclusive), 
       * prove that at least one duplicate number must exist. Assume that there is only one duplicate number, find the duplicate one.
       * Input: [1,3,4,2,2]
       * Output: 2
       * 
       * Proving that at least one duplicate must exist in nums is simple application of the pigeonhole principle. 
       * Here, each number in nums is a "pigeon" and each distinct number that can appear in nums 
       * is a "pigeonhole". Because there are n+1n+1 numbers are nn distinct possible numbers, 
       * the pigeonhole principle implies that at least one of the numbers is duplicated.
       * 
       */
      //Floyd's Tortoise and Hare (Cycle Detection)
      def findDuplicate(nums: Array[Int]): Int = {
        var tortoise = nums(0)
        var hare = nums(0)
        do {
          tortoise = nums(tortoise)
          hare = nums(nums(hare)) 
          
        } while(tortoise != hare)
          
        // Find the "entrance" to the cycle.  
        var ptr1 = nums(0)
        hare  
        while(ptr1 != hare) {
          ptr1 = nums(ptr1)
          hare = nums(hare)
        }
        ptr1
      }
      //Following only work if all integer between 1 to n present in nums
      //O(n)
      def findDuplicate2(nums: Array[Int]): Int = {
        
        val max = nums.max
        val expectedSum = (1 to max).sum
        (nums.sum - expectedSum)/(nums.length - max) 
      }
      
      //https://leetcode.com/problems/find-the-duplicate-number/discuss/72841/Java-O(1)space-using-Binary-Search
      // O(nlog(n))
      def findDuplicateBS(nums: Array[Int]): Int = {
        
        var low = 1
        var high = nums.length - 1 //this give n
        
        while(low <= high) {
          var cnt = 0
          val mid = low + (high - low) / 2
          for(n <- nums) {
            if(n <= mid) cnt += 1
          }
          if(cnt <= mid) {
            low = mid + 1
          } else high = mid - 1
        }
        low
      }
    }
        
    /**
     * https://leetcode.com/problems/k-closest-points-to-origin/
     */
    def kClosest(points: Array[Array[Int]], K: Int): Array[Array[Int]] = {
      
      //points.sortWith((lt1, lt2)  => lt1(0) * lt1(0) + lt1(1) * lt1(1) <
          //lt2(0) * lt2(0) + lt2(1) * lt2(1))
      //points.sortBy(lt1  => lt1(0) * lt1(0) + lt1(1) * lt1(1))(Ordering[Int].reverse)   
      points.sortBy(lt1  => lt1(0) * lt1(0) + lt1(1) * lt1(1)).slice(0, K)   
      
    }
    
    def kClosest2(points: Array[Array[Int]], K: Int): Array[Array[Int]] = {
      
      val ord = Ordering.by[Array[Int], Int](t => t(0) * t(0) + t(1) * t(1)).reverse
      val priorityQ = scala.collection.mutable.PriorityQueue[Array[Int]]()(ord)
      priorityQ ++= points
      val res = for(i <- 0 until K) yield priorityQ.dequeue() 
      res.toArray
    }  
    
  }
  
  object Hard {
    
  }
  

  
  def main(args: Array[String]) {
    
    println(Easy.bubbleSort(Array(64, 34, 25, 12, 22, 11, 90)).mkString(","))
    println(Easy.insertionSort(Array(64, 34, 25, 12, 22, 11, 90)).mkString(","))
    
  }
    
    
  
  
}

