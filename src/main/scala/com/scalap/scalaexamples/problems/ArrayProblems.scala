package com.scalap.scalaexamples.problems

import scala.collection.mutable.ListBuffer

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
            //dont inc/rement i so it can be checked again in case n-1 element is also k
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
    
      
    /**
     * https://leetcode.com/problems/missing-number/solution/
     * Given an array containing n distinct numbers taken from 0, 1, 2, ..., n, find the one that is missing from the array.
     * Input: [3,0,1]
     * Output: 2
     */    
    object MissingNumber {
      
      def missingNumber(nums: Array[Int]): Int = {
        val expectedCount = nums.size + 1
        val expected = (0 until expectedCount).sum
        (expected - nums.sum)    
      }        
      
    }    
    
    /**
     * https://leetcode.com/problems/find-pivot-index/
     * T: O(n)
     * S: O(1)
     */
    object PivotIndex {
      
      def pivotIndex(nums: Array[Int]): Int = {
        
        var sum = 0
        var leftsum = 0
        
        sum = nums.sum
        
        for(i <- 0 until nums.length) {
          if(leftsum == sum - leftsum - nums(i)) return i
          leftsum += nums(i)
        }
        -1
      }
      
    }
    
    /**
     * https://leetcode.com/problems/missing-ranges/
     */
    object MissingRanges {
      
      def findMissingRanges(nums: Array[Int], lower: Int, upper: Int): List[String] = {
        
        var lst = collection.mutable.ArrayBuffer[String]()
        var prev = lower
        
        def getRange(n1: Int, n2: Int) = {
          if(n1 == n2) n1.toString() else s"$n1->$n2"
        }
        
        for(n <- nums) {
          
          // not within the range yet
          if(n < prev) {
            
          } else if(n == prev) { //// continue to find the next one
            prev += 1
          } else {
            val range = getRange(prev, n - 1)
            lst += range
            prev = n + 1
          }
        }
        if(prev <= upper) lst += getRange(prev, upper)
        lst.toList
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
      
      //By modifying the array
      def findDuplicateInPlace(nums: Array[Int]): Int = {
        
        if(nums.isEmpty) return -1
        
        var cur = nums(0)
        var prev = 0
        do {
          
          val tmp = nums(cur)
          nums(cur) = tmp * -1
          prev = cur
          cur = tmp
        } while(cur > 0)
        prev
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

    //https://leetcode.com/problems/container-with-most-water/
    def maxArea(height: Array[Int]): Int = {
      var maxArea = 0
      var l = 0
      var r = height.length - 1
      
      while (l < r) {
        maxArea = Math.max(maxArea, Math.min(height(l), height(r)) * (r - l))
        if(height(l) < height(r)) l += 1 else r -= 1 
      }
      maxArea
    }

    //https://leetcode.com/problems/3sum/
    /**
     * Given an array nums of n integers, are there elements a, b, c in nums such that a + b + c = 0? 
     * Find all unique triplets in the array which gives the sum of zero.
     */
    //n^2 + nlong
    def threeSum(nums: Array[Int]): List[List[Int]] = {
      
      val sorted = nums.sorted
      val triplets = collection.mutable.ListBuffer[List[Int]]()
      
      for(i <- 0 until sorted.length - 2) {
        
        if(i == 0 || (sorted(i) != sorted(i-1))) { //skip same result
        
          val targetSum = 0 - sorted(i)
          var low = i + 1
          var high = sorted.length - 1
          
          while(low < high) {
            
            if(sorted(low) + sorted(high) == targetSum) {
              triplets += List(sorted(i), sorted(low), sorted(high))
              low += 1
              high -= 1
              while(low < high && sorted(low) == sorted(low - 1)) low += 1 //skip same num
              while(low < high && sorted(high) == sorted(high + 1)) high -= 1 //skip same num
              
            } else if(sorted(low) + sorted(high) < targetSum) {
              low += 1
            } else high -= 1
          }
        }
      }
      triplets.toList
    }    
    
    //https://leetcode.com/problems/4sum/
    //O(n^(k-1)) + nlog(n) (here k = 4)
    //Space:   O(n^(k-1)) for recursive stack
    /**
     * Given array nums = [1, 0, -1, 0, -2, 2], and target = 0.
    
    A solution set is:
    [
      [-1,  0, 0, 1],
      [-2, -1, 1, 2],
      [-2,  0, 0, 2]
    ]
     */
    object FourSum {
      
        
        def fourSum(nums: Array[Int], target: Int): List[List[Int]] = {
      
          val sorted = nums.sorted
          kSum(sorted, target, 4, 0).toList
          
        }
        def kSum(sorted: Array[Int], target: Int, k: Int, idx: Int) : ListBuffer[List[Int]] = {
    
          val res = collection.mutable.ListBuffer[List[Int]]()
          
          if(idx >= sorted.length) {
             return res
          } 
          //2-sum
          if(k == 2) {
            var low = idx
            var high = sorted.length - 1
            
            while(low < high) {
              if(sorted(low) + sorted(high) == target) {
                res += List(sorted(low), sorted(high))
                low += 1
                high -= 1
                while(low < high && sorted(low) == sorted(low - 1)) low += 1
                while(low < high && sorted(high) == sorted(high + 1)) high -= 1            
              } else if(sorted(low) + sorted(high) < target) {
                low += 1
              } else high -= 1
            }
            
          } else {
            var i = idx
            while(i < sorted.length - k + 1) {
              
              val tmp = kSum(sorted, target - sorted(i), k - 1, i + 1)
              if(tmp.nonEmpty){
                //append previous results at 0th position while backtracking and append that to final list as well
                res ++= tmp.map(sorted(i) :: _)
              }
              while (i < sorted.length-1 && sorted(i) == sorted(i+1)) {
                  //skip duplicated numbers
                  i += 1
              }
              i += 1
            }
          }
          res
        }    
    }
    
    //https://leetcode.com/problems/search-in-rotated-sorted-array/
    //https://leetcode.com/problems/search-in-rotated-sorted-array/discuss/14425/Concise-O(log-N)-Binary-search-solution
    /**
     * Input: nums = [4,5,6,7,0,1,2], target = 0
     * Output: 4
     */
    object RotatedSortArray {
        
        def search(nums: Array[Int], target: Int): Int = {
          
          var l = 0
          var h = nums.length - 1
          
          while(l < h) {
            
            val m = l + (h - l)/2
            if(nums(m) == target) return m 
            
            if(nums(m) > nums(h)) { //real mid on left side eg. 3,4,5,6,1,2
              if(target > nums(m) || target <= nums(h)) {
                l = m + 1
              } else {
                h = m
              }
            } else { //real mid is right side
              if(target > nums(m) && target <= nums(h)){
                l = m + 1
              } else {
                h = m
              }
            }
          }
          if(l == h && target == nums(l) ) l else -1     
        }
        
        //nums = [4,5,6,7,0,1,2], target = 0
        def search1(nums: Array[Int], target: Int): Int = {
          
          var l = 0
          var h = nums.length - 1
          
          while(l <= h) {
            
            val m = l + (h - l)/2
            if(nums(m) == target) return m
            
            //if left part is monotonically increasing, or the pivot point is on the right part
            if(nums(l) <= nums(m)) {
              if(target >= nums(l) && target < nums(m)){
                h = m - 1
              } else {
                l = m + 1
              }
            } else { //if right part is monotonically increasing, or the pivot point is on the left part
              if(target > nums(m) && target <= nums(h)){
                l = m + 1
              } else {
                h = m - 1
              }
            }
          }
          -1
        }
        
        //nums = [4,5,6,7,0,1,2], target = 0
        def search2(nums: Array[Int], target: Int): Int = {
          
          var l = 0
          var h = nums.length - 1
          
          while(l < h) {
            
             val m = l + (h - l)/2
            
             if(nums(m) > nums(h)) {
               l = m + 1
             } else {
               h = m
             }
          }
          // lo==hi is the index of the smallest value and also the number of places rotated.
          var rot = l
          l = 0
          h = nums.length - 1
          // The usual binary search and accounting for rotation.
          
          while(l <= h) {
            val m = l  + (h - l)/2
            val realM = (m + rot) % nums.length
            if(nums(realM) == target) return realM
            if(target > nums(realM)) l = m + 1 else h = m - 1
          }
          
          -1
          
        }
    }
    
    /**
     * https://leetcode.com/problems/find-minimum-in-rotated-sorted-array/
     */
    object FinMinInRotatedSortArray {
      
        def findMin(nums: Array[Int]): Int = {
          
          if(nums == null || nums.isEmpty) return -1
          if(nums.length == 1) return nums(0)
          
          var l = 0
          var h = nums.length - 1
          
              // if the last element is greater than the first element then there is no rotation.
        // e.g. 1 < 2 < 3 < 4 < 5 < 7. Already sorted array.
        // Hence the smallest element is first element. A[0]
          if(nums(h) > nums(0)) return nums(0)
          
          while(l <= h) {
            
            val m = l + (h - l)/2
            
            // if the mid element is greater than its next element then mid+1 element is the smallest
            // This point would be the point of change. From higher to lower value.
            if(nums(m) > nums(m + 1)) return nums(m + 1)
            
            // if the mid element is lesser than its previous element then mid element is the smallest
            if(nums(m) < nums(m - 1)) return nums(m)
            
            if(nums(m) > nums(h)) {
              l = m + 1
            } else {
              h = m - 1
            }
            
          }
          -1
        } 
    }
    
    /**
     * https://leetcode.com/problems/find-first-and-last-position-of-element-in-sorted-array/
     * 
     * Given an array of integers nums sorted in ascending order, 
     * find the starting and ending position of a given target value.
     * Input: nums = [5,7,7,8,8,10], target = 8
     * Output: [3,4]
     */
    object SearchRangeInSorted {
      
      def searchRange(nums: Array[Int], target: Int): Array[Int] = {
        
        var l = 0
        var h = nums.length - 1
        
        while(l < h) {
          
          val m = l + (h - l)/2
          if(target > nums(m)) l = m + 1
          else h = m //moving to left if target is same as nums(m) 
        }
        if(l == nums.length || nums(l) != target) return Array(-1, -1) 
        
        val start = l
        
        // l = 0 //no need to reset l to 0 because we can ignore all the elements till l
        h = nums.length - 1
        
        while(l < h) {
          val m = l + (h-l)/2 + 1	// Make mid biased to the right; otherwise range will stuck
          if(target < nums(m)) h = m -1
          else l = m
        }
        Array(start, h)
      }
      
      def searchRange2(nums: Array[Int], target: Int): Array[Int] = {
        
        def findGreaterOrEqual(target: Int) : Int = {
          
          var low = 0
          var high = nums.length //not nums.length -1 to handle case Array(1) and Array(2,2)
          
          while(low < high) {
             val mid = low + (high - low) /2
             if(target > nums(mid)) low = mid + 1
             else high = mid
          }
          low
        }
        val start = findGreaterOrEqual(target)
        if(start == nums.length || start != target) return Array(-1, -1)
        val end = findGreaterOrEqual(target + 1) - 1
        
        Array(start, end)
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
    

    /**
     * https://leetcode.com/problems/single-element-in-a-sorted-array/
     * 
     */
    object SingleNonDuplicate {
      
      /**
       * algorithm will still work even if the array isn't fully sorted. As long as pairs are always 
       * grouped together in the array 
       * Invariant property - subarray with the non duplicate number is always odd-lengthed,
       * Time: O(logN)
       * Space: O(1)
       */
      def singleNonDuplicate(nums: Array[Int]): Int = {
        
        if(nums.isEmpty) return -1
          
        var lo = 0
        var hi = nums.length - 1
        
        while(lo < hi) {
          val mid = lo + (hi - lo)/2
          val evenHalf = (hi - mid) % 2 == 0
          if(nums(mid + 1) == nums(mid)) {
            if(evenHalf) {
              lo = mid + 2
            } else {
              hi = mid - 1
            }
          } else if(nums(mid - 1) == nums(mid)) {
            if(evenHalf) {
              hi = mid - 2
            } else {
              lo = mid + 1
            }
            
          } else {
            return nums(mid)
          }
        }
        return nums(lo)    //lo or hi; both same    
      }
      
      /**
       * The single element is at the first even index not followed by its pair.
       * After the single element, the pattern changes to being odd indexes followed by their pair.
       * Consider only even indexes; if following element is its pair then single element has to be on right
       */
      def singleNonDuplicate2(nums: Array[Int]): Int = {
        
        if(nums.isEmpty) return -1
        
        var lo = 0
        var hi = nums.length - 1
        
        while(lo < hi) {
          var mid = lo + (hi - lo) / 2
          mid = if(mid % 2 == 1) mid - 1 else mid
          if(nums(mid + 1) == nums(mid)){
            lo = mid + 2
          } else {
            hi = mid
          }
        }
        nums(hi) //lo or hi; both same
      }
    }
    
    
    /**
     * https://leetcode.com/problems/k-diff-pairs-in-an-array/
     * Input: [3, 1, 4, 1, 5], k = 2
     * Output: 2
     * Explanation: There are two 2-diff pairs in the array, (1, 3) and (3, 5).
     * Although we have two 1s in the input, we should only return the number of unique pairs.
     * 
     * simpler version - forget about duplicate and solve; then consider duplicates
     */
    object KdiffPairs {
      
      def findPairs(nums: Array[Int], k: Int) : Int = {
        
        if(k < 0) return 0
        val kmap = collection.mutable.Map[Int, Int]()
        //Create a frequency map; frequency count of each element
        for(n <- nums) {
          kmap.put(n, kmap.getOrElse(n, 0) + 1)
        }
        
        var sum = 0
        kmap.foreach{
          case(i, v) => if(k == 0) {
            if(v >= 2) sum += 1 //if you have duplicate values then difference can always be 0
          } else {
            if(kmap.contains(i + k)) sum += 1
          }
        }
        sum
      }
      
      /**
       * two pointer approach
       * skip duplicates with while loop of if condition
       * 
       */
      def findPairsSort(nums: Array[Int], k: Int) : Int = {
        
        if(k < 0) return 0
        var sum = 0
        val sorted = nums.sorted
        var start = 0
        var end = 1
        while(end < sorted.length) {
          if(end <= start ||  sorted(end) - sorted(start) < k) end += 1
          //since array is sorted, first condition in following check if there are duplicate values side by side;
          //if so it just forward the start pointer
          else if((start > 0 && sorted(start) == sorted(start - 1)) || (sorted(end) - sorted(start) > k)) start += 1
          else { //sorted(end) - sorted(start) == k
            sum += 1
            start += 1
          }
        }
        sum
      }
      
      /**
       * Like aboe just a different style of iterating and checking conditions
       */
      def findPairsSort2(nums: Array[Int], k: Int) : Int = {
        
        if(k < 0) return 0
        var sum = 0
        val sorted = nums.sorted
        var start = 0
        var end = 1
        while(start < sorted.length && end < sorted.length) {
          if(end == start ||  sorted(end) - sorted(start) < k) end += 1
          else if(sorted(end) - sorted(start) > k) start += 1
          else { //sorted(end) - sorted(start) == k
            sum += 1
            start += 1
            //since array is sorted, first condition in following check if there are duplicate values side by side;
            //if so it just forward the start pointer
            while(start < sorted.length && sorted(start) == sorted(start - 1)) start += 1
          }
        }
        sum
      }      
    }
    
    /**
     * https://leetcode.com/problems/binary-subarrays-with-sum/
     */
    object NumSubarraysWithSum {
      
      def numSubarraysWithSum(A: Array[Int], S: Int): Int = {
        
        var res = 0
        
        def dfs(start: Int, cursum: Int) {
          if(cursum > S || start >= A.length) return
          val newsum = cursum + A(start)
          if(newsum == S) res += 1
          dfs(start + 1, newsum)
        }
        
        for(i <- 0 until A.length) {
          dfs(i, 0)
        }
        res
      }
      
      //prefix sum optimized; 
      def numSubarraysWithSum2(A: Array[Int], S: Int): Int = {
        
        if (A == null || A.length == 0) return 0
        var res = 0
        var sum = 0
        //frequency of presum; ie. how many subarray have sum upto value represented by index count
        //map can be used for simplicity
        // because A consist of only 1 and 0, the maximum possible prefix sum
        // value is A.length, add 1 to make room for the value of 0
        var presum = Array.ofDim[Int](A.length + 1)
        presum(0) = 1 // subarray of length 0 will have sum of 0
        for(num <- A) {
          sum += num        
          val target = sum - S
          if(target >= 0) res += presum(target)
          presum(sum) += 1
        }
        res
      }
  
    }
    
    /**
     * https://leetcode.com/problems/max-consecutive-ones-ii/
     */
    object MaxConsicutiveOnes2 {
      
      /**The result is made of "previous consecutive 1s" + 0 + "current consecutive 1s". Each time a 0 is encountered, 
       * the number of "current consecutive 1s" plus 1 becomes the number of "previous consecutive 1s".
       */
      def findMaxConsecutiveOnes(nums: Array[Int]): Int = {
        var max = 0
        var curcount = 0
        var prevcount = 0
        for(n <- nums) {
          if(n == 1) {
            curcount += 1
          } else {
            prevcount = curcount + 1
            curcount = 0
          }
          max = Math.max(max, curcount + prevcount)
        }
        max 
      }
      //Slight variation from above
      def findMaxConsecutiveOnes2(nums: Array[Int]): Int = {
        var max = 0
        var curcount = 0
        var prevcount = 0
        for(n <- nums) {
          if(n == 1) {
            curcount += 1
          } else {
            max = Math.max(max, curcount + prevcount)
            prevcount = curcount + 1
            curcount = 0
          }
        }
        Math.max(max, curcount + prevcount)
      }
      
      //More generic soln
      def findMaxConsecutiveOnes3(nums: Array[Int]): Int = {
        var max = 0
        var zero = 0
        var k = 1
        var prev = 0
        var cur = 0
        
        for(cur <- 0 until nums.length) {
        
          if(nums(cur) == 0) zero += 1
          while(zero > k) {
            if(nums(prev) == 0 ) zero -= 1
            prev += 1
          }
          max = Math.max(max, cur - prev + 1)
        }
        max
      }
      
      //slight variation from above
      def findMaxConsecutiveOnes4(nums: Array[Int]): Int = {
        var max = 0
        var zero = 0
        var k = 1
        var prev = 0
        
        for(cur <- 0 until nums.length) {
        
          if(nums(cur) == 0) zero += 1
          if(zero == k) {
            max = Math.max(max, cur - prev + 1)
          }
          while(zero > k) {
            if(nums(prev) == 0 ) zero -= 1
            prev += 1
          }
        }
        Math.max(max, nums.length - prev)
      }
      
    }
    
    /**
     * https://leetcode.com/problems/max-consecutive-ones-iii/
     */
    object MaxConsicutiveOnes3 {
      
      def longestOnes(A: Array[Int], K: Int): Int = {
        
        var max, zero, prev = 0
        
        for(cur <- 0 until A.length) {
          
          if(A(cur) == 0) zero += 1
          
          if(zero == K) max = Math.max(max, cur - prev + 1)
          
          while(zero > K) {
            if(A(prev) == 0) zero -= 1
            prev += 1
          }
        }
        Math.max(max, A.length - prev)
        
      }
      
      /**
       * https://leetcode.com/problems/max-consecutive-ones-iii/solution/
       */
      def longestOnes2(A: Array[Int], K: Int): Int = {
        
        var left, right = 0
        var k = K
        
        while(right < A.length) {
          
          // If we included a zero in the window we reduce the value of K.
          // Since K is the maximum zeros allowed in a window.
          if(A(right) == 0) k -= 1
          
          // A negative K denotes we have consumed all allowed flips and window has
          // more than allowed zeros, thus increment left pointer by 1 to keep the window size same.
          if(k < 0) {
            if(A(left) == 0) k += 1
            left += 1
          }
          right += 1
        }
        right - left //Or A.length - left
      }
      
    }
    
    /**
     * https://leetcode.com/problems/divide-array-in-sets-of-k-consecutive-numbers/
     */
    object PossibleToDivideInK {
      
      /**
       * https://leetcode.com/problems/divide-array-in-sets-of-k-consecutive-numbers/discuss/457586/Java-Map-Short-and-Concise
       */
      def isPossibleDivide(nums: Array[Int], k: Int): Boolean = {
        
        if(nums.isEmpty || k < 1 || nums.length % k != 0) return false
        
        var numsfreq = collection.mutable.Map[Int, Int]()
        
        for(n <- nums) {
          numsfreq.put(n, numsfreq.getOrElse(n, 0) + 1)
        }
        
        val sorted = nums.sorted
        
        for(n <- sorted) {
          if(numsfreq(n) > 0) {
            for(i <- n until n + k) {
              if(!numsfreq.contains(i) || numsfreq(i) == 0) return false
              numsfreq.put(i, numsfreq(i) - 1)
            }
          }
        }
        true
      }
      
      //https://leetcode.com/problems/divide-array-in-sets-of-k-consecutive-numbers/discuss/470238/JavaC%2B%2BPython-Exactly-Same-as-846.-Hand-of-Straights
      def isPossibleDivide2(nums: Array[Int], k: Int): Boolean = {
        
        import scala.collection.JavaConverters._
        
        if(nums.isEmpty || k < 1 || nums.length % k != 0) return false
        var numsfreq = new java.util.TreeMap[Int, Int]()
        for(n <- nums) numsfreq.put(n, numsfreq.getOrDefault(n, 0) + 1)
        for(n <- numsfreq.keySet().asScala){
          if(numsfreq.get(n) > 0) {
            for(i <- k - 1 to 0 by -1) {
              if(numsfreq.getOrDefault(n + i, 0) < numsfreq.get(n)) return false
              numsfreq.put(n + i, numsfreq.get(n + i) - numsfreq.get(n))
            }
          }
        }
        true
      }
      
    }
    
    /**
     * https://leetcode.com/problems/split-array-into-consecutive-subsequences/
     */
    object PossibleToDivide {
      
      /**
       * https://leetcode.com/problems/split-array-into-consecutive-subsequences/discuss/182460/Java-O(n)-Time-O(n)-Space-Solution-with-Detailed-Example
       * https://leetcode.com/problems/split-array-into-consecutive-subsequences/discuss/106496/Java-O(n)-Time-O(n)-Space
       * https://leetcode.com/problems/split-array-into-consecutive-subsequences/discuss/106514/C%2B%2BPython-Esay-Understand-Solution
       * 
       * We iterate through the array once to get the frequency of all the elements in the array
       * We iterate through the array once more and for each element we either see if it can be appended to a previously 
       * constructed consecutive sequence or if it can be the start of a new consecutive sequence. If neither are true, 
       * then we return false.
       */
      def isPossible(nums: Array[Int]): Boolean = {
        
        var numsFreq = collection.mutable.Map[Int, Int]()
        var appendFreq = collection.mutable.Map[Int, Int]()
        
        for(n <- nums) numsFreq.put(n, numsFreq.getOrElse(n, 0) + 1)
        
        for(i <- nums) {
          
          if(numsFreq(i) != 0) {
            
            if (appendFreq.getOrElse(i, 0) > 0) {
              appendFreq.put(i, appendFreq(i) - 1)
              appendFreq.put(i + 1, appendFreq.getOrElse(i + 1, 0) + 1)
            }
            else if(numsFreq.getOrElse(i + 1, 0) > 0 && numsFreq.getOrElse(i + 2, 0) > 0) {
              numsFreq.put(i + 1, numsFreq(i + 1) - 1)
              numsFreq.put(i + 2, numsFreq(i + 2) - 1)
              appendFreq.put(i + 3, appendFreq.getOrElse(i + 3, 0) + 1)
            } 
 
            else return false
            
            numsFreq.put(i, numsFreq(i) - 1)
          }
        }
        true
      }
      
      /**
       * More generic solution for any k
       * https://leetcode.com/problems/split-array-into-consecutive-subsequences/discuss/130452/20ms-Java-PriorityQueue-with-Explanations
       * 
       */
      def isPossible2(nums: Array[Int]): Boolean = {
        //TODO
        false
      }
      
    }
    
    /**
     * https://leetcode.com/problems/minimum-increment-to-make-array-unique/solution/
     */
    object MinimumIncrementForUnique {

      /**
       * https://leetcode.com/problems/minimum-increment-to-make-array-unique/discuss/279964/python-concise-easy-understanding-water-level-simulation
       */
      def minIncrementForUnique(A: Array[Int]): Int = {
        
        val sorted = A.sorted
        var level = -1
        var res = 0
        
        for(n <- sorted) {
          if(level < n) {
            level = n
          } else {
            level += 1
            res += level - n
          }
        }
                  
        res
      }   
      /**
       * https://leetcode.com/problems/minimum-increment-to-make-array-unique/solution/
       * https://leetcode.com/problems/minimum-increment-to-make-array-unique/discuss/198215/Java-O(n-%2B-m)-solution-without-sort
       */
      def minIncrementForUnique2(A: Array[Int]): Int = {
        var res = 0
        res
      }
    }
    
    /**
     * https://leetcode.com/problems/merge-intervals/solution/
     * O(n log n) //sorting
     * O(1)
     */
    object MergeIntervals {
      
      def merge(intervals: Array[Array[Int]]): Array[Array[Int]] = {
        
        if(intervals.isEmpty) return intervals
        
        
        val sorted = intervals.sortBy(arr => arr(0))
        
        val merged = collection.mutable.ArrayBuffer[Array[Int]]()
        
        merged += sorted(0)
        
        for(interval <- sorted.tail) {
        
          val lastmax = merged(merged.size - 1)(1)
          if(lastmax < interval(0)) { // Disjoint intervals, add the new interval to the list
            merged += interval
          } else { // Overlapping intervals, move the end if needed
            merged(merged.size - 1)(1) = Math.max(lastmax, interval(1))            
          }
          
        }
        
        merged.toArray
          
      }  
      
    }
    
    /**
     * Like MergeIntervals but merge across two different Sets2
     */
    object MergeIntervals2 {
      
      
      /**
       * Naive implementation that premerge two sets and then merges intervals
       */
      def unionRanges(set1: Array[Array[Int]], set2: Array[Array[Int]]) : Array[Array[Int]] = {
           
        if(set1.isEmpty) return set2
        if(set2.isEmpty) return set1
       
        //we can merge two sets since we are taking union after sorting
        val mergedSet = set1 ++: set2
        val sortedSet = mergedSet.sortBy(arr => arr(0))    
       
        val res = collection.mutable.ArrayBuffer[Array[Int]]()
       
        res += sortedSet(0)
       
        for(interval <- sortedSet.tail) {
                       
            val last = res(res.size - 1)
           
            //check if last range is non overlapping
            if(last(1) < interval(0)) {                
                res += interval
            } else { //last range is overlapping and can be expanded to include current one
                res(res.size - 1)(1) = Math.max(last(1), interval(1))
            }            
           
        }
       
        res.toArray
       
      }
      
      /**
       * Merging two sets without premerge
       */
      def unionRanges2(set1: Array[Array[Int]], set2: Array[Array[Int]]) : Array[Array[Int]] = {
       
        if(set1.isEmpty) return set2
        if(set2.isEmpty) return set1
               
        val sortedSet1 = set1.sortBy(arr => arr(0))    
        val sortedSet2 = set2.sortBy(arr => arr(0))    
       
        val res = collection.mutable.ArrayBuffer[Array[Int]]()
       
        res += sortedSet1(0)
        var curlst = 2 //tracks which list we need to lookup and compare against last element of `res`
        var ptr1 = 1 //cur pointer for list 1
        var ptr2 = 0 //cur pointer for list 2
       
        while(ptr1 < sortedSet1.size || ptr2 < sortedSet2.size ) {
                       
            val last = res(res.size - 1)
           
            val interval = if(curlst == 2) {                
                val it = sortedSet2(ptr2)            
                ptr2 += 1    
                it
            } else {
                val it = sortedSet1(ptr1)
                ptr1 += 1
                it
            }
           
            //overlapping
            if(last(1) >= interval(0)) {      
                val start = Math.min(last(0), interval(0))        
                val end = Math.max(last(1), interval(1))
                res(res.size - 1) = Array(start, end)
            } else { //last range is overlapping and can be expanded to include current one
                res += interval
            }           
            
            curlst = if(ptr2 <= sortedSet2.length - 1 && ptr1 <= sortedSet1.length - 1) {
                if(sortedSet1(ptr1)(0) < sortedSet2(ptr2)(0)) { //compare start of next intervals in both lists; small should be explore next as it has more chances of merging                                                                      //with existing one
                    1
                } else {
                    2
                }
            } else if(ptr2 >= sortedSet2.length) {
                1
            } else if(ptr1 >= sortedSet1.length) {
                2
            } else curlst
        }
       
        res.toArray
      }
      
    }
    
    /**
     * https://leetcode.com/problems/non-overlapping-intervals/
     * Classic Greedy problem: Interval Scheduling
     */
    object EraseOverlapIntervals {
      
      /**
       * Invariant: if two intervals are overlapping, we want to remove the interval that has 
       * the longer end point -- the longer interval will always overlap with more or the same number of 
       * future intervals compared to the shorter one
       */
      def eraseOverlapIntervals(intervals: Array[Array[Int]]): Int = {
        
        if(intervals.isEmpty) return 0
        val sorted = intervals.sortBy(arr => arr(1)) //sorting based on arr(0) works too
        var count, prev = 0
        
        for(i <- 1 until sorted.length) {
          if(sorted(prev)(1) > sorted(i)(0)) {
            if(sorted(prev)(1) > sorted(i)(1)) {
              prev = i
            }
            count += 1
          } else {
            prev = i
          }          
        }
        count
      }
      
      def eraseOverlapIntervals2(intervals: Array[Array[Int]]): Int = {
        
        if(intervals.isEmpty) return 0
        val sorted = intervals.sortBy(arr => arr(1)) //sorting based on arr(0) works too
        var count = 0
        var end = sorted(0)(1)
        
        for(i <- 1 until sorted.length) {
          if(sorted(i)(0) < end) {
            count += 1
          } else {
            end = sorted(i)(1)
          }          
        }
        count
      }      
      
    }
    
    /**
     * https://leetcode.com/problems/sort-colors/
     */
    object SortColors {
      
      
      def sortColors(nums: Array[Int]): Unit = {
        
        val colorCount = Array.ofDim[Int](3)
          
        for(n <- nums) {
          colorCount(n) += 1
        }
        
        for(i <- 0 until nums.length) {          
          if(colorCount(0) > i) nums(i) = 0
          else if(colorCount(0) + colorCount(1) > i) nums(i) = 1
          else nums(i) = 2
        }
      } 
      
      def sortColors2(nums: Array[Int]): Unit = {
        
        var color0, color1, color2 = 0
          
        for(n <- nums) {
          if(n == 0) color0 += 1
          if(n == 1) color1 += 1
          if(n == 2) color2 += 1
        }
        
        for(i <- 0 until nums.length) {          
          if(color0 > i) nums(i) = 0
          else if(color0 + color1 > i) nums(i) = 1
          else nums(i) = 2
        }
      }   
      
      //Single pass
      def sortColors3(nums: Array[Int]): Unit = {
        
        var n0 = 0
        var n2 = nums.length - 1
        
        var i = 0
        while(i <= n2) {
          if(nums(i) == 0) {
            nums(i) = nums(n0)
            nums(n0) = 0
            i += 1
            n0 += 1
          } else if (nums(i) == 2) {
            nums(i) = nums(n2)
            nums(n2) = 2
            n2 -= 1
          } else {
            i += 1
          }
        }
      }
    }
    
    /**
     * https://leetcode.com/problems/best-time-to-buy-and-sell-stock-ii/
     */
    object BestTimeToBuySell2 {
      
      def maxProfit(prices: Array[Int]): Int = {
        
        var maxProfit = 0
        val n = prices.length
        var i = 0
        
        while(i < n - 1) {
        
          while(i + 1 < n && prices(i) >= prices(i + 1)) i += 1
          val buy = prices(i)
          while(i + 1 < n && prices(i) <= prices(i + 1)) i += 1
          val sell = prices(i)
          maxProfit += sell - buy
        }
        maxProfit
      }
      
      def maxProfit2(prices: Array[Int]): Int = {
        
        var maxProfit = 0
        
        for(i <- 1 until prices.length) {
          if(prices(i) > prices(i - 1)) {
            maxProfit += prices(i) - prices(i - 1)
          }
        }
        maxProfit
      }
      
    }
    
    /**
     * https://leetcode.com/problems/contiguous-array/solution/
     */
    object ContiguousArray {
      
      def findMaxLength(nums: Array[Int]): Int = {
        
        val map = collection.mutable.Map[Int, Int]()
        var max = 0
        var count = 0
        map.put(0, -1) //count to array index map
        
        for(i <- 0 until nums.length) {
          
          val curnum = if(nums(i) == 1) 1 else -1
          count += curnum
          if(map.contains(count)) {
            max = Math.max(max, i - map(count))
          } else {
            map.put(count, i)
          }
            
        }
        max
      }
      
    }
    
  }
  
  
  
  object Hard {
    
  }
  

  
  def main(args: Array[String]) {
    
    println(Easy.bubbleSort(Array(64, 34, 25, 12, 22, 11, 90)).mkString(","))
    println(Easy.insertionSort(Array(64, 34, 25, 12, 22, 11, 90)).mkString(","))
    
    val nthbit = 1 << 3
    println("nthbit: "+ nthbit.toBinaryString)
    for(i <- 0 until 8) {
      val bitmask = Integer.toBinaryString(i | nthbit).substring(1)
      //println(bitmask)
    }
    
    val intervals = Array(Array(1,4),Array(0,4))//,Array(8,10))
    val sorted = intervals.sortBy(arr => arr(0))//(Ordering[Int].reverse)
    println(sorted.map(arr => arr.mkString(",")) mkString("\\"))
    
    
    
    
  }
    
    
  
  
}

