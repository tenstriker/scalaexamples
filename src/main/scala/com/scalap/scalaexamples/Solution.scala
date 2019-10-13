package com.scalap.scalaexamples

import sun.security.util.Length
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

class ListNode(var _x: Int = 0) {
 var next: ListNode = null
 var x: Int = _x
 
 def printLists() = {
   var cur = this
   while(cur.next != null) {
     print(cur._x + "->")
     cur = cur.next
   }
   println(cur._x + "->null")
 }
}

object Solution {
  
  
  /**
   * https://leetcode.com/problems/valid-perfect-square/  
   */
  object MathProblems {
    
    //Binary search
    def isPerfectSquare2(num: Int) : Boolean = {
      
      var n = num
      var l: Long = 1 
      var h: Long = num // long type to avoid 2147483647 case
  
      while(l <= h){
          
        val m : Long = l + (h -l)/2
  
        val sq : Long = m * m // long type to avoid 2147483647 case
        if(num == sq) return true
  
        if(sq > num) {
          h = m - 1
        } else {
          l = m + 1
        }
      }
      false
    }
    
    //A square number is 1+3+5+7+...,
    //time complexity is O(sqrt(n))
    def isPerfectSquare(num: Int) : Boolean = {
      
      var n = num
      var i = 1
      while(n > 0) {
        n -= i
        i += 2
      }
      n == 0
    }
    
    //https://leetcode.com/problems/sqrtx/
    def sqrt(x: Int) : Int = {
      
      if(x == 0) return 0
      var left = 1
      var right = x
      
      while(left <= right) {
        val mid = left + (right - left) / 2
        if(mid * mid > x) right = mid - 1
        else if(mid * mid < x) left = mid + 1
        else return mid
      }
      return right
    }
    
    //power in x^y
    def power(x: Int, y: Int) : Int = { 
      
        if(y == 0) return 1
        
        val d = power(x, y/2)
    
        if(y % 2 == 0) return d*d
        return x*d*d
    }  
    
    //https://leetcode.com/problems/binary-watch/
    //Just go through the possible times and collect those with the correct number of one-bits.
    def readBinaryWatch(num: Int): List[String] = {
      
      val times = collection.mutable.ArrayBuffer[String]()
      
      for(h <- 0 until 12){
        for(m <- 0 until 60){
          if(Integer.bitCount(h * 64 + m) == num){ // << 6 so minute doesnt overlap on hour
            times += "%d:%02d".format(h, m)
          }
        }
      }
      
      times.toList
    }
    
    def readBinaryWatchFn(num: Int): List[String] = {
      def getOneCount(n: Int): Int = n.toBinaryString.filter(_ == '1').length
      (0 until 12).flatMap { h =>
        (0 until 60).filter(m => getOneCount(m) + getOneCount(h) == num)
          .map(m => h.toString + ":" + "%02d".format(m)) }.toList
    }
    
    //https://leetcode.com/problems/arranging-coins/
    def arrangeCoins(n: Int): Int = {
      
      var sum: Long = 0
      var cnt = 0
      for(i <- 1 to n) {
          sum += i
          if(sum > n) return cnt else cnt += 1
      }
      cnt
    }
    
    def arrangeCoins2(n: Int): Int = {
      
      var i = 0
      var num = n
      while(num > 0) {
        i += 1
        num -= i
      }
      if(num == 0) return i else i - 1
    }

  }
  
  //https://leetcode.com/problems/reverse-integer/
  def reverseInt(x: Int): Int = {
    
    var rev = 0
    var xi = x // Math.abs(x)
    while(xi != 0) {
      val pop = xi % 10
      val newResult = rev * 10 + pop 
      //If overflow exists, the new result will not equal previous one.
      if ((newResult - pop) / 10 != rev) { return 0 } 
      rev = newResult
      xi /= 10
    }
    rev
  }
  
  def reverseInt2(x: Int): Int = {
    
    var rev = 0
    var xi = x // Math.abs(x)
    while(xi != 0) {
      val pop = xi % 10
      if (rev > Int.MaxValue/10 || (rev == Integer.MAX_VALUE / 10 && pop > 7)) return 0
      if (rev < Int.MinValue/10 || (rev == Integer.MIN_VALUE / 10 && pop < -8)) return 0
      val newResult = rev * 10 + pop 
      rev = newResult
      xi /= 10
    }
    rev
    
  }
    
    
    //https://leetcode.com/problems/search-insert-position/
    /**
     * 
     * Given a sorted array and a target value, return the index if the target is found. If not, 
     * return the index where it would be if it were inserted in order.
     * 
     * Input: [1,3,5,6], 5
     * Output: 2
     * 
     * Input: [1,3,5,6], 2
     * Output: 1
     * 
     */
    def searchInsert(nums: Array[Int], target: Int): Int = {
      
      if(nums == null || nums.isEmpty) return -1
      var i = 0
      while(i < nums.length && nums(i) < target) i += 1
      i        
    }
    
    //https://leetcode.com/problems/intersection-of-two-arrays/
    /**
     * Each element in the result must be unique.
     * Input: nums1 = [1,2,2,1], nums2 = [2,2]
     * Output: [2]
     * 
     * Input: nums1 = [4,9,5], nums2 = [9,4,9,8,4]
     * Output: [9,4]
     */
    def intersection(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {

      val bitset1 = collection.mutable.BitSet(nums1: _*)
      val bitset2 = collection.mutable.BitSet(nums2: _*)
      bitset1.intersect(bitset2).toArray
      
    }
        
    //https://leetcode.com/problems/palindrome-number/
    //start reversing number till almost halfway (xi > rev)
    def isPalindrome(x: Int): Boolean = {
      //can't start with 0 so can not end with 0
      if(x < 0 || (x%10 == 0 && x != 0)) return false  
      var xi = x
      var rev = 0
      //only iterating until middle 
      while(xi > rev) {
        rev = rev * 10 + xi%10
        xi /= 10
      }
      xi == rev || xi == rev/10
    }

    
    class ListNodeList {
      
       var head: ListNode = null
       
       def insert(x : Int) = {
         if(head == null) head = new ListNode(x)
         else {
           var cur = head
           while(cur.next != null) cur = cur.next
           cur.next = new ListNode(x)
         }
       }
       
       def printLists() = {
         var cur = head
         while(cur.next != null) {
           print(cur._x + "->")
           cur = cur.next
         }
         println(cur._x + "->null")
       }
    }

    object LinkedList {
      
      //https://leetcode.com/problems/palindrome-linked-list/
      def isPalindromeReverseHalf(head: ListNode): Boolean = {
        
        if(head == null ) return true
        //find the length; reverse first half and compare it to second
        var len = 0
        var copy = head
        while(copy != null) {
          len += 1
          copy = copy.next
        }
        var prev: ListNode = null
        var cur = head
        //reverse first half
        for(i <- 0 until len/2) {
          val tmp = cur.next
          cur.next = prev
          prev = cur
          cur = tmp
        }
        //for odd number of nodes increment one
        if(len % 2 != 0) cur = cur.next
        
        //compare cur and prev which is reversed now
        while(cur != null) {
          if(cur._x != prev._x) return false
          cur = cur.next
          prev = prev.next
        }
        true
        
      }
      
      //https://leetcode.com/problems/remove-linked-list-elements/
      /**
       * Input:  1->2->6->3->4->5->6, val = 6
       * Output: 1->2->3->4->5
       */
      def removeElements(head: ListNode, `val`: Int): ListNode = {
          
        if(head == null) return head
        val dummy = new ListNode()
        dummy.next = head
        var cur = dummy
        while(cur.next != null){
          
          if(cur.next._x == `val`) {
            cur.next = cur.next.next
          } else {
            cur = cur.next
          }
        }
        dummy.next  
      }
      
      def removeElementsRec(head: ListNode, `val`: Int): ListNode = {
          
        if(head == null) return head
        head.next = removeElementsRec(head.next, `val`)
        if(head._x == `val`) head.next else head
      }
      
       def reverseList(head: ListNode): ListNode = {
         
         if(head == null || head.next == null) return head
         var cur = head
         var prev: ListNode = null
         while(cur != null){
           val tmp = cur.next
           cur.next = prev
           prev = cur
           cur = tmp
         }
         prev
       }
     
      def reverseListRecurse(head: ListNode): ListNode = {
        
        if(head == null || head.next == null ) return head
        
        var nxt = reverseListRecurse(head.next)
        head.next.next = head
        head.next = null
        nxt //just preserving pointer to last node while backtracking  
      }
      
      /**
       * https://leetcode.com/problems/middle-of-the-linked-list/
       * Input: [1,2,3,4,5]
       * Output: Node 3 from this list (Serialization: [3,4,5])
       */
      def middleNode(head: ListNode): ListNode = {
        
        var slow = head
        var fast = head
        while(fast != null && fast.next != null) {
          slow = slow.next
          fast = fast.next.next
        }
        slow
      }
     
      
      //https://leetcode.com/problems/merge-two-sorted-lists/
      def mergeTwoSortedLists(l1: ListNode, l2: ListNode): ListNode = {
        
        var dummyHead: ListNode = new ListNode()
        var cur: ListNode = dummyHead
        var l1t = l1
        var l2t = l2
        
        if(l1 == null) return l2
        if(l2 == null) return l1
        
        while(l1t != null && l2t != null) {
          if(l1t.x <= l2t.x) {
            cur.next = new ListNode(l1t.x)
            l1t = l1t.next
          } else {
            cur.next = new ListNode(l2t.x)
            l2t = l2t.next
          } 
          cur = cur.next
        }
        if(l1t != null) {
          cur.next = l1t 
        } else if(l2t != null) {
          cur.next = l2t
        }
        dummyHead.next
      }
      
      def mergeTwoSortedListsInPlace(l1: ListNode, l2: ListNode): ListNode = {
        
        var c1 = l1
        var c2 = l2
        
        if(l1 == null) return l2
        if(l2 == null) return l1
        
        var dummy = new ListNode()
        var prev = dummy
        prev.next = c1
        
        while(c1 != null && c2 != null) {
          if(c1.x <= c2.x) {
            prev=c1
            c1 = c1.next
          } else {
            val tmp = c2.next
            prev.next = c2
            c2.next = c1
            prev = c2
            c2 = tmp
          } 
        }
        if(c2 != null) {
          prev.next = c2 
        } 
        dummy.next
      }
      
      
      //merge using recursion; need to convert to scala
      
     /* public ListNode mergeTwoLists(ListNode l1, ListNode l2){
      		if(l1 == null) return l2;
      		if(l2 == null) return l1;
      		if(l1.val < l2.val){
      			l1.next = mergeTwoLists(l1.next, l2);
      			return l1;
      		} else{
      			l2.next = mergeTwoLists(l1, l2.next);
      			return l2;
      		}
      }*/
      
     def deleteDuplicates(head: ListNode): ListNode = {
       
       var cur = head
       while(cur != null && cur.next != null){
         if(cur.next._x == cur._x) {
           cur.next = cur.next.next
         } else {
           cur = cur.next   
         }         
       }
       head 
     }
      
      def hasCycle(head: ListNode) : Boolean = {
        
        if (head == null || head.next == null) return false 
        var slow = head
        var fast = head
        
        while(fast != null && fast.next != null) {
          slow = slow.next
          fast = fast.next.next
          if(slow == fast) return true
        }
        
        false
      }
      
      def hasCycle2(head: ListNode) : Boolean = {
        
        if(head == null || head.next == null) return false
        
        var slow = head
        var fast = head.next
        while(slow != fast) {
          if(fast == null || fast.next == null) return false
          slow = slow.next
          fast = fast.next.next
        }
        true
      }
      
    }
    
    
    //https://leetcode.com/problems/excel-sheet-column-title/
    def convertToTitle(n: Int) {
      
      val result = new StringBuilder()
      var p = n
      while(p > 0){
          p -= 1
          result.insert(0, ('A' + p % 26).toChar)
          p /= 26
      }
      result.toString()
    }
    
    def convertToTitleRecur(n:Int) : String = {
      
      if(n == 0)  "" else {
        val p = n - 1
        convertToTitleRecur(p / 26) + ('A' + (p % 26)).toChar
      }
    }
    
    

    
    object ArrayProblems {
      
      def twoSum(nums: Array[Int], target: Int): Array[Int] = {
        
          //val validNums = nums.filter(_ <= target)
          //println("validNums: "+validNums.mkString(","))
          val validNums = nums
          for(i <- 0 until validNums.length;
              j <- i+1 until validNums.length) {
            
            if(validNums(i) + validNums(j) == target) {
              return Array[Int](i, j)
            }
          }    
          return Array[Int]() 
      }
      
      def twoSumOpt1(nums: Array[Int], target: Int): Array[Int] = {
        
        var diffMap = collection.mutable.Map[Int, Int]()
        
        for(i <- 0  until nums.length) {
          if(diffMap.contains(nums(i))) {
            return Array(diffMap(nums(i)), i)
          } else {
            diffMap.put(target - nums(i), i)
          }
        }
        Array[Int]() 
        
      }
      
      def twoSumSorted(numbers: Array[Int], target: Int): Array[Int] = {
        
        var left = 0
        var right = numbers.length - 1
        
        while(numbers(left) + numbers(right) != target) {
          if(numbers(left) + numbers(right) > target) {
            right -= 1
          } else left += 1
        }
        Array(left+1, right+1)
      }
      
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
      
      //https://leetcode.com/problems/pascals-triangle/submissions/
      object PascalTriangle {
        
        def generate(numRows: Int): List[List[Int]] = {
            if(numRows == 0) return List()
            if(numRows == 1) return List(List(1))
            var finalArr = collection.mutable.ListBuffer[List[Int]]()
            var prevArr = List(1)
            finalArr += prevArr
            
            for(n <- 0 until numRows - 1) {
              var curArr = collection.mutable.ListBuffer(prevArr(0))
              var i = 0
              while(i + 1 < prevArr.length) {
                curArr += prevArr(i) + prevArr(i + 1)
                i += 1
              }
              curArr += 1
              prevArr = curArr.toList
              finalArr += curArr.toList
            }
            finalArr.toList
        }
        
        def generate2(numRows: Int): List[List[Int]] = {
          if(numRows == 0) return List()
          if(numRows == 1) return List(List(1))
          val finalArr = collection.mutable.ListBuffer[List[Int]](List(1))
          
          for(n <- 0 until numRows - 1) {
            val prevArr = finalArr(n)
            val curArr = collection.mutable.ListBuffer[Int](1)
            var i = 0
            while(i + 1 < prevArr.length) {
              curArr += prevArr(i) + prevArr(i + 1)
              i += 1
            }
            curArr += 1
            finalArr += curArr.toList
          }
          finalArr.toList
        }
        
        def generateRecurs(numRows: Int): List[List[Int]] = {
          
          if(numRows == 0) return List()
          if(numRows == 1) return List(List(1))
          
          @annotation.tailrec
          def pascal(lst: collection.mutable.ListBuffer[List[Int]], n: Int) : List[List[Int]] = {
            
            if(n == 0) return lst.toList
            val prevArr = lst.last
            var curArr = collection.mutable.ListBuffer(prevArr(0))
            var i = 0
            while(i + 1 < prevArr.length) {
              curArr += prevArr(i) + prevArr(i + 1)
              i += 1
            }
            curArr += 1
            lst += curArr.toList
            pascal(lst, n -1)
          }
          
          pascal(collection.mutable.ListBuffer[List[Int]](List(1)), numRows - 1)
        }
        
        def generateRecurs2(numRows: Int): List[List[Int]] = {
          
          
          @annotation.tailrec
          def loop(prev: List[Int], cur: List[Int]) : List[Int] = prev match {
            case h :: Nil => cur :+ 1
            case x1 :: x2 :: xs => loop(x2 :: xs, cur :+ (x1 + x2))
          }
          
          @annotation.tailrec
          def pascal(lst: collection.mutable.ListBuffer[List[Int]], n: Int) : List[List[Int]] = {
            if(n == 0) return lst.toList
            val prevArr = lst.last
            lst += loop(prevArr, List(1))
            pascal(lst, n -1)
          }
          
          if(numRows == 0) return List()
          if(numRows == 1) return List(List(1))
          pascal(collection.mutable.ListBuffer[List[Int]](List(1)), numRows - 1)
          
        }
      }    
      
      //https://leetcode.com/problems/intersection-of-two-arrays-ii/
      /**
       * Consider duplicates
       * Input: nums1 = [1,2,2,1], nums2 = [2,2]
       * Output: [2,2]
       */
      object ArrayIntersect {
        
        
        def intersect(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
          if(nums1 == null || nums1.length == 0) return Array()
          if(nums2 == null || nums2.length == 0) return Array()
          val (lookup, traverse) = if(nums1.length >= nums2.length) (nums1, nums2) else (nums2, nums1)
          
          val lst = collection.mutable.ListBuffer[Int]()
          val lkupMap = collection.mutable.Map[Int, Int]() 
          lookup.map(_ -> 1).foreach {
            case(k, v) => if(lkupMap.contains(k)) lkupMap(k) += 1 
                          else lkupMap(k) = 1
          }
          
          traverse.foreach { num => 
            if(lkupMap.contains(num) && lkupMap(num) > 0) {
              lst.prepend(num)
              lkupMap(num) -= 1
            }
          }
          lst.toArray
        }
        
        //Given arrays are sorted 
        def intersectSorted(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
          if(nums1 == null || nums1.length == 0) return Array()
          if(nums2 == null || nums2.length == 0) return Array()
          
          val lst = collection.mutable.ListBuffer[Int]()
          
          var i = 0
          var j = 0
          
          while(i <  nums1.length && j < nums2.length) {
            if(nums1(i) < nums2(j)) i += 1
            else if(nums1(i) > nums2(j)) j += 1
            else {
              lst.prepend(nums1(i))
              i += 1
              j += 1
            }
          }
          lst.toArray
        }
  
      }
      
      /**
       * https://leetcode.com/problems/range-sum-query-immutable/
       * Given an integer array nums, find the sum of the elements between indices i and j (i ≤ j), inclusive.
       * Given nums = [-2, 0, 3, -5, 2, -1]
       * sumRange(0, 2) -> 1
       * sumRange(2, 5) -> -1
       */
      class NumArray(_nums: Array[Int]) {
        
        val presum = Array.fill(_nums.length + 1)(0)
        for(i <- 0 until _nums.length) {
          presum(i+1) = presum(i) + _nums(i)
        }
  
        def sumRange(i: Int, j: Int): Int = {
          presum(j+1) - presum(i) 
          
        }
      
      }
      
      /**
       * https://leetcode.com/problems/k-diff-pairs-in-an-array/
       * Input: [3, 1, 4, 1, 5], k = 2
       * Output: 2
       * Explanation: There are two 2-diff pairs in the array, (1, 3) and (3, 5).
       * Although we have two 1s in the input, we should only return the number of unique pairs.
       */
      object KdiffPairs {
        
        def findPairs(nums: Array[Int], k: Int) : Int = {
          
          if(k < 0) return 0
          val kmap = collection.mutable.Map[Int, Int]()
          for(n <- nums) {
            kmap.put(n, kmap.getOrElse(n, 0) + 1)
          }
          
          var sum = 0
          kmap.foreach{
            case(i, v) => if(k == 0) {
              if(v >= 2) sum += 1
            } else {
              if(kmap.contains(i + k)) sum += 1
            }
          }
          sum
        }
        
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
            else {
              sum += 1
              start += 1
            }
          }
          sum
        }
      }      
      
    }
    
    object StringProblems {
      
      /**
       * https://leetcode.com/problems/robot-return-to-origin/
       * Valid moves are R (right), L (left), U (up), and D (down). If the robot returns to the origin after
       * it finishes all of its moves, return true. Otherwise, return false.
       * Input: "UD"
       * Output: true 
       */
      def judgeCircle(moves: String): Boolean = {
        
        var updown = 0
        var leftright = 0
        
        for(ch <- moves) {
            ch match {
              case 'U'  => updown += 1
              case 'D'  => updown -= 1
              case 'L'  => leftright += 1
              case 'R'  => leftright -= 1
            }
        }
        
        updown == 0 && leftright == 0  
      }     
      
      /**
       * https://leetcode.com/problems/unique-email-addresses/
       * Input: ["test.email+alex@leetcode.com","test.e.mail+bob.cathy@leetcode.com","testemail+david@lee.tcode.com"]
       * Output: 2
       * Explanation: "testemail@leetcode.com" and "testemail@lee.tcode.com" actually receive mails
       */
      def numUniqueEmails(emails: Array[String]): Int = {
        
        val seen = collection.mutable.Set[String]()
        
        for(email <- emails) {
          val parts = email.split("@")
          var firstpart = parts(0).replaceAll("\\.", "")
          if(firstpart.contains("+")) firstpart = firstpart.substring(0, firstpart.indexOf("+"))
          seen.add(firstpart+"@"+parts(1))
        }
        seen.size
        
      }
      
      /**
       * https://leetcode.com/problems/reverse-string/
       * Do not allocate extra space for another array, you must do this by modifying the input 
       * array in-place with O(1) extra memory.
       */
      def reverseString(s: Array[Char]): Unit = {
        
        var i = 0
        var j = s.length - 1
        while(i < j) {
          val tmp = s(i)
          s(i) = s(j)
          s(j) = tmp
          i += 1
          j -= 1 
        }
      }
      
      def reverseStringRecurs(s: String): String = {
        if(s.isEmpty()) return s
        reverseStringRecurs(s.substring(1) + s.charAt(0))
      }
      
      /**
       * https://leetcode.com/problems/reverse-words-in-a-string-iii/
       * Input: "Let's take LeetCode contest"
       * Output: "s'teL ekat edoCteeL tsetnoc"
       */
      def reverseWords(s: String): String = {
        s.split(" ").map(_.reverse).mkString(" ")
      }
      def reverseWords2(s: String): String = {
        
        
        def split(s: String) : Array[String] = {
          
          val words = collection.mutable.ArrayBuffer[String]()
          val word = new StringBuilder()
          
          for(ch <- s) {
            if(ch != ' ') word.append(ch)
            else {
              words += word.toString()
              word.setLength(0)
            }
          }
          words += word.toString()  
          words.toArray
        }
        
        def reverse(s: String) : String = {
          val res = new StringBuilder()
          for(ch <- s) res.insert(0, ch)
          res.toString()
        }
        
        val words = split(s)
        val sb = new StringBuilder()
        
        for(word <- words) {
          sb.append(reverse(word)).append(" ")
        }
        sb.toString().trim()
      }
      //https://leetcode.com/problems/longest-common-prefix/
      //O(n.minLen) comparisons where minLenminLen is the length of the shortest string in the array.
      def longestCommonPrefixVerticle(strs: Array[String]): String = {
        
        if(strs == null | strs.size == 0) return ""
        
        for((ch, i) <- strs.head.view.zipWithIndex) {
          println(ch, i)
          for(str <- strs.tail) {
            //println(str, j)
            if(i == str.length() || str(i) != ch) {
              return strs.head.substring(0, i)
            }
          }
        }
        strs.head
      }
      
      def longestCommonPrefixHorizontal(strs: Array[String]): String = {
        
        if(strs.size == 0 ) return ""
        
        var prefix = strs(0)
        
        for((str, i) <- strs.tail.view.zipWithIndex) {
          while(str.indexOf(prefix) != 0) { //not found
            prefix = prefix.substring(0, prefix.length() - 1)
            if (prefix.isEmpty()) return ""
          }
        }
        prefix
      }
      
      def longestCommonPrefixHorizontal2(strs: Array[String]): String = {
        
        if(strs == null || strs.size == 0 || strs.isEmpty || (strs.size == 1 && "".equals(strs.head))) return ""
        
        var prefix = strs(0)
        var i  = 1
        
        while( i < strs.length) {
          while(strs(i).indexOf(prefix) != 0) {
            prefix = prefix.substring(0, prefix.length() - 1)
          }
          i += 1
        }
        prefix
      }
      
      def longestCommonPrefixScala(strs: Array[String]): String = {
        (strs.min.view,strs.max.view).zipped.takeWhile(v => v._1 == v._2).unzip._1.mkString
      }
      
      
      def longestCommonPrefixScala2(strs: Array[String]): String = {
        @tailrec
        def commonPrefix(str: String, prefix: String):String = {
          if (prefix.isEmpty) ""
          else if(str.startsWith(prefix)) prefix
          else commonPrefix(str, prefix.substring(0, prefix.length - 1))
        }
        
        if(strs.isEmpty) ""
        else strs.foldLeft(strs.head)((prefix, str) => commonPrefix(str, prefix))
      }
          
  
      
      def longestCommonPrefixDnC(strs: Array[String]): String = {
        
        if(strs.size == 0 || strs.isEmpty || (strs.size == 1 && "".equals(strs.head))) return ""
        
        def longestCommonPrefix(strs: Array[String], l: Int, r: Int) : String = {
         
          if(l == r) {
            return strs(l)
          } 
          
          val m = (l + r) / 2
          val lcpLeft = longestCommonPrefix(strs, l, m)
          val lcpRight = longestCommonPrefix(strs, m + 1, r)
          
          
          def commonPrefix(lcpLeft: String, lcpRight: String) : String = {
            
            var prefix = lcpLeft
            var prefixxLen = prefix.length()
            for(i <- 0 until lcpRight.length()) {
              if(prefixxLen == i || lcpRight(i) != prefix(i)) {
                return prefix.substring(0, i)
              }
            }
            lcpRight
          }
          
          commonPrefix(lcpLeft, lcpRight)
        }
        
        longestCommonPrefix(strs, 0, strs.length - 1)
      }
      
      def longestCommonPrefixBinarySrch(strs: Array[String]) : String = {
        
        if(strs == null || strs.length == 0 || (strs.size == 1 && "".equals(strs.head))) return ""
        
        var minLen = strs.foldLeft(Int.MaxValue)((min, str) => Math.min(min, str.length()))
        
        var low = 0
        var high = minLen
        while(low <= high) {
          val mid = (low + high) / 2
          if(isCommonPrefix(strs, mid)) {
            low = mid + 1
          } else {
            high = mid - 1
          }
        }
        
        def isCommonPrefix(strs: Array[String], len: Int): Boolean  = {
          val prefix = strs(0).substring(0, len)
          for(str <- strs) {
            if(!str.startsWith(prefix)) return false
          }
          true
        }
        
        strs(0).substring(0, (low + high) / 2)
      }
      
      //https://leetcode.com/problems/roman-to-integer/
      object RomanToInt {
        
        def romanToInt(s: String): Int = {
        
          if(s == null || s.length() == 0) return 0
          var i = 0
          var res = 0
          //for (i <- s.length -1 to 0 by -1)
          while(i < s.length()) {
            var ch = s(i)
            var chn = if(i+1 != s.length()) s(i+1) else '0'
            ch match {
              case 'I' => chn match {
                case 'V' => res = res + 4; i += 2
                case 'X' => res = res + 9; i += 2
                case _ => res += 1; i += 1
              }
              case 'V' => res += 5; i += 1
              case 'X' => chn match {
                case 'L' => res = res + 40; i += 2
                case 'C' => res = res + 90; i += 2
                case _ => res += 10; i += 1
              }
              case 'L' => res += 50; i += 1
              case 'C' => chn match {
                case 'D' => res = res + 400; i += 2
                case 'M' => res = res + 900; i += 2
                case _ => res += 100; i += 1
              }
              case 'D' => res += 500; i += 1
              case 'M' => res += 1000; i += 1
            }
          }
          res
        }
  
        
        val dict: Map[Char, Int] = Map(        
          'I' -> 1,
          'V' -> 5,
          'X' -> 10,
          'L' -> 50,
          'C' -> 100,
          'D' -> 500,
          'M' -> 1000
        )
        
        def romanToIntFwd(s: String) : Int = {
          
          var res = 0
          val len = s.length()
          for(i <- 0 until len - 1) {
            
            if(dict(s(i)) < dict(s(i + 1))){
              res -= dict(s(i))
            } else {
              res += dict(s(i))
            }
          }
          res + dict(s(len - 1))
        }
        
        def romanToIntFwd2(s: String) : Int = {
          
          var res = 0
          val len = s.length()
          for(i <- 1 until len) {
            
            if(dict(s(i)) <= dict(s(i - 1))){
              res += dict(s(i-1))
            } else {
              res -= dict(s(i-1))
            }
          }
          res + dict(s(len - 1))
        }
        
        def romanToIntBack(s: String) : Int = {
          
          val len = s.length()
          var res = dict(s(len -1))
          
          for(i <- len - 2 to 0 by -1) {
            if(dict(s(i)) < dict(s(i+1))) res -= dict(s(i))
            else res += dict(s(i))
          }
          res
          
        }
        
        /*def romanToIntScala(s: String) : Int = s.toSeq match {
          case Seq(a, b, t@_*) if dict(a) < dict(b) => dict(b) - dict(a) + romanToIntScala(t)
          case Seq(a, t@_*) => dict(a) + romanToIntScala(t)
          case _ => 0
        }*/
        
        def romanToIntFun1(s: String) : Int = {
          s.foldLeft((0, 0))((t, cur) => { 
            (t._1 + dict(cur) + (if(t._2 < dict(cur)) - 2 * t._2 else 0), dict(cur))
          })._1
        }
        
        def romanToIntFun2(s: String) : Int = {
          
          s.foldRight(0, 0)((cur, t) => {
            
            if(dict(cur) < t._2) ((t._1 - dict(cur)), dict(cur))
            else ((t._1 + dict(cur)), dict(cur))
          })._1
        }
              
      }
      
      //https://leetcode.com/problems/valid-parentheses/
      object ValidParen {
        
        val brk = Map('(' -> ')','{' -> '}','[' -> ']')
        
        case class Node(ch: Char, var next: Node = null)
        
        def isValidParnStr(s: String) :  Boolean = {
          
          /*if(s == null) return false
          if("".equals(s)) return true
          if(!brk.contains(s(0))) return false*/
          
          if(s.length() %2 == 1 ) return false
          
          var nodeList:  Node = Node(s(0))
          var head: Node = nodeList
          
          for(i <- 1 until s.length()) {
            val ch = s(i) 
            if(brk.contains(ch)) {
              val nn = Node(ch)
              nn.next = head
              head = nn
            }
            else if(head != null && ch == brk(head.ch)) {
              head = head.next
            }
            else return false
          }
          head == null
        }
        
        def isValidParnStck(s: String) : Boolean = {
          
          if(s.length() %2 == 1 ) return false
          
          val stk = collection.mutable.Stack[Char]()
          
          for(ch <- s) {
            if(ch == '('){
              stk.push(')')            
            } else if(ch == '{'){
              stk.push('}')
            } else if(ch == '['){
              stk.push(']')
            } else if(stk.isEmpty || stk.pop() != ch) return false
          }
          stk.isEmpty
        }
        
        def isValidParnFun(s : String) : Boolean = {
          
          if(s.length() % 2 == 1) return false
          
          s.foldLeft(collection.mutable.Stack[Char]())((stk, ch) => {
            
            if(ch == '('){
              stk.push(')')            
            } else if(ch == '{'){
              stk.push('}')
            } else if(ch == '['){
              stk.push(']')
            } else if(stk.isEmpty || stk.pop() != ch) return false
            stk
          }).isEmpty
        }
        
      }
      
      
      /**
       * https://leetcode.com/problems/letter-case-permutation/
       * Given a string S, we can transform every letter individually to be lowercase or uppercase to create 
       * another string.  Return a list of all possible strings we could create.
       * 
       * Input: S = "a1b2"
       * Output: ["a1b2", "a1B2", "A1b2", "A1B2"]
       * 
       * Input: S = "12345"
       * Output: ["12345"]
       */
      object LetterCasePermutation {
        
        //Recursive or DFS soln
        def letterCasePermutation(S: String): List[String] = {
          
            val ostr = S.toLowerCase()
            val res = collection.mutable.ArrayBuffer[String]()
      
            
            def permute(str: String, idx: Int) : Unit = {
      
              if(idx == ostr.length()) {
                res += str
                return  
              }
              val ch = ostr(idx)
      
              if(ch.isLetter) permute(str + ch.toUpper, idx + 1)
      
              permute(str + ch, idx + 1)
      
            }
            permute("", 0)
            res.toList
        }
        
        //BFS
        def letterCasePermutationBFS(S: String): List[String] = {
          
          if(S == null || S.isEmpty()) return List[String]() 
          
          val que = collection.mutable.Queue[String]()
          que.enqueue(S)
          
          for(i <- 0 until S.length()) {
            
            val ch = S(i)
            
            if(!ch.isDigit) {
              var size = que.size
              
              while(size > 0){
                
                val str = que.dequeue().toCharArray()
                str(i) = str(i).toUpper
                que.enqueue(String.valueOf(str))
                
                str(i) = str(i).toLower
                que.enqueue(String.valueOf(str))
                
                size -= 1
              }
            }
          }
          que.toList
          
        }
        
      }
      
      //https://leetcode.com/problems/implement-strstr/
      //n^2
      def strStrExp(haystack: String, needle: String): Int = {
        
        if(needle.length() == 0) return 0
        if(haystack.length() == 0 || haystack.length() < needle.length()) return -1
        
        for(i <- 0 to haystack.length() - needle.length()){ //no point in checking in haystack after they surpass the difference of length
          var j = 0
          while(j < needle.length() && needle(j) == haystack(i + j)) {
            if(j == needle.length() - 1) return i
            j += 1
          }
        }
        -1
      }
      /**
       * "mississippi"
       * "issipi"
       */
      def strStr(haystack: String, needle: String): Int = {
      
        if(needle.length() == 0) return 0
        if(haystack.length() == 0 || haystack.length() < needle.length()) return -1
        
        var i = 0
        var j = 0
        var secondIndexOfFirstChar = -1
        
        while(j < haystack.length() - needle.length()){
          
          while(i <= needle.length() && j < haystack.length() && haystack(j) == needle(i)) {
            if(secondIndexOfFirstChar == 0 && haystack(j) == needle(0)) secondIndexOfFirstChar = j
            if(secondIndexOfFirstChar == -1) secondIndexOfFirstChar = 0
            j += 1
            i += 1
          } 
          if(i > 0) {
            if(i == needle.length()) return j - i
            else {
              i = 0
              j = if(secondIndexOfFirstChar != 0) secondIndexOfFirstChar else j
            }
          } else {
            j += 1          
          }
        }
        -1
      }
      def strStrFun(haystack: String, needle: String): Int = {
        
        @annotation.tailrec
        def find(haystack: String, needle: String, n: Int) : Int = {
          if(n > haystack.length() - needle.length()) -1
          else if(haystack.substring(n, n + needle.length()) == needle) n
          else find(haystack, needle, n + 1)
        }
        if(needle.length() == 0) return 0
        if(haystack.length() == 0 || haystack.length() < needle.length()) return -1
        find(haystack, needle, 0)
      }
      
      //https://leetcode.com/problems/count-and-say/
      object CountAndSay {
        
        def countAndSay(n: Int): String = {
          if(n < 1) return "0"
          if(n == 1) return "1"
          var str = "1"
          for(i <- 0 until n - 1) {
            val sb = new StringBuilder()
            var cnt = 1
            for(j <- 0 until str.length()){
              if(j+1 < str.length() && str(j+1) == str(j)) cnt += 1
              else {
                sb.append(cnt).append(str(j))
                cnt = 1
              }
            }
            str = sb.toString()
          }
          str
        }
        
        //foldleft
        def countAndSayFun1(n: Int): String = {
          if(n < 1) return "0"
          if(n == 1) return "1"
          var str = "1"
          for(i <- 0 until n - 1) {
            val sb = new StringBuilder()
            var cnt = 1
            val res = str.tail.foldLeft((str.head, new StringBuilder(), cnt))((t, ch) => {
              if(ch == t._1) (ch, t._2, t._3 + 1) 
              else {
                (ch, t._2.append(t._3).append(t._1), 1)
              }
            })
            str = res._2.toString() + res._3 + res._1
          }
          str
        }
        
        //recursive
        def countAndSayFun2(n: Int): String = {
          
          if(n < 1) return "0"
          if(n == 1) return "1"
          
          @annotation.tailrec
          def countAndSay(prev: String, n: Int) : String = {
            if(n == 0) return prev
            else //countAndSay(updateCount2(prev.toCharArray(), new StringBuilder(), 1), n -1 )
              countAndSay(updateCount(prev, 0, new StringBuilder(), 1), n -1 )
            
          }
          
          @annotation.tailrec
          def updateCount2(str: Seq[Char], sb: StringBuilder, cnt: Int) : String = {
              str match {
                case Seq(head, t@_*) => if(t.nonEmpty && t.head == head) {
                  updateCount2(t, sb, cnt + 1) 
                } else {
                  updateCount2(t, sb.append(cnt).append(head), 1)
                }
                case _ =>  sb.toString()
              }
          }
          
          
          
          @annotation.tailrec
          def updateCount(str: String, idx: Int,  sb: StringBuilder, cnt: Int) : String = {
            if(idx == str.length() - 1)  {
              return sb.toString() + cnt + str(idx)
            }
            if(str(idx + 1) == str(idx)) updateCount(str, idx + 1, sb, cnt + 1) else {
              updateCount(str, idx + 1, sb.append(cnt).append(str(idx)), 1)
            }
          }
          
          countAndSay("1", n-1)
          
        }
      }
      
    }
    
    object DPProblems {
      
      //https://leetcode.com/problems/maximum-subarray/
      /**
       * Given an integer array nums, find the contiguous subarray (containing at least one number) which has 
       * the largest sum and return its sum.
       * 
       * Input: [-2,1,-3,4,-1,2,1,-5,4],
       * Output: 6 Explanation: [4,-1,2,1] has the largest sum = 6.
       */
      object MaxSubArray {
        
        def maxSubArray(nums: Array[Int]): Int = {
          
          if(nums.isEmpty) return 0
          if(nums.size == 1) return nums(0)
          
          var maxSoFar = nums(0)
          var maxEndinghere = nums(0)
          
          for(i <- 1 until nums.length) {
            maxEndinghere = Math.max(maxEndinghere + nums(i), nums(i))
            maxSoFar = Math.max(maxSoFar, maxEndinghere)
          }
          maxSoFar
        }
        
        def maxSubArray2(nums: Array[Int]): Int = {
          
          if(nums.isEmpty) return 0
          if(nums.size == 1) return nums(0)
          
          var maxSoFar = Int.MinValue
          var sum = 0
          
          for(i <- 0 until nums.length) {
            if(sum < 0) {
              sum = nums(i)
            } else sum += nums(i)
            maxSoFar = Math.max(maxSoFar, sum)
          }
          maxSoFar
        }
        
        
        def maxSubArrayFun1(nums: Array[Int]): Int = {
          
          if(nums.isEmpty) return 0
          if(nums.size == 1) return nums(0)
          
          nums.foldLeft((0,Int.MinValue))((t, num) => {
            val sum = if(t._1 < 0) num else t._1 + num
            val max = Math.max(t._2, sum)
            (sum, max)
          })._2
          
        }
        
        //Scala DP
        def maxSubArrayFun2(nums: Array[Int]): Int = {
          nums.tail.scan(nums.head){(s,e) => math.max(s+e, e)}.max
        }
        
      }
      
      
      //https://leetcode.com/problems/best-time-to-buy-and-sell-stock/    
      /**
       * Input: [7,1,5,3,6,4]
       * Output: 5
       * Explanation: Buy on day 2 (price = 1) and sell on day 5 (price = 6), profit = 6-1 = 5.
       * Not 7-1 = 6, as selling price needs to be larger than buying price.
       */
      object SellStock1 {
        
        //this is Not a DP soln
        def maxProfit(prices: Array[Int]): Int = {
          
          if(prices.length <= 1) return 0
          var minBuy = prices(0)
          var profit = 0
          for(i <- 1 until prices.length) {
            if(prices(i) < minBuy) {
              minBuy = prices(i)
            }
            else profit = Math.max(profit, prices(i) - minBuy)
          }
          profit
        }      
        
        def maxProfitFun1(prices: Array[Int]): Int = {
          
          prices.foldLeft((Int.MaxValue,0)) { case((minBuy, maxProfit), cur) => {
            val mb = Math.min(minBuy, cur)
            (mb, Math.max(maxProfit, cur - mb))
            }
          }._2  
        }
        
        //DP soln
        def maxProfitFun2(prices: Array[Int]): Int = {
          if(prices.length <= 1) return 0
          prices.zip(prices.scan(Int.MaxValue)(math.min).tail).map(p => p._1 - p._2).max
        }
      }
  
      //https://leetcode.com/problems/house-robber/
      //https://leetcode.com/problems/house-robber/discuss/156523/From-good-to-great.-How-to-approach-most-of-DP-problems.
      /**
       * Given a list of non-negative integers representing the amount of money of each house,
       *  determine the maximum amount of money you can rob tonight
       *  You cannot rob adjacent house
       *  Input: [1,3,1,1,7]
       *  Output: 10
       */
      object RobHouse {
        
        def rob(nums: Array[Int]): Int = {
          
          def loop(idx: Int) : Int = {
            if(idx < 0) return 0
            Math.max(loop(idx-2) + nums(idx), loop(idx-1))
          }
          if(nums == null) return 0 
          return loop(nums.length - 1)
        }
        
        def rob2(nums: Array[Int]): Int = {
          
          var memo = Array.fill(nums.length){-1}
          
          def loop(idx: Int) : Int = {
            if(idx < 0) return 0
            if(memo(idx) >= 0) return memo(idx)
            memo(idx) = Math.max(loop(idx-2) + nums(idx), loop(idx-1))
            memo(idx)
          }
          if(nums == null) return 0 
          return loop(nums.length - 1)
        }
        
        def rob3(nums: Array[Int]) : Int = {
          
          if(nums == null) return 0 
          var notRob = 0
          var rob = 0
          for(n <- nums) {
            val tmp = notRob
            notRob = Math.max(notRob, rob)
            rob = n + tmp
          }
          Math.max(notRob, rob)
        }
        
        def rob4(nums: Array[Int]) : Int = {
          
          if(nums == null) return 0 
          val dp = Array.fill(nums.length + 1, 2){0}
          for(i <- 1 to nums.length) {
            dp(i)(0) = Math.max(dp(i-1)(0), dp(i-1)(1))
            dp(i)(1) = dp(i-1)(0) + nums(i-1)
          }
          Math.max(dp(nums.length)(0), dp(nums.length)(1))
        }
        
        def rob5(nums: Array[Int]) : Int = {
          
          if(nums == null) return 0 
          var robEven = 0
          var robOdd = 0
          for(i <- 0 until nums.length) {
            
            if(i % 2 == 0) robEven = Math.max(robEven + nums(i), robOdd)
            else robOdd = Math.max(robOdd + nums(i), robEven)
          }
          Math.max(robEven,robOdd)
          
        }
      }
      
      //https://leetcode.com/problems/min-cost-climbing-stairs/
      /**
       * Each step has cost
       * Once you pay the cost, you can either climb one or two steps. You need to find minimum cost 
       * to reach the top of the floor, and you can either start from the step with index 0, or the step with index 1
       * Input: cost = [10, 15, 20]
       * Output: 15
       * 
       * Input: cost = [1, 100, 1, 1, 1, 100, 1, 1, 100, 1]
       * Output: 6
       */
      object MinCostClimbingStairs {
        
        //Recursive: Time limit exceeds
        def minCostClimbingStairsRecr(cost: Array[Int]) : Int = {
          
          def loop(n : Int) : Int = {
            if(n == 2) return Math.min(cost(0), cost(1))
            if(n < 2) return 0
            Math.min(loop(n-1) + cost(n-1), loop(n-2) + cost(n-2))
          }
          
          loop(cost.length)
        }
        
        //DP - Memoization
        def minCostClimbingStairsDP(cost: Array[Int]) : Int = {
        
          val memo = Array.fill(cost.length + 1){-1}
          
          def loop(n : Int) : Int = {
            if(n == 2) return Math.min(cost(0), cost(1))
            if(n < 2) return 0
            if(memo(n) > -1) return memo(n)
            memo(n) = Math.min(loop(n-1) + cost(n-1), loop(n-2) + cost(n-2))
            memo(n)
          }
          loop(cost.length)
        }      
        
        def minCostClimbingStairs(cost: Array[Int]) : Int = {
          
          var f1 = cost(0)
          var f2 = cost(1)
          
          for(i <- 2 until cost.length) {
            val tmp = cost(i) + Math.min(f1, f2)
            f1  = f2
            f2 = tmp
          }
          
          Math.min(f1,f2)
        }
        
  
        
  
        
      }
      
      //https://leetcode.com/problems/climbing-stairs/solution/
      /**
       * n steps
       * you can either climb 1 or 2 steps. In how many distinct ways can you climb to the top?
       */
      object ClimbStairs {
        
        //Rec and memo; forward
        def climbStairsRec(n: Int): Int = {
          
          val memo = Array.fill(n + 1){-1}
          
          def loop(i: Int) : Int = {
            if(i > n) return 0
            if(i == n) return 1
            if(memo(i) > -1) return memo(i)
            memo(i) = loop(i+1) + loop(i+2)
            memo(i)
          }
          loop(0)
        }
        
       //Rec and memo; backward 
       def climbStairsRec2(n: Int): Int = {
          
          val memo = Array.fill(n + 1){-1}
          
          def loop(i: Int) : Int = {
            if(i < 0) return 0
            if(i == 0) return 1
            if(memo(i) > -1) return memo(i)
            memo(i) = loop(i-1) + loop(i-2)
            memo(i)
          }
          loop(n)
        }
       
       //DP - O(n) time O(1) space
       /**
        * explanation: draw a tree; you can see 3 can be reach via 1 and 2 times via 2 so total 3. 
        * 4 can be reached 3 times via 3 and 2 times via 2 so total 5 and so on..
        */
       def climbStairsDP(n: Int): Int = {
          
          var a = 1
          var b = 2
          
          for(i <- 3 to n) {
            val tmp = a + b 
            a = b
            b = tmp
          }
          b
        }
       
       //DP recursrive - TailRec
       def climbStairsDPRecursive(n: Int): Int = {
         
         @annotation.tailrec
         def loop(i: Int, a: Int = 0, b: Int = 1): Int = {
           if(i == 0 )  b
           else loop(i - 1, b, a + b)
         }
         loop(n)
       }
      }
      
    }
    

    

    object TreeProblems {
      
      class TreeNode(var _value: Int) {
       var value: Int = _value
       var left: TreeNode = null
       var right: TreeNode = null
      }
      
          /**
     * https://leetcode.com/problems/two-sum-iv-input-is-a-bst/solution/
     */
    def findTargetSumInBST(root: TreeNode, k: Int): Boolean = {
      
      var complimentSet = collection.mutable.HashSet[Int]()      
      def traverseTree(node: TreeNode) : Boolean = {
        if(node == null) return false        
        if(complimentSet.contains(k - node._value)) {
          return true          
        } 
        complimentSet.add(node._value)
        return traverseTree(node.left) || traverseTree(node.right)  
      }
      traverseTree(root)  
    }
    
    //https://leetcode.com/problems/convert-sorted-array-to-binary-search-tree/
    object SortedArrayToBBST {
      
      def sortedArrayToBST(nums: Array[Int]): TreeNode = {
          
        def loop(l: Int, h: Int): TreeNode = {
           
          if(l > h) return null
          
          val m = (l + h)/ 2
          
          var node = new TreeNode(nums(m))
          node.left = loop(l, m-1)
          node.right = loop(m+1, h)
          node
        }
        if(nums == null) return null 
        
        loop(0, nums.length - 1)
      }
      
    }
      
      /**
       * https://leetcode.com/problems/symmetric-tree/
       * Given a binary tree, check whether it is a mirror of itself (ie, symmetric around its center).
       * For example, this binary tree [1,2,2,3,4,4,3] is symmetric:
       */
      object SymmetricTree {
        
        def isSymmetric(root: TreeNode): Boolean = {
          if(root == null) return true
          
          def isMirror(left: TreeNode, right: TreeNode) : Boolean = {
            if(left == null && right == null) return true
            if(left == null || right == null) return false
            left._value == right.value && isMirror(left.right, right.left) && isMirror(left.left, right.right)
          }
          isMirror(root.left, root.right)
        }
        
        def isSymmetricItr(root: TreeNode): Boolean = {
          
          if(root == null) return true
          val que = collection.mutable.Queue[TreeNode]()
          que.enqueue(root.left)
          que.enqueue(root.right)
          while(que.nonEmpty) {
            val t1 = que.dequeue()
            val t2 = que.dequeue()
            if (t1 == null && t2 == null){
            }else {
              if (t1 == null || t2 == null) return false
              if(t1._value != t2._value) return false
              que.enqueue(t1.left)
              que.enqueue(t2.right)
              que.enqueue(t1.right)
              que.enqueue(t2.left)
            }
          }
          true
          
        }
        
      }
      
      /**
       * https://leetcode.com/problems/maximum-depth-of-binary-tree/
       * Given a binary tree, find its maximum depth.
       */
      object TreeMaxDepth {
        
        //recursive DFS
        def maxDepth(root: TreeNode): Int = {
        
          if(root == null) return 0
          def loop(node: TreeNode) : Int = {
            if(node == null) return 0
            1 + Math.max(loop(node.left), loop(node.right))
          }
          loop(root)  
        }
        
        def maxDepthDFS(root: TreeNode): Int = {
          
          if(root == null) return 0
          val stack = collection.mutable.Stack[TreeNode]()
          val values = collection.mutable.Stack[Int]()
          stack.push(root)
          values.push(1)
          var max = 0
          
          while(stack.nonEmpty) {
            val elem = stack.pop()
            val tmp = values.pop()
            max = Math.max(max, tmp)
            if(elem.left != null) {
              stack.push(elem.left)
              values.push(tmp + 1)
            }
            if(elem.right != null) {
              stack.push(elem.right)
              values.push(tmp + 1)
            }
          }
          max
        }
        
        def maxDepthBFS(root: TreeNode): Int = {
          
          if(root == null) return 0
          val que = collection.mutable.Queue[TreeNode]()
          que.enqueue(root)
          var max = 0
          
          while(que.nonEmpty) {
            var size = que.size
            while(size > 0){
              val ele = que.dequeue()
              if(ele.left != null) que.enqueue(ele.left)
              if(ele.right != null) que.enqueue(ele.right)
              size -= 1
            }
            max += 1
          }
          max        
        }
        
      }      
      
      
    }
    


    
    def main(args: Array[String]) {
      
      println("Inside main")
      //callTwoSum()
      println(('A' + (28 % 26)).toChar)
      println(300 % 26)
      println(300/26)
      println(convertToTitle(28))
      
      val intmax = Int.MaxValue //2147483647
      println(Int.MaxValue.toString().reverse)
      val int = 746384741 * 10 + 2
      println(int)
      println((int - 2) / 10)
      println(reverseInt(2147483647))
      println(isPalindrome(123321))
      println(isPalindrome(12321))
      println(isPalindrome(-12321))
      
      val ll1 = new ListNodeList()
      List(2,4,6,9).map(ll1.insert(_))
      ll1.printLists()
      
      val ll2 = new ListNodeList()
      List(5,6,10,11).map(ll2.insert(_)) //5,6,10,11
      ll2.printLists()
      
      val ll3 = new ListNodeList()
      List(6,9).map(ll3.insert(_)) //6,9
      ll3.printLists()
      
      val head1 = ll1.head
      
      //val mergeL = mergeTwoSortedLists(ll1.head, ll2.head)
      val mergeL = LinkedList.mergeTwoSortedListsInPlace(ll3.head, ll2.head)
      mergeL.printLists()
      
      val lcpArr = Array("flo","flower","flight")
      
      val lmv = lcpArr.min.toCharArray()
      val lxv = lcpArr.max.toCharArray()
      val zipped = (lmv, lxv).zipped
      val filtered = zipped.takeWhile(v => v._1 == v._2)
      val lcpt = filtered.unzip
      println(lcpt._1.mkString)
      
      
      println(StringProblems.ValidParen.isValidParnStck("()"))
      println(StringProblems.ValidParen.isValidParnStck("{()[]}"))
      
      val t1 = System.currentTimeMillis()
      //println(strStr("mississippi","issip"))
      println("total time: " + (System.currentTimeMillis() - t1))
      val t2 = System.currentTimeMillis()
      println(StringProblems.strStrExp("mississippi","issip"))
      println("total time: " + (System.currentTimeMillis() - t2))
      println("CountAndSay:" + StringProblems.CountAndSay.countAndSayFun2(4))
      
      val prices = Array(7,1,5,3,6,4)
      println(prices.scan(Int.MaxValue)(math.min).mkString(","))
      println(prices.zip(prices.scan(Int.MaxValue)(math.min).tail).mkString(","))
      println(prices.zip(prices.scan(Int.MaxValue)(math.min).tail).map(p => p._1 - p._2).mkString(","))
      println(prices.zip(prices.scan(Int.MaxValue)(math.min).tail).map(p => p._1 - p._2).foldLeft(0)(math.max))
      
      
      println(DPProblems.RobHouse.rob(Array(2,1,1,7,5,2)))
      
      
      
      val ll = new ListNodeList()
      List(-1,2,3,4,5,3,2,-1).map(ll.insert(_))
      ll.printLists()
      println(isPalindrome(1221))
      println(isPalindromeReverseHalf(ll.head))
      
      println(-123 % 10)
      
      println(DPProblems.MinCostClimbingStairs.minCostClimbingStairs(Array(0,1,2,2)))
      
      println(DPProblems.ClimbStairs.climbStairsDPRecursive(5))
      
      println(StringProblems.reverseWords2("Let's take LeetCode contest"))
    }
}