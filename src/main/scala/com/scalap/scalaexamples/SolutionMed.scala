package com.scalap.scalaexamples

import scala.collection.mutable.ListBuffer


object SolutionMed {
  
//https://www.codechef.com/wiki/tutorial-dynamic-programming
/**
 * Allowed operations
 * n-1
 * if(n%2 == 0) n/2
 * if(n%3 == 0) n/3
 */
object MinStepsTo1 {
  
  def getToTheOneRecMemo(n: Int) = {
    
    0
  }
  
  def getToTheOneItr(n: Int) = {
    
  }
}



object ArrayProblems {
  
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
  //O(n^k-1) + nlog(n) (here k = 4)
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
          val m = l  + (h -l)/2
          val realM = (m +l) % nums.length
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
        l
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

  
}


object StringProblems {
  
  //https://www.codechef.com/wiki/tutorial-dynamic-programming
  object LongestIncreasingSubSequence {
  
  }
  //Longest non repeating char substring without repeating char
  //https://leetcode.com/problems/longest-substring-without-repeating-characters/
  /**
   * Input: "abcabcbb"
   * Output: 3
   * 
   * Time complexity : O(2n) = O(n)O(2n)=O(n)
   * Space complexity : O(min(m, n))O(min(m,n))
   */
  object LengthOfLongestSubstring {
    
    //Sliding window based on HashSet
    def lengthOfLongestSubstring(s: String) : Int = {
      
      var i, j = 0
      val set = collection.mutable.HashSet[Char]()
      var max = 0
      while(j < s.length()) {
        var ch = s(j)
        if(!set(ch)) { //test if element is in set or not
          j += 1
          set.add(ch)
          max = Math.max(max, j - i)
        } else {
          set.remove(s.charAt(i))
          i += 1
        }
      }
      max
    }
    
    //Sliding window based on Map
    def lengthOfLongestSubstring2(s: String) : Int = {
      var i, j = 0
      var max = 0
      val map = collection.mutable.Map[Char, Int]()
      
      while(j < s.length()) {
        var ch = s(j)
        if(map.contains(ch)){
          i = Math.max(i, map(ch))
        }
        max = Math.max(max, j - i + 1)
        map.put(ch, j + 1)
        j +=  1
      }
      max
    }
    
    //tail recrusive; just for hack of it but i won't use this soln
    def lengthOfLongestSubstring3(s: String) : Int = {
      
      val map = collection.mutable.Map[Char, Int]()
      
      @annotation.tailrec
      def loop(i: Int, j: Int, max: Int): Int = {
        
        if(j == s.length()) return max
        
        var ch = s(j)
        
        var newi = i
        if(map.contains(ch)) {
          newi = Math.max(i, map(ch))
        } 
        val newmax = Math.max(max, j - newi + 1)
        map.put(ch, j + 1)
        loop(newi, j + 1, newmax)
      }
      loop(0, 0, 0)
    }
    
  }
  
  //https://leetcode.com/problems/longest-palindromic-substring/solution/
  //for O(n) soln - manacher's algorithm
  /**
   * abcdcbghhgbcf
   * soln - cbghhgbc
   */
  object LongestPalindrome {
    
    
    //Expand Around Center
    def longestPalindrome(s: String): String = {
      
      if(s == null || s.length() < 1) return ""
      var start, end = 0
      var maxlen = 0
      
      for(i <- 0 until s.length()) {
        
        val len1 = expandAroundCenter(s, i, i)
        val len2 = expandAroundCenter(s, i, i + 1)
        val len = Math.max(len1, len2)
        val prevlen = end - start
        if(len > prevlen) {
          start = i - (len - 1) / 2
          end = i + len/2
        }
      }
      
      def expandAroundCenter(s: String, left: Int, right: Int) : Int = {
        
        var (l, r) = (left, right)
        while(l >= 0 && r < s.length() && s(l) == s(r)) {
          l -= 1
          r += 1
        }
        r - l - 1
      }
      
      s.substring(start, end + 1)
    }
  
  }
  
  //https://leetcode.com/problems/string-to-integer-atoi/
  object Atoi {
    
      def myAtoi(str: String): Int = {
  
        if(str == null || str.length() < 1) return 0
        
        var res: Int = 0
        var sign = 1
        var founddigit = false
        
        val len = str.length()
        
        for(i <- 0 until len) {
          
          val ch = str(i)
          ch match {
            case ' '  => if(res != 0 || founddigit) return res * sign
            case '-' => if(i+1 == len || !str(i+1).isDigit) {
             return res * sign
            } else sign = -1
            case '+' => if(i+1 == len || !str(i+1).isDigit) {
             return res * sign
            }
            case x if(x.isLetter) => return res * sign
            case x if(x.isDigit) => {
              founddigit = true
              //max 2147483647
              //min -2147483648
              if(res > Int.MaxValue/10 && sign == 1 || (res == Integer.MAX_VALUE / 10 && x > 7)) {
                return if(sign == 1) Int.MaxValue else Int.MinValue
              } else {
                res = (res * 10) + Character.getNumericValue(x)              
              }
            }
            case _ => return res
          }
        }
        res  * sign
      }
    
  }  
  
}








object LinkedList {
  
    //https://leetcode.com/problems/remove-nth-node-from-end-of-list/solution/
    def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
      
      if(n == 0 || head == null) return head
      val dummy = new ListNode(0)
      dummy.next = head
      var first = dummy
      var second = dummy
    
      // Advances first pointer so that the gap between first and second is n nodes apart
      for(i <- 0 to n){
        first = first.next
      }
      // Move first to the end, maintaining the gap
      while(first != null) {
        first = first.next
        second = second.next
      }
      second.next = second.next.next
      dummy.next
    }
    
    def removeNthFromEnd2(head: ListNode, n: Int): ListNode = {
      
      if(n <= 0 || head == null) return head
      var size = 1
      var cur, prev = head
      while(cur.next != null) {
        size += 1
        cur = cur.next
        if(size > n + 1) prev = prev.next
      }
      if(size == n) return head.next //first node
      else {
        prev.next = prev.next.next
        head
      }
    }
    
    
    def mergeTwoSortedLists(l1: ListNode, l2: ListNode): ListNode = {
      
      var newListHead: ListNode = new ListNode()
      var cur: ListNode = newListHead
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
      newListHead.next
    }
    
    def mergeTwoSortedListsInPlace(l1: ListNode, l2: ListNode): ListNode = {
      
      var c1 = l1
      var c2 = l2
      
      if(l1 == null) return l2
      if(l2 == null) return l1
      
      var dummy = new ListNode()
      var p1 = dummy
      p1.next = c1
      
      while(c1 != null && c2 != null) {
        if(c1.x <= c2.x) {
          p1=c1
          c1 = c1.next
        } else {
          val tmp = c2.next
          p1.next = c2
          c2.next = c1
          p1 = c2
          c2 = tmp
        } 
      }
      if(c2 != null) {
        p1.next = c2 
      } 
      dummy.next
    }
    
    def isPalindromeReverseHalf(head: ListNode): Boolean = {
      
      if(head == null ) return true
      //find the lenght; reverse first half and compare it to second
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
      //for odd number of nodes
      if(len % 2 != 0) cur = cur.next
      //compare cur and prev which is reversed now
      while(cur != null) {
        if(cur._x != prev._x) return false
        cur = cur.next
        prev = prev.next
      }
      true
      
    }
    
    /**
     * https://leetcode.com/problems/reverse-linked-list-ii/
     * Reverse a linked list from position m to n. Do it in one-pass.
     * Input: 1->2->3->4->5->NULL, m = 2, n = 4
     * Output: 1->4->3->2->5->NULL
     */
    def reverseBetween(head: ListNode, m: Int, n: Int): ListNode = {
        
      if(head == null) return head
      
      var front = head
      var back = head
      
      for(i <- 1 until m-1) {
        front = front.next
      }
      var start = if(m == 1) front else front.next
      var prev = start
      var cur = start.next
      for(i <- 0 until n-m) {
        val tmp = cur.next
        cur.next = prev
        prev = cur
        cur = tmp        
      }
        
      front.next = prev
      start.next = cur
      if(m ==1) prev else head
      
    }
    
    def reverseBetween2(head: ListNode, m: Int, n: Int): ListNode = {
      
      if(head == null) return head
      var prev: ListNode = null
      var cur = head
      var m2 = m
      var n2 = n
      
      while(m2 > 1) {
        prev = cur
        cur = cur.next
        m2 -= 1
        n2 -= 1
      }
      
      val front = prev
      val start = cur
      
      while(n2 > 0) {
        val tmp = cur.next
        cur.next = prev
        prev = cur
        cur = tmp 
        n2 -= 1
      }
      
      start.next = cur
      if(front != null ) {
        front.next = prev
        head
      } else {
        prev
      }
    }
    
    /**
     * https://leetcode.com/problems/linked-list-cycle-ii/
     * 
     * Given a linked list, return the node where the cycle begins. If there is no cycle, return null.
     * To represent a cycle in the given linked list, we use an integer pos which represents the position (0-indexed) in the linked list where tail connects to. If pos is -1, then there is no cycle in the linked list.
     * Note: Do not modify the linked list.
     * 
     * 
     * Assume the distance from head to the start of the loop is x1
the distance from the start of the loop to the point fast and slow meet is x2
the distance from the point fast and slow meet to the start of the loop is x3
What is the distance fast moved? What is the distance slow moved? And their relationship?

x1 + x2 = distance slow moved
x1 + x2 + x3 + x2 = distance fast moved
x1 + x2 + x3 + x2 = 2 (x1 + x2)
Thus x1 = x3
     */
    def detectCycleStart(head: ListNode) : ListNode = {
      
      if(head == null) return head
      var slow = head
      var fast = head
      while(slow != fast) {
        if(fast == null || fast.next == null) return null
        slow = slow.next
        fast = fast.next.next
      }
      slow = head
      while(slow != fast) {
        slow = slow.next
        fast = fast.next
      }
      slow
    }
    def detectCycleStart2(head: ListNode) : ListNode = {
      
      if(head == null) return head
      var slow = head
      var fast = head
      
      while(fast != null && fast.next != null) {
        slow = slow.next
        fast = fast.next.next
        if(slow == fast) {
          slow = head
          while(slow != fast) {
            slow = slow.next
            fast = fast.next
          }
          return slow
        }
      }
      null
    }    
    
    /**
     * https://leetcode.com/problems/add-two-numbers/
     * You are given two non-empty linked lists representing two non-negative integers. The digits are stored in 
     * reverse order and each of their nodes contain a single digit. Add the two numbers and return it as a linked 
     * list.
     * Input: (2 -> 4 -> 7) + (5 -> 6 -> 4)
     * Output: 7 -> 0 -> 2 -> 1
     * Explanation: 742 + 465 = 1207
     */
    def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {

      val dummyHead = new ListNode(0)
      var cur = dummyHead
      var a = l1
      var b = l2
      var carry = 0
      
      while(a != null || b != null) {
        
        val t1 = if(a != null) a._x else 0
        val t2 = if(b != null) b._x else 0
        val tmp = t1 + t2 + carry
        carry = tmp / 10
        cur.next = new ListNode(tmp % 10)
        cur = cur.next
        if(a != null) a = a.next
        if(b != null) b = b.next
      }
      if(carry > 0) cur.next = new ListNode(carry)
      dummyHead.next
    }
    
    //Not using ListNode in order to utilize scala pattern matching
    def addTwoNumbersRecur(l1: List[Int], l2: List[Int]): List[Int] = {
      
      def addTwo(a: List[Int], b: List[Int], carry: Int): List[Int] = (a, b) match {
        case (Nil, Nil) => {
          if(carry > 0) List(carry) else Nil 
        }
        case (a :: atail, Nil) => {
          val sum = a + carry   
          sum  % 10 :: addTwo(atail, Nil,  sum / 10)
        }
        case (Nil, b:: btail) => {
          val sum = b + carry   
          sum  % 10 :: addTwo(Nil, btail,  sum / 10)
        }
        case (a:: atail, b:: btail) => {
          val sum = a + b + carry   
          sum  % 10 :: addTwo(atail, btail,  sum / 10)
        }
      }
      addTwo(l1, l2, 0)
    }
    
}

/**
 * https://leetcode.com/problems/lru-cache/
 * The cache is initialized with a positive capacity.
 */
class LRUCache(_capacity: Int) {

    def get(key: Int): Int = {
        0
    }

    def put(key: Int, value: Int) {
        
    }

}

  
  def main(args: Array[String]) {
    
    val l1 = List(5,6,7)
    val l2 = List(5,8,7,9,4)
    
    println("1.1.1.1".split("\\.").mkString("[.]"))
    
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