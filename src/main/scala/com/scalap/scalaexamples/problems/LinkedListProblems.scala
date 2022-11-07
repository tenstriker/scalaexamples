package com.scalap.scalaexamples.problems

import com.scalap.scalaexamples.ListNode

object LinkedListProblems {
  
  object Easy {
    
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
      nxt
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

    
    object Medium {
      
      //https://leetcode.com/problems/remove-nth-node-from-end-of-list/solution/
      
      //2 pass, without dummy node
      def removeNthFromEnd2passes(head: ListNode, n: Int): ListNode = {
        
        if(n == 0 || head == null) return head
        var cur = head
        var len = 0
        
        while(cur != null) {
          cur = cur.next
          len += 1
        }
        len = len - n
        cur = head
        if(len == 0) return head.next //first node delete situation
        len = len - 1
        while(len > 0) {
          cur = cur.next
          len -= 1
        }
        cur.next = cur.next.next
        head
      }
    
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
      
      /**
       * https://leetcode.com/problems/merge-two-sorted-lists/
       */
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
      
      def mergeTwoSortedListsInPlace1(l1: ListNode, l2: ListNode): ListNode = {
        
        if(l1 == null) return l2
        if(l2 == null) return l1
        
        var c1 = l1
        var c2 = l2
        
        val dummy = new ListNode()
        var point = dummy
        
        while(c1 != null && c2 != null) {
          
          if(c1.x <= c2.x) {
            point.next = c1
            c1 = c1.next
          } else {
            point.next = c2
            c2 = c1
            c1 = point.next.next
          }
          point = point.next  
        }
        if(c1 != null) {
          point.next = c1
        } else {
          point.next = c2
        }
        
        dummy.next
      }
      
      //Recursive approach
      def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {
        
        if(l1 == null) return l2
        if(l2 == null) return l1
        
        if(l1.x <= l2.x) {
          l1.next = mergeTwoLists(l1.next, l2)
          return l1
        } else {
          l2.next = mergeTwoLists(l1, l2.next)
          return l2
        }
        
      }
      
      def mergeTwoSortedListsInPlace2(l1: ListNode, l2: ListNode): ListNode = {
        
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
    
    object AddTwoNumbers {
      
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
     * https://leetcode.com/problems/swap-nodes-in-pairs/
     */
    object SwapPairs {
      
      def swapPairs1(head: ListNode): ListNode = {
        var dummy = new ListNode(0)
        dummy.next = head
        var cur = dummy
        while(cur.next != null && cur.next.next != null) {
          val first = cur.next
          val second = cur.next.next
          first.next = second.next
          second.next = first
          cur.next = second
          cur = first
        }
        dummy.next
      }
      
      def swapPairs(head: ListNode): ListNode = {
        if(head == null || head.next == null) return head
        val tmp = head.next
        head.next = swapPairs(head.next.next)
        tmp.next = head
        tmp
      }
    }
    
  }
    
  object Hard {
    
    /**
     * https://leetcode.com/problems/merge-k-sorted-lists/solution/
     */
    object MergeKLists {
      
      /**
       * T: O(N logK)
       * S: O(1)
       */
      def mergeKListsDivideAndConquer(lists: Array[ListNode]): ListNode = {
        
        if(lists.isEmpty) return null
        
        def partitionAndMerge(start: Int, end: Int): ListNode = {
          if(start == end) return lists(start)
          if(start < end) {
            val mid = start + (end - start)/2
            val l1 = partitionAndMerge(start, mid)
            val l2 = partitionAndMerge(mid + 1, end)
            return mergeTwoLists(l1, l2)
          } else {
            return null
          }
        }       
        
        def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {
        
          if(l1 == null) return l2
          if(l2 == null) return l1
          
          if(l1.x <= l2.x) {
            l1.next = mergeTwoLists(l1.next, l2)
            return l1
          } else {
            l2.next = mergeTwoLists(l1, l2.next)
            return l2
          }
          
        }       
       
        partitionAndMerge(0, lists.length - 1)
      }
    }
    
    /**
     * https://leetcode.com/problems/merge-k-sorted-lists/solution/
     * Look into comments
     */
    def mergeKListsDivideAndConquer2(lists: Array[ListNode]): ListNode = {
      
      if(lists.isEmpty) return null
      
      var interval = 1
      val len = lists.length
      
      while(interval < len) {
        
        for(i <- 0 until len - interval by 2) {
          lists(i) = Medium.mergeTwoSortedListsInPlace1(lists(i), lists(i + interval))
        }
        interval *= 2
      }
      lists(0)
    }
    
    /**
     *  T: O(Nlogk)
     *  S : O(k) 
     */
    def mergeKLists(lists: Array[ListNode]): ListNode = {
      
      if(lists.isEmpty) return null
      
      //Scala by defautls have max heap priority queue; so changing order here in Ordering
      implicit val a = new Ordering[ListNode] {
        override def compare(a:ListNode, b:ListNode) = {
          if (a.x < b.x) 
              1
          else if (a.x == b.x) 
              0
          else
              -1
        }
      }
      
      //implicit val listOrdering: Ordering[ListNode] = Ordering.by(_.x)
      
      val que = collection.mutable.PriorityQueue[ListNode]()
      
      val dummy = new ListNode()
      var point = dummy
      
      for(node <- lists) {
        if(node != null) que.enqueue(node)
      }
      
      while(que.nonEmpty) { 

        point.next = que.dequeue()
        point = point.next
        val newNode = point.next
        if(newNode != null) que.enqueue(newNode)
      }
      
      dummy.next
    }
    
    
  }
      
  
}