package com.scalap.scalaexamples

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