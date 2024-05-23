package com.scalap.scalaexamples.problems

object TreeProblems {
  
    class TreeNode(var _value: Int) {
       var value: Int = _value
       var left: TreeNode = _
       var right: TreeNode = _
    }
    
    /**
     * N-ary tree node
     */
    class Node(var _value: Int) {
      var value: Int = _value
      var children: List[Node] = List()
    }
    
  object Easy {
        
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
      
      //dfs, Recursion
      def sortedArrayToBST(nums: Array[Int]): TreeNode = {
          
        //bottom-up traversal; backtracking is only required to get the root node 
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
      
      //DFS - top down; backtracking
      def isSymmetric(root: TreeNode): Boolean = {
        if(root == null) return true
        
        def isMirror(left: TreeNode, right: TreeNode) : Boolean = {
          if(left == null && right == null) return true
          if(left == null || right == null) return false
          left._value == right.value && isMirror(left.right, right.left) && isMirror(left.left, right.right)
        }
        isMirror(root.left, root.right)
      }
      
      //Using BFS
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
      
      //DFS; Recursive Topdown; computation happens on backtracking
      def maxDepth(root: TreeNode): Int = {
      
        if(root == null) return 0
        def loop(node: TreeNode) : Int = {
          if(node == null) return 0
          1 + Math.max(loop(node.left), loop(node.right))
        }
        loop(root)  
      }
      
      //BFS - Iterative using stack
      def maxDepthBFS1(root: TreeNode): Int = {
        
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
      
      //BFS - Iterative using queue
      def maxDepthBFS2(root: TreeNode): Int = {
        
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
    
    /**
     * https://leetcode.com/problems/range-sum-of-bst/
     */
    object RangeSumBST {
      
      def rangeSumBST1(root: TreeNode, L: Int, R: Int): Int = {
        
        if(root == null) return 0
        var sum = 0
        
        //DFS recursive - bottom up
        def loop(node: TreeNode): Unit = {
          if(node == null) return
          if(node.value >= L && node.value <= R) sum += node.value 
          if(node.value > L) loop(node.left)
          if(node.value < R) loop(node.right)
        }
        
        loop(root)
        sum
      }
      
      //top down; backtracking
      def rangeSumBST(root: TreeNode, L: Int, R: Int): Int = {
        
        if(root == null) return 0
        if(root.value < L) return rangeSumBST(root.right, L, R) //`return` stmt is important
        if(root.value > R) return rangeSumBST(root.left, L, R)
        root.value + rangeSumBST(root.left, L, R) + rangeSumBST(root.right, L, R)
      }        

    }  
    
      /**
       * https://leetcode.com/problems/minimum-distance-between-bst-nodes/
       * https://leetcode.com/problems/minimum-absolute-difference-in-bst/
       */
      object MinDiffInBST {
      
        //DFS recursive backtracking
        def minDiffInBST2(root: TreeNode): Int = {
          
          if(root == null) return 0
          var min = Int.MaxValue
          var prev: Int = -1
          
          //DFS inorder
          def loop(node: TreeNode) {
            if(node == null) return 
            loop(node.left)
            if(prev != -1) {
              min = Math.min(min, node.value - prev)
            }
            prev = node.value
            loop(node.right)
          }
          loop(root)
          min
            //Math.min(Math.min(root.value, minDiffInBST(root.left)), Math.min(root.value, minDiffInBST(root.right)))
            
        }    
        
      }
      /**
       * https://leetcode.com/problems/merge-two-binary-trees/
       */
      object MergeBinaryTree {
        
        //DFS preorder - recusrive bottom up
        def mergeTrees(t1: TreeNode, t2: TreeNode): TreeNode = {
            if(t1 == null) return t2
            if(t2 == null) return t1
            
            t1.value = t1.value + t2.value
            t1.left = mergeTrees(t1.left, t2.left)
            t1.right = mergeTrees(t1.right, t2.right)
            t1
        }        
        
      }
      
      //https://leetcode.com/problems/invert-binary-tree/solution/
      object InvertBinaryTree {
        
        //DFS - preorder - recursive; bottom up
        def invertTree2(root: TreeNode): TreeNode = {
          if(root == null ) return null
          val tmp = root.left
          root.left = root.right
          root.right = tmp
          invertTree(root.left)
          invertTree(root.right)
          root
        }
        
        //DFS - postorder - top down; backtracking
        def invertTree(root: TreeNode): TreeNode = {
          if(root == null ) return null
          val left = invertTree(root.left)
          val right = invertTree(root.right)
          root.left = right
          root.right = left
          root
        }

        //BFS - queue
        def invertTreeItr(root: TreeNode): TreeNode = {
          
          if(root == null) return null
          val que = collection.mutable.Queue[TreeNode]()
          que.enqueue(root)
          while(que.nonEmpty) {
            val cur = que.dequeue()
            val tmp = cur.left
            cur.left = cur.right
            cur.right = tmp
            if(cur.left != null) que.enqueue(cur.left)
            if(cur.right != null) que.enqueue(cur.right)
          }
          root
        }
      }
      
      /**
       * 	https://leetcode.com/problems/binary-tree-tilt/
       */
      object FindTilt {
        
        def findTilt1(root: TreeNode): Int = {
              var tiltG = 0 
              
              def loop(root: TreeNode): Int = {
                if(root == null) return 0
                val lefttilt = loop(root.left)
                val righttilt = loop(root.right)
                val tilt = Math.abs(lefttilt - righttilt)
                tiltG += tilt
                val tmp = root.value + lefttilt + righttilt
                root.value = tilt
                tmp
              }
              loop(root)
            tiltG
        }  
        
        def findTilt(root: TreeNode) : Int = {
          
          //DFS recursive - postorder
          def loop(root: TreeNode): (Int, Int) = {
            if(root == null) return (0, 0)
            val left = loop(root.left)
            val right = loop(root.right)
            val tilt = Math.abs(left._1 - right._1)            
            val newSum = root.value + left._1 + right._1
            root.value = tilt
            val newTilt = left._2 + right._2 + tilt
            (newSum, newTilt)
          }
          loop(root)._2
        }
      }
      
      /**
       * https://leetcode.com/problems/binary-tree-level-order-traversal-ii/
       */
      object LevelOrderTraversal {
    
        //BFS
        def levelOrderBottomBfs(root: TreeNode): List[List[Int]] = {
          
          if(root == null) return List[List[Int]]()
          
          val que = collection.mutable.Queue[TreeNode]()
          val lstF = collection.mutable.ListBuffer[List[Int]]()
          
          que.enqueue(root)
          while(que.nonEmpty) {
            var size = que.size
            val lst = collection.mutable.ArrayBuffer[Int]()
            while(size > 0){
              val ele = que.dequeue()
              lst += ele.value
              if(ele.left != null) que.enqueue(ele.left)
              if(ele.right != null) que.enqueue(ele.right)
              size -= 1
            }
            lst.toList +=: lstF //prepend here to create reverse order
          }
          lstF.toList
        }
        
        //DFS
        def levelOrderBottomDfs(root: TreeNode): List[List[Int]] = {
          
          val lstF = collection.mutable.ArrayBuffer[List[Int]]()
          
          //DFS - preorder
          def DFS(root: TreeNode, level: Int) {
            if(root == null) return
            if(lstF.size == level) { //time to create a list for given level
              List[Int]() +=: lstF //prepending list so it'd be in reverse order
            }
            //lstF.size - level - 1 should always be 0 so it can add element to newly prepended list  
            lstF(lstF.size - level - 1) = lstF(lstF.size - level - 1) :+ root.value //appending element to maintain order withing sublist
            DFS(root.left, level + 1)
            DFS(root.right, level + 1)
          }
          DFS(root, 0)
          lstF.toList
        }
        
      }
      
      /**
       * https://leetcode.com/problems/maximum-depth-of-n-ary-tree/
       */
      object MaxDepthNary {
        
        //BFS
        def maxDepthBFS(root: Node) : Int = {
          if(root == null) return 0
          val que = collection.mutable.Queue[Node]()
          var max = 0
          que.enqueue(root)
          while(que.nonEmpty) {
            var size = que.size
            while(size > 0){
              val ele = que.dequeue()
              if(ele.children.nonEmpty) ele.children.foreach(ch => que.enqueue(ch))
              size -= 1
            }
            max += 1
          }
          max
        }
        
        //DFS using foldleft
        def maxDepthDFS(root: Node) : Int = {
          
          if(root == null) return 0
          
          def maxDepth(root: Node, max: Int) : Int = {
            if(root == null) return 0
            1 + root.children.foldLeft(0){ (max, node) => 
              Math.max(max, maxDepth(node, max))
            }
          }
          maxDepth(root, 0)
        }
        
        //DFS using foreach
        def maxDepth(root: Node) : Int = {
          
          if(root == null) return 0
          var max = 0
          root.children.foreach { node =>
            max = Math.max(max, maxDepth(node))
          }
          1 + max
        }
      }
      
      /**
       * https://leetcode.com/problems/sum-of-root-to-leaf-binary-numbers/
       */
      object SumRootToLeaf {
        
        //time: O(n), space: O(H) - H recursion stack with length of tree height
        def sumRootToLeaf(root: TreeNode): Int = {
            
          var sum = 0
          def loop(root: TreeNode, num : Int) : Int = {
            if(root == null) return 0
            val num2 = (num << 1 ) | root.value
            if(root.left == null && root.right == null) sum += num2
            loop(root.left, num2)
            loop(root.right, num2)
          }
          loop(root, 0)
          sum
        }
        
        //DFS preorder iterative
        def sumRootToLeafIter(root: TreeNode): Int = {
          
          if(root == null) return 0
          var sum = 0
          val stack = collection.mutable.Stack[Tuple2[TreeNode, Int]]()
          stack.push((root, 0))
          while(stack.nonEmpty) {
            val ele = stack.pop()
            val root = ele._1
            val curnum = ele._2
            if(root != null) {
                val num = (curnum << 1) | root.value
                if(root.left == null && root.right == null) sum += num
                stack.push((root.right, num))
                stack.push((root.left, num))
            }                           
          }
          sum
        }
        
      }
      
      /**
       * https://leetcode.com/problems/path-sum/
       */
      object PathSum1 {
        
        def hasPathSum2(root: TreeNode, targetSum: Int): Boolean = {
        
          def dfs(node: TreeNode, sumSoFar: Int) : Boolean = {
            if(node == null) return false //return stmt is imp here
            val curSum = sumSoFar + node.value
            if(node.left == null && node.right == null) return curSum == targetSum //return stmt is imp here
            dfs(node.left, curSum) || dfs(node.right, curSum)
          }
          dfs(root, 0)
        }
        //Math trick by modifying targetSum on each call
        def hasPathSum(root: TreeNode, targetSum: Int): Boolean = {
          if(root == null) return false //return stmt is imp here
          if(root.left == null && root.right == null) return root.value == targetSum //return stmt is imp here
          hasPathSum(root.left, targetSum - root.value) || hasPathSum(root.right, targetSum - root.value)
        }        
        
      }
      
      /**
       * https://leetcode.com/problems/increasing-order-search-tree/
       */
      object IncreasingBST {
        
        //in-order with array
        def increasingBST(root: TreeNode): TreeNode = {
          
          val arr = collection.mutable.ArrayBuffer[Int]()
          
          def inorder(node: TreeNode) {
            if(node == null) return
            inorder(node.left)
            arr += node.value
            inorder(node.right)
          }
          inorder(root)
          
          val tree = new TreeNode(0)
          arr.foldLeft(tree){(tree, v) =>
            tree.right = new TreeNode(v)
            tree.right
          }
          tree.right
        }    
        
        //Inorder; creating new tree directly using existing nodes
        def increasingBST2(root: TreeNode): TreeNode = {
          
          val tree = new TreeNode(0) //dummy root node
          var cur = tree
          
          def inorder(node: TreeNode) {
            if(node == null) return
            inorder(node.left)
            node.left = null
            cur.right = node
            cur = node
            inorder(node.right)
          }
          inorder(root)
          tree.right
        }
        
        //in-place by relinking existing nodes; with clousre variables to track head and prev
        def increasingBST3(root: TreeNode): TreeNode = {
          
          var prev: TreeNode = null
          var head: TreeNode = null
          
          def dfs(node: TreeNode) {
            if(node == null) return
            dfs(node.left)
            if(prev != null) {
              node.left = null
              prev.right = node
            }
            if(head == null) head = node // record the left most node as it will be our root
            prev = node
            dfs(node.right)
          }
          dfs(root)
          head
        }
        
        //in-place by relinking existing nodes; tail node is different for left and right subtree 
        def increasingBST4(root: TreeNode): TreeNode = {
          
          def dfs(node: TreeNode, tail: TreeNode): TreeNode = {
            if(node == null) return tail
            val res = dfs(node.left, node) //left subtree gets curnode as its future tail
            node.left = null
            node.right = dfs(node.right, tail) //right subtree gets parent node as its future tail
            res //maintain same head pointer throughout backtracking
          }
          dfs(root, null) 
        }
        
      }
      
      /**
       * https://leetcode.com/problems/minimum-depth-of-binary-tree/
       */
      object MinDepth {
        
        def minDepth2(root: TreeNode): Int = {
          if(root == null) return 0
          var min = Int.MaxValue
          
          def dfs(node: TreeNode, curSum: Int) {
            if(node == null) return 
            if(node.left == null && node.right == null) min = Math.min(min, curSum)
            dfs(node.left, curSum + 1)
            dfs(node.right, curSum + 1)
          }
          dfs(root, 1)
          min
        }   
        
        //pure back tracking; top down
        //Postorder traversal
        def minDepth(root: TreeNode): Int = {
          if(root == null) return 0
          val left = minDepth(root.left)
          val right = minDepth(root.right)
          if(left == 0 || right == 0)  left + right + 1 else Math.min(left, right) + 1  
        }
        
        def minDepthBfs(root: TreeNode): Int = {
          if(root == null) return 0
          var que = collection.mutable.Queue[TreeNode]()
          que.enqueue(root)
          var min = 1
          while(que.nonEmpty) {
            var size = que.size
            while(size > 0) {
              val ele = que.dequeue()
              if(ele.left == null && ele.right == null) return min
              if(ele.left != null) que.enqueue(ele.left)
              if(ele.right != null) que.enqueue(ele.right)
              size -= 1
            }
            min += 1
          }
          min
        }
      }
      
      /**
       * 
       * https://leetcode.com/problems/lowest-common-ancestor-of-a-binary-search-tree/
       * 
       * The point from where p and q won't be part of the same subtree or when one is the parent of the other.
       * BST properties:
       * Left subtree of a node N contains nodes whose values are lesser than or equal to node N's value.
				 Right subtree of a node N contains nodes whose values are greater than node N's value.
				 Both left and right subtrees are also BSTs.
       */
      object LowestCommonAncestorBST {
        
        //T : O(N), S: O(N)
        def lowestCommonAncestor(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
          if(p.value > root.value && q.value > root.value) lowestCommonAncestor(root.right, p, q)
          else if(p.value < root.value && q.value < root.value) lowestCommonAncestor(root.left, p, q)
          else root // We have found the split point, i.e. the LCA node.
        }    
        
        //T:O(N) S:O(1)
        def lowestCommonAncestorItr(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
          var node = root
          while(node != null) {
             if(p.value > node.value && q.value > node.value) node = node.right
             else if(p.value < node.value && q.value < node.value) node = node.left
             else return node // We have found the split point, i.e. the LCA node.
          }
          null
        }
      }
      
      // >> mark
      
      /**
       * https://leetcode.com/problems/leaf-similar-trees/
       */
      object SameLeaves {
        
        def leafSimilar1(root1: TreeNode, root2: TreeNode): Boolean = {
          
          val lst1 = collection.mutable.ArrayBuffer[Int]()
          val lst2 = collection.mutable.ArrayBuffer[Int]()
          
          def dfs(node: TreeNode, lst: collection.mutable.ArrayBuffer[Int]) {
            if(node != null) {
              if(node.left == null && node.right == null) lst += node.value
              dfs(node.left, lst)
              dfs(node.right, lst)
            }
          }
          dfs(root1, lst1)
          dfs(root2, lst2)
          lst1.equals(lst2)
          
        }
        //Using two stack to store nodes from both root1 and root2 and traversing them together
        def leafSimilar2(root1: TreeNode, root2: TreeNode): Boolean = {
          
          val stk1 = collection.mutable.Stack[TreeNode]()
          val stk2 = collection.mutable.Stack[TreeNode]()
          stk1.push(root1)
          stk2.push(root2)
          while(stk1.nonEmpty && stk2.nonEmpty) {
            if(dfs(stk1) != dfs(stk2)) return false
          }
          def dfs(stk: collection.mutable.Stack[TreeNode]) : Int = {
            val node = stk.pop()
            if (node.right != null) stk.push(node.right)
            if (node.left != null) stk.push(node.left)
            if (node.left == null && node.right == null) return node.value
            dfs(stk)
          }
          stk1.isEmpty && stk2.isEmpty //to make sure both leaves of trees have similar lenght and one is not subset of another
        }
        
        /**
         * DFS
         * 
         */
        def leafSimilar3(root1: TreeNode, root2: TreeNode): Boolean = {
          val que = collection.mutable.Queue[Int]()
          var break: Boolean = false //to stop further recrusion
          var ans: Boolean = true //to see if mismatch is found
          def dfs(node: TreeNode, fillque: Boolean) {
            if(node == null || break) return
            if(node.left == null && node.right == null) {
              if(fillque) que.enqueue(node.value) else {
                if(que.isEmpty || node.value != que.dequeue()) {
                  ans = false
                  break = true
                }
              }
            }
            dfs(node.left, fillque)
            dfs(node.right, fillque)
          }
          dfs(root1, true)
          dfs(root2, false)
          ans && que.isEmpty // both is checked as one leaves of tree can be subset of another 
        }
      }
      
      /**
       * https://leetcode.com/problems/sum-of-left-leaves/
       */
      object SumOfLeftLeaves {
        
        def sumOfLeftLeaves(root: TreeNode): Int = {
          if(root == null) return 0
          var ans = 0
          if(root.left != null) {
            if(root.left.left == null && root.left.right == null) ans += root.left.value
            else ans += sumOfLeftLeaves(root.left)
          }
          ans += sumOfLeftLeaves(root.right)
          ans
        }
        
        def sumOfLeftLeavesBfs(root: TreeNode): Int = {
          var stack = collection.mutable.Stack[TreeNode]()
          if(root == null) return 0
          stack.push(root)
          var sum = 0
          while(stack.nonEmpty) {
            val ele = stack.pop()
            if(ele.left != null) {
              if(ele.left.left == null && ele.left.right == null) sum += ele.left.value
              else stack.push(ele.left)
            }
            if(ele.right != null) stack.push(ele.right)
          }
          sum
        }
      }
      
  
      /**
       * https://leetcode.com/problems/find-mode-in-binary-search-tree/
       */
    object FindMode {
      def findMode(root: TreeNode): Array[Int] = {
        
        var arr = collection.mutable.ArrayBuffer[Int]()
        var modcount = 0
        var max = 0
        var prev: TreeNode = null 
        
        def inorder(node: TreeNode) {
          
          if(node == null) return
          inorder(node.left)
          modcount = if(prev != null && node.value == prev.value) modcount + 1 else 1 //increment or reset
          if(modcount > max) {
            max = modcount
            arr.clear()
            arr += node.value
          } else if(modcount == max) {
            arr += node.value
          }
          prev = node
          inorder(node.right)
        }    
        inorder(root)
        arr.toArray
      }
    }
    
    /**
     * https://leetcode.com/problems/balanced-binary-tree/
     */
    object IsBalancedTree {
      
      
      def isBalanced(root: TreeNode): Boolean = {
        
        def postorder(root: TreeNode) : (Int, Boolean) = {
          if(root == null) return (0, true)
          val left = postorder(root.left) 
          val right = postorder(root.right)
          val absdiff = Math.abs(left._1 - right._1)
          val max = Math.max(left._1, right._1) + 1
          val isbalanced = left._2 && right._2 && (absdiff <= 1)
          (max, isbalanced) 
        }
        postorder(root)._2
      }
      

      def isBalanced2(root: TreeNode): Boolean = {
        
        /**
         * Clever (multipurpose) use of Int return type; It can return height or -1 if tree is imbalance;  
         */
        def postorder(root: TreeNode) : Int = {
          if(root == null) return 0
          val left = postorder(root.left)
          if(left == -1) return -1 //left tree is already found to be imbalanced
          val right = postorder(root.right)
          if(right == -1) return -1
          if(left - right < -1 || left - right > 1) return -1
          Math.max(left, right) + 1
        }
        postorder(root) != -1
      }
      
    }
  
    
  }
    
    
    
    object Medium {

      //https://leetcode.com/problems/sum-root-to-leaf-numbers/
      object SumeRootToLeaf {
        
        //DFS recur
        def sumNumbers(root: TreeNode): Int = {
          
          var sum = 0
          def loop(root: TreeNode, curNum: Int) : Int = {
            if(root == null) return 0
            val num = curNum * 10 + root.value
            if(root.left == null && root.right == null) sum += num
            loop(root.left, num)
            loop(root.right, num)
          }
          loop(root, 0)
          sum
        }
        
        //DFS Itr
        def sumNumbersItr(root: TreeNode): Int = {
          
          if(root == null) return 0
          var sum = 0
          val stack = collection.mutable.Stack[Tuple2[TreeNode, Int]]()
          stack.push((root, 0))
          while(stack.nonEmpty) {
            val ele = stack.pop()
            val root = ele._1
            val curNum = ele._2
            val num = curNum * 10 + root.value
            if(root.left == null && root.right == null) sum += num
            if(root.right != null) stack.push((root.right, num))
            if(root.left != null) stack.push((root.left, num))
          }
          sum          
        }        
      }
      
      object SmallestFromLeaf {
        
        //DFS recur
        def smallestFromLeaf(root: TreeNode): String = {
          
          var min = "~"          
          def dfs(node: TreeNode, sb: StringBuilder) {
            if(node == null) return
            val chr = ('a' + node.value).toChar
            sb.append(chr)
            if(node.left == null && node.right == null) {
              val curval = sb.reverse.toString()
              if(curval.compareTo(min) < 0) min = curval
            }
            dfs(node.left, sb)
            dfs(node.right, sb)
            sb.deleteCharAt(sb.length() - 1) //since sb is mutable its necessary to remove last char after we reach leaf
          }
          dfs(root, new StringBuilder())
          min
        }
      }
      
      object PathSum2 {
        
        def pathSum(root: TreeNode, targetSum: Int): List[List[Int]] = {
          
          val lstg = collection.mutable.ArrayBuffer[List[Int]]()
          
          def dfs(node: TreeNode, lst: List[Int], sumSoFar: Int) {
            if(node == null) return
            val curSum = sumSoFar + node.value
            //Following optimization doesn't apply if tree is mix of positive and negative number in which case we have to
            //go through all the paths
            //if(targetSum > 0 && curSum > targetSum) return
            //if(targetSum < 0 && curSum < targetSum) return
            val curLst = lst :+ node.value
            if(node.left == null && node.right == null && curSum == targetSum) {
              lstg += curLst.toList
              return
            }
            dfs(node.left, curLst, curSum)
            dfs(node.right, curLst, curSum)
          }
          dfs(root, List[Int](), 0)
          
          lstg.toList
        }
        
        //BFS
        def pathSumBFS(root: TreeNode, targetSum: Int): List[List[Int]] = {
          val lstg = collection.mutable.ArrayBuffer[List[Int]]()
          val que = collection.mutable.Queue[List[TreeNode]]()
          if(root == null) return lstg.toList
          que.enqueue(List(root))
          while(que.nonEmpty) {
            val ele = que.dequeue()
            val lastele = ele.last
            val cursum = ele.map(_.value).reduce(_ + _)
            if(lastele.left == null && lastele.right == null && cursum == targetSum) {
              lstg += ele.map(_.value)
            } else {
              if(lastele.left != null) que.enqueue(ele :+ lastele.left)
              if(lastele.right != null) que.enqueue(ele :+ lastele.right)
            }
          }
          lstg.toList
        }
      }
      
      object LowestCommonAncestorBST2 {
        
        def lowestCommonAncestor1(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
          
          var paren: TreeNode = null
          def dfs(node: TreeNode) : Boolean = {
            if(node == null) return false
            if(paren != null) throw new RuntimeException //optimize to avoid any further unnecessary calls
            val left = if(dfs(node.left)) 1 else 0 
            val right = if(dfs(node.right)) 1 else 0
            val mid = if(node == p || node == q) 1 else 0
            if(left + mid + right >= 2 ) paren = node
            left + right + mid > 0
          }
          scala.util.Try(dfs(root))
          paren
        }
        
        //Runs slower then above
        def lowestCommonAncestor2(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
          
          var paren: TreeNode = null
          def dfs(node: TreeNode) : Boolean = {
            if(node == null) return false
            if(paren != null) throw new RuntimeException //"optimize" to avoid any further unnecessary calls
            val left = dfs(node.left) 
            val right = dfs(node.right)
            val mid = node == p || node == q
            if(left && mid || left && right || mid && right) paren = node 
            left || right || mid 
          }
          scala.util.Try(dfs(root))
          paren          
        }
        
        //faster
        def lowestCommonAncestor(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
         
          if(root == null || root == p || root == q) return root
          val left = lowestCommonAncestor(root.left, p, q)
          val right = lowestCommonAncestor(root.right, p, q)
          if(left == null) {
            return right
          } else if(right == null) {
            return left 
          } else return root 
        }
        
        //DFS iter
        def lowestCommonAncestorItr(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
          
          if(root == null || p == null || q == null) return null
          
          // HashMap for parent pointers
          val map = collection.mutable.Map[TreeNode, TreeNode]()
          
          // Stack for tree traversal
          val stack = collection.mutable.Stack[TreeNode]()
          
          map.put(root, null)
          stack.push(root)
          
          // Iterate only until we find both the nodes p and q
          while(!map.contains(p) || !map.contains(q)) {
            val ele = stack.pop()
            if(ele.left != null) {
              map.put(ele.left, ele)
              stack.push(ele.left)
            }
            if(ele.right != null) {
              map.put(ele.right, ele)
              stack.push(ele.right)
            }
          }
          
          //Find ancestors of node p
          var ancestors = collection.mutable.Set[TreeNode]()
          var pa = p 
          while(pa != null) {
            ancestors.add(pa)
            pa = map(pa)
          }
          
          var qa = q
          while(!ancestors.contains(qa)) {
            qa = map(qa)
          }
          qa
        }
        
      }
      /**
       * https://leetcode.com/problems/lowest-common-ancestor-of-deepest-leaves/
       * https://leetcode.com/problems/smallest-subtree-with-all-the-deepest-nodes/
       */
      object SubtreeWithAllDeepest {
        
        def subtreeWithAllDeepest(root: TreeNode): TreeNode = {
          
          def dfs(node: TreeNode) : Tuple2[TreeNode, Int] = {
            if(node == null) return (node, 0)
            val left = dfs(node.left)
            val right = dfs(node.right)
            if(left._2 > right._2) return (left._1, left._2 + 1)  
            if(right._2 > left._2) return (right._1, right._2 + 1)
            return (node, left._2 + 1)
          }
          dfs(root)._1
        }
        
        //https://leetcode.com/problems/smallest-subtree-with-all-the-deepest-nodes/discuss/146868/Simple-Java-dfs-recursion-function-with-explanation
        def subtreeWithAllDeepest2(root: TreeNode): TreeNode = {
          
          var maxLevel = 0
          var ans : TreeNode = null
          
          def dfs(node: TreeNode, level: Int) : Int = {
            if(node == null) return level
            val leftLevel = dfs(node.left, level + 1)
            val rightLevel = dfs(node.right, level + 1)
            
            val curLevel = Math.max(leftLevel, rightLevel)
            maxLevel = Math.max(maxLevel, curLevel)
            if(leftLevel == maxLevel && rightLevel == maxLevel) ans = node
            curLevel
          }
          dfs(root, 0)
          ans
        }        
        //Doesnt work for some input
        def subtreeWithAllDeepest3(root: TreeNode): TreeNode = {
          var ans: TreeNode = null
          var max = -1
          def dfs(node: TreeNode, curDepth: Int, parenNode: TreeNode) {
            if(node == null)  return
            if(node.left == null && node.right == null && curDepth >= max) {
              max = curDepth
              ans = if(parenNode.left == null || parenNode.right == null) node else parenNode
            } else {
              dfs(node.left, curDepth + 1, node)
              dfs(node.right, curDepth + 1, node)              
            }
          }
          
          dfs(root, 0, root)
          ans
        }
      }
      
      /**
       * https://leetcode.com/problems/count-good-nodes-in-binary-tree/
       */
      object GoodNodes {
        
        //T: O(n) S: O(h)
        def goodNodes(root: TreeNode): Int = {
          
          var total = 0
          def preorder(root: TreeNode, maxsofar: Int) {
            if(root == null) return
            if(root.value >= maxsofar) total += 1
            val max = Math.max(root.value, maxsofar)
            preorder(root.left, max)
            preorder(root.right, max)
          }
          preorder(root, Int.MinValue)
          total
        }
        
        //T: O(n) S: O(h)
        def goodNodes2(root: TreeNode): Int = {
          
          def preorder(root: TreeNode, maxsofar: Int) : Int = {
            if(root == null) return 0
            val max = Math.max(root.value, maxsofar)
            (if(root.value >= maxsofar) 1 else 0) + preorder(root.left, max) + preorder(root.right, max)
          }
          preorder(root, Int.MinValue)
        }
      }

      /**
       * https://leetcode.com/problems/deepest-leaves-sum/      
       */
      object DeepestLeavesSum {
        def deepestLeavesSum(root: TreeNode): Int = {
            var sum = 0
            var depth = 0
            def dfs(root: TreeNode, curdepth: Int) {
              if(root == null) return
              if(root.left == null && root.right == null) {
                if(curdepth > depth) {
                  depth = curdepth
                  sum = root.value
                }
                else if(curdepth == depth) sum += root.value
              }
              dfs(root.left, curdepth + 1)
              dfs(root.right, curdepth + 1)
            }
            dfs(root, 0)
            sum
        }
        
        def deepestLeavesSumBfs(root: TreeNode): Int = {
          var que = collection.mutable.Queue[TreeNode]()
          if(root == null) return 0
          var sum = 0
          que.enqueue(root)
          while(que.nonEmpty) {
            var size = que.size
            sum = 0 // Reset sum for each level
            while(size > 0) {
              val ele = que.dequeue()
              sum += ele.value
              if(ele.left != null) que.enqueue(ele.left)
              if(ele.right != null) que.enqueue(ele.right)
              size -= 1
            }
          }
          sum
        }
      }
      
      object InorderTraversal {
        
        def inorderTraversal(root: TreeNode): List[Int] = {
          if(root != null) inorderTraversal(root.left) ::: List(root.value) ::: inorderTraversal(root.right ) else List()
        }
        
        /**
         * Mix of tree traversal and stack traversal 
         * The basic idea is referred from here: using stack to simulate the recursion procedure: for each node, 
         * travel to its left child until it's left leaf, then pop to left leaf's higher level node A, and switch to A's right branch. 
         * Keep the above steps until cur is null and stack is empty. 	
         */
        def inorderTraversalItr(root: TreeNode): List[Int] = {
          
          var arr = collection.mutable.ArrayBuffer[Int]()
          if(root == null) return arr.toList
          
          var stk = collection.mutable.Stack[TreeNode]()
          var cur = root
          while(cur != null || stk.nonEmpty) {
            
            while(cur != null) { // Travel to each node's left child, till reach the left leaf
              stk.push(cur)
              cur = cur.left
            }
            cur = stk.pop() // Backtrack to higher level node A
            arr += cur.value
            cur = cur.right
          }
          arr.toList
        }
        
        /**
         * Traerse only stack and track visited node
         * Track visited node
         */
        def inorderTraversalItr2(root: TreeNode): List[Int] = {
          
          var arr = collection.mutable.ArrayBuffer[Int]()
          if(root == null) return arr.toList
                    
          var stk = collection.mutable.Stack[Tuple2[TreeNode, Boolean]]()
          
          stk.push((root, false))
          
          while(stk.nonEmpty) {
            val ele = stk.pop()
            val node = ele._1            
            if(ele._2 || node.left == null) { //when left end is found or node for which all left is visited already
              arr += node.value
              if(node.left == null && node.right != null) stk.push((node.right, false))  
            } else {
              if(node.right != null) stk.push((node.right, false))
              stk.push((node, true))
              if(node.left != null) stk.push((node.left, false))              
            }
          }
          arr.toList
        }
      }
      
      /**
       * https://leetcode.com/problems/validate-binary-search-tree/
       */
      object ValidBST {
        
        def isValidBST(root: TreeNode): Boolean = {
          
          def preorder(root: TreeNode, min: Long, max: Long)  : Boolean = {
            if(root == null) return true
            if(root.value >= max || root.value <= min) return false
            preorder(root.left, min, root.value) && preorder(root.right, root.value, max)
          }
          preorder(root, Long.MinValue, Long.MaxValue)
        }
        /**
         * If its not BST inorder traversal will not be in sorted order
         */
        def isValidBST2(root: TreeNode): Boolean = {
          
          var prev: TreeNode = null
          
          def inorder(root: TreeNode)  : Boolean = {
            if(root == null) return true
            val left = inorder(root.left)
            val mid = if(prev != null) root.value > prev.value else true
            prev = root 
            val right = inorder(root.right) 
            left && mid && right
          }
          inorder(root)
        }
        
        def isValidBST3(root: TreeNode): Boolean = {
          
          var prev: TreeNode = null
          var stack = collection.mutable.Stack[TreeNode]()
          var cur = root
          
          while(cur != null || stack.nonEmpty) {
            if(cur != null) {
              stack.push(cur)
              cur = cur.left
            } else {
              val ele = stack.pop()
              if(prev != null && ele.value <= prev.value) return false
              prev = ele
              cur = ele.right
            }
          }
          true                        
        }
        
      }
      
      /**
       * https://leetcode.com/problems/insert-into-a-binary-search-tree/
       */
      object InsertIntoBST {
        def insertIntoBST(root: TreeNode, `val`: Int): TreeNode = {
            
          if(root == null) return new TreeNode(`val`)
          val target = `val`
          
          def preorder(root: TreeNode) {
            if(root == null) return 
            if(target < root.value) {
              if(root.left == null) {
                root.left = new TreeNode(target) 
              } else preorder(root.left)
            } else if (target > root.value) {
              if(root.right == null) {
                root.right = new TreeNode(target) 
              } else preorder(root.right)
            }
          }
          preorder(root)
          root
        }        
      }
      /**
       * https://leetcode.com/problems/kth-smallest-element-in-a-bst/
       * Trivail soln - inorder -> create an arra -> iterate array and fetch kth ele
       */
      object KthSmallest {
        
        def kthSmallest1(root: TreeNode, k: Int): Int = {
          
          var num = 0 
          var count = 0
          
          def inorder(root: TreeNode) {
            if(root == null) return
            inorder(root.left)
            count += 1
            if(count == k) num = root.value
            inorder(root.right)
          }
          inorder(root)
          num
        }
        
        def kthSmallest2(root: TreeNode, k: Int): Int = {
          
          var num = 0
          var count = k
          
          def inorder(root: TreeNode) {
            if(root.left != null) inorder(root.left)   
            count -= 1
            if(count == 0) num = root.value
            else if(root.right != null) inorder(root.right)
          }
          inorder(root)
          num
        }
        
        
        def kthSmallestDFS(root: TreeNode, k: Int): Int = {
          
          var stk = collection.mutable.Stack[TreeNode]()
          var cur = root
          var k1 = k
          
          while(cur != null || stk.nonEmpty) {
            while(cur != null) {
              stk.push(cur)
              cur = cur.left
            }
            cur = stk.pop()
            k1 -= 1
            if(k1 == 0) return cur.value
            cur = cur.right
          }
          -1
        }
      }
      
      /**
       * https://leetcode.com/problems/longest-zigzag-path-in-a-binary-tree/
       */
      object LongestZigZag {
        
        def longestZigZag(root: TreeNode): Int = {
          
          var maxDepth = 0
          
          /**
           * bottom up
           */
          def dfs(root: TreeNode, direction: Int, curDepth: Int) {
            if(root == null) {
              maxDepth = Math.max(maxDepth, curDepth - 1)
              return
            }
            if(direction == 0) { //need to go left
              dfs(root.left, 1, curDepth + 1)
              dfs(root.right, 0, 1) //check if right tree has even bigger zigzag
            }
            if(direction == 1) {
              dfs(root.right, 0, curDepth + 1)
              dfs(root.left, 1, 1)              
            }
          }
          
          dfs(root, 0, 0)
          maxDepth  
          
        }
        
        def longestZigZag2(root: TreeNode): Int = {
          var maxDepth = 0
          
          //bottom up
          def dfs(root: TreeNode, goLeft: Boolean) : Int = {
            if(root == null) return 0
            val left = dfs(root.left, false)
            val right = dfs(root.right, true)
            maxDepth = Math.max(maxDepth, Math.max(left, right))
            //imp: while coming back check which side of depth you need to consider
            val curValidDepth = if(goLeft) left else right 
            1 + curValidDepth
          }
          dfs(root, true)
          maxDepth
        }

        
      }
      
    }
    
    object Hard {
     
      /**
       * https://leetcode.com/problems/serialize-and-deserialize-binary-tree/solution/
       */
      class CodecDFS {
          
          // Encodes a list of strings to a single string.
          def serialize(root: TreeNode): String = {
            
            val sb = new StringBuilder()
              
            def dfs(root: TreeNode) {
              if(root == null) {
                sb.append("n,")
              } else {
                sb.append(root.value).append(",")
                dfs(root.left)
                dfs(root.right)
              }
            }
            dfs(root)
            sb.toString()
          }
          
          // Decodes a single string to a list of strings.
          def deserialize(data: String): TreeNode = {
            
            val arr = data.split(",")
            var idx = 0
            
            def dfs() : TreeNode = {
              
              if(arr(idx).equals("n") || idx == arr.length) {
                idx += 1
                return null 
              }
              val root = new TreeNode(Integer.parseInt(arr(idx)))
              idx += 1
              root.left = dfs()
              root.right = dfs()
              root
            }
            dfs()
          }
      }
      
      /**
       * https://leetcode.com/problems/serialize-and-deserialize-binary-tree/discuss/74264/Short-and-straight-forward-BFS-Java-code-with-a-queue
       */
      class CodecBFS {
        
          // Encodes a list of strings to a single string.
          def serialize(root: TreeNode): String = {
            val sb = new StringBuilder()
              
            sb.toString()
          }
          
          // Decodes a single string to a list of strings.
          def deserialize(data: String): TreeNode = {
            null
          }
        
      }
      
    }
    
   
    def main(args: Array[String]) {
      
      val treeNode = new TreeNode(1)
      treeNode.left = new TreeNode(2)
      treeNode.right = new TreeNode(5)
      treeNode.left.left =  new TreeNode(3)
      treeNode.left.right =  new TreeNode(4)
      
      val codec = new Hard.CodecDFS()
      val srltree = codec.serialize(treeNode)
      println("srltree:" + srltree)
      
      val treed = codec.deserialize(srltree)
      println("treed: "+ treed)
    }
      
  
}