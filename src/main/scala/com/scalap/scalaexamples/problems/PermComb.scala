package com.scalap.scalaexamples.problems

object PermComb {
  
      /**
     * https://leetcode.com/problems/subsets/
     */
    object Subsets {
      
      /**
       * e.g. 1,2,3
       * ()
       * (), (1) //Append 1 to existing sets
       * (), (1), (2), (1,2) //Append 2 to existing sets
       * (), (1), (2), (1,2), (3), (1,3), (2,3), (1,2,3)  //Append 3 to existing sets
       * 
       * T: n * 2^n
       * S: n * 2^n
       * 
       * https://medium.com/@vasanths294/permutation-combination-subset-time-complexity-eca924e00071
       */
      def subsets1(nums: Array[Int]) : List[List[Int]] = {
        
        var subsets = collection.mutable.ArrayBuffer[List[Int]]()
        subsets += List[Int]()
        
        for(n <- nums) {
          for(set <- subsets) { //last updated subsets 
            val cur = set :+ n //append n to every list in subsets
            //https://stackoverflow.com/questions/26249635/scala-mutablelist-when-foreach-add-element-why-not-throw-exception
            subsets += cur // add newly created list to subsets; NO concurrent modification exception!
          }
        }
        subsets.toList
      }
      
      
      /**
       * ()
       * (1)
       * (1,2)
       * (1,2,3)
       * (1,3)
       * (2)
       * (2,3)
       * (3)
       * 
       * [[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]]
       */
      def subsets2(nums: Array[Int]) : List[List[Int]] = {
        
        var subsets = collection.mutable.ArrayBuffer[List[Int]]()
        var n = nums.length
        /**
         * start is moving index; allows us to recurse and backtrack from each number to rest
         * 
         */
        def backtracking(start: Int, cur: collection.mutable.ArrayBuffer[Int]) {
          subsets += cur.toList
          for(i <- start until n) {
            cur += nums(i)
            backtracking(i + 1, cur)
            cur.remove(cur.size - 1)
          }
        }
        backtracking(0, collection.mutable.ArrayBuffer[Int]())
        subsets.toList
        
      }
      
      /**
       * identify number of possible combinations
       * for each combination find bitmask; from there find which number combination represent this bitmap
       * bit mask
       */
      def subsets3(nums: Array[Int]) : List[List[Int]] = {
        
        var subsets = collection.mutable.ArrayBuffer[List[Int]]()
        var n = nums.length
        var p = 1 << n //identify number of possible combinations
        for(i <- 0 until p) {
          val bitmask = Integer.toBinaryString(i | p).substring(1)
          var cur = collection.mutable.ArrayBuffer[Int]()
          for(j <- 0 until n) {
            if(bitmask.charAt(j) == '1') cur += nums(j)
          }
          subsets += cur.toList
        }
        subsets.toList
      }
      
      /**
       * bit mask
       */
      def subsets4(nums: Array[Int]) : List[List[Int]] = {
        
        var subsets = collection.mutable.ArrayBuffer[List[Int]]()
        var n = nums.length
        var p = 1 << n
        for(i <- 0 until p) {
          var cur = collection.mutable.ArrayBuffer[Int]()
          for(j <- 0 until n) {
            if(((i >> j) & 1) == 1) cur += nums(j)
          }
          subsets += cur.toList
        }
        subsets.toList
      }
      
      
      /**
       * Little complicated backtracking; uses k as a no of elements in subsets
       * () // k =0
       * (1),(2),(3) //k = 1
       * (1,2),(1,3),(2, 3) //k = 2
       * (1,2,3) // k = 3
       */ 
      def subsets5(nums: Array[Int]) : List[List[Int]] = {
        
        var subsets = collection.mutable.ArrayBuffer[List[Int]]()
        subsets += List[Int]()
        var n = nums.length
        /**
         * start is moving index; allows us to recurse and backtrack from each number to rest
         * Each recursion level focuses on one element, we need to decide choose or not choose this element. 
         * (Every level split into 2 branches.)
         */
        def backtracking(k: Int, start: Int, cur: collection.mutable.Stack[Int]) {
        
          if(cur.size == k) {
            subsets += cur.toList            
            return
          }
          
          for(i <- start until n) {
            cur.push(nums(i))
            backtracking(k, i + 1, cur)
            cur.pop
          }
        }
        
        for(k <- 1 to n) {
          backtracking(k, 0, collection.mutable.Stack[Int]())
        }
        subsets.toList
      }
      
      def subsets(nums: Array[Int]): List[List[Int]] = 
        nums.foldLeft(List(List.empty[Int])){case (acc, e) => acc ++ acc.map( e :: _)}
    }
    
    /**
     * https://leetcode.com/problems/subsets-ii/
     */
    object SubSets2 {
      
      def subsetsWithDup(nums: Array[Int]): List[List[Int]] = {
        
        var subsets = collection.mutable.ArrayBuffer[List[Int]]()
        var numssorted = nums.sorted
        var n = nums.length
        
        /**
         * Each recursion level focuses on all the following elements. We scan through all the following elements 
         * and decide whether to choose or not choose that element. (Every level split into N branches.)
         */
        def backtracking(start: Int, cur: collection.mutable.ArrayBuffer[Int]) {
          
          subsets += cur.toList
          
          for(i <- start until n) {
            cur += numssorted(i)
            //following is similar to: if(i > start && numssorted(i) == numssorted(i-1)) continue; // skip duplicates
            if(i <= start || (numssorted(i) != numssorted(i - 1))) backtracking(i + 1, cur)
            cur.remove(cur.length - 1)
          }
            
        }
        backtracking(0, collection.mutable.ArrayBuffer[Int]())
        subsets.toList
      }   
      
      def subsetsWithDup2(nums: Array[Int]): List[List[Int]] = {
        
        var subsets = collection.mutable.ArrayBuffer[List[Int]]()
        val numssorted = nums.sorted
        var n = nums.length
        subsets += List[Int]()
        
        var lastIndex = 0
        for(i <- 0 until n) {
          
          //if S[i] is same to S[i - 1], then it needn't to be added to all of the subset, 
          //just add it to the last l subsets which are created by adding S[i - 1]
          
          //Find an offset in subsets for lastly added sets
          val start = if(i == 0 || numssorted(i) != numssorted(i - 1)) 0 else lastIndex 
          lastIndex = subsets.size  
          for(j <- start until lastIndex) {
            val cur = subsets(j) :+ numssorted(i)
            subsets += cur //https://stackoverflow.com/questions/26249635/scala-mutablelist-when-foreach-add-element-why-not-throw-exception
          }
        }
        subsets.toList
      }    
      
    }
    
    /**
     * https://leetcode.com/problems/permutations/
     * O(n*n!)
     */
    object Permutations {
      
      def permute(nums: Array[Int]): List[List[Int]] = {
        
        val res = collection.mutable.ArrayBuffer[List[Int]]()
        val n = nums.length
        
        def backtracking(tmpLst: collection.mutable.ArrayBuffer[Int]) {
          
          if(tmpLst.size == n) res += tmpLst.toList
          
          for(i <- 0 until n) {
            if(!tmpLst.contains(nums(i))) { //linear serch here
              tmpLst += nums(i)
              backtracking(tmpLst)
              tmpLst.remove(tmpLst.size - 1)
            }
          }          
        }
        backtracking(collection.mutable.ArrayBuffer[Int]())
        res.toList
      }
      
      def permute2(nums: Array[Int]): List[List[Int]] = {
        
        val res = collection.mutable.ArrayBuffer[List[Int]]()
        val n = nums.length
        val visited = Array.ofDim[Boolean](n)
        
        def backtracking(tmpLst: collection.mutable.ArrayBuffer[Int]) {
          
          if(tmpLst.size == n) res += tmpLst.toList
          
          for(i <- 0 until n) {
            if(!visited(i)) { 
              tmpLst += nums(i)
              visited(i) = true
              backtracking(tmpLst)
              visited(i) = false
              tmpLst.remove(tmpLst.size - 1)
            }
          }
          
        }
        backtracking(collection.mutable.ArrayBuffer[Int]())
        res.toList
      }   
      
      
      //Using swaps
      def permute3(nums: Array[Int]): List[List[Int]] = {
        
        val res = collection.mutable.ArrayBuffer[List[Int]]()
        val n = nums.length
        
        def swap(i: Int, start: Int, tmpLst: collection.mutable.ArrayBuffer[Int]) {
            val tmp = tmpLst(i)
            tmpLst(i) = tmpLst(start)
            tmpLst(start) = tmp
        }
        
        def backtracking(start: Int, tmpLst: collection.mutable.ArrayBuffer[Int]) {
          
          if(start == n) res += tmpLst.toList
          
          for(i <- start until n) {
            swap(i, start, tmpLst)            
            backtracking(start + 1, tmpLst)
            swap(i, start, tmpLst)
          }
        }
        backtracking(0, collection.mutable.ArrayBuffer(nums : _*))
        res.toList
      }
      
    }
    
    /**
     * https://leetcode.com/problems/permutations-ii/
     */
    object Permute2 {
      
      /**
       * T: O(n*n!)
       */
      def permuteUnique(nums: Array[Int]): List[List[Int]] = {
        
        val res = collection.mutable.ArrayBuffer[List[Int]]()
        val n = nums.length
        val sorted = nums.sorted
        val visited = Array.ofDim[Boolean](n)
        
        def backtracking(tmpLst: collection.mutable.ArrayBuffer[Int]) {
          
          if(tmpLst.size == n) {
            res += tmpLst.toList
            return
          }
          
          for(i <- 0 until n) {
            
            //if(visited(i) || (i > 0 && sorted(i) == sorted(i - 1) && !visited(i - 1))) { continue }
            if(!visited(i) && (i <= 0 || sorted(i) != sorted(i - 1) || visited(i - 1))) { 
              tmpLst += sorted(i)
              visited(i) = true
              backtracking(tmpLst)
              visited(i) = false
              tmpLst.remove(tmpLst.size - 1)
            }
          }          
        }
        backtracking(collection.mutable.ArrayBuffer[Int]())
        res.toList
        
      }
      
      /**
       * map to maintain count of each element
       */
      def permuteUnique2(nums: Array[Int]): List[List[Int]] = {
        
        val res = collection.mutable.ArrayBuffer[List[Int]]()
        val map = collection.mutable.Map[Int, Int]()
        val n = nums.length
        nums.foreach(n => {
          val freq = map.getOrElse(n, 0)
          map.put(n, freq + 1)
        })
        
        def backtracking(tmpLst: collection.mutable.ArrayBuffer[Int]) {
          
          if(tmpLst.size == n) {
            res += tmpLst.toList
            return
          }
          
          map.foreach { case(k, v) =>
            if(v != 0) {
              tmpLst += k
              map.put(k, v - 1)
              backtracking(tmpLst)
              tmpLst.remove(tmpLst.size - 1)
              map.put(k, v)
            }
          }          
        }
        backtracking(collection.mutable.ArrayBuffer[Int]())
        res.toList        
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
    
            if(ch.isLetter) permute(str + ch.toUpper, idx + 1) //permute with upper case
    
            permute(str + ch, idx + 1) // also permute for lower case and permute  for numbers
    
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
    
    /**
     * https://leetcode.com/problems/palindrome-permutation/
     */
    object PermutePalindrome {
      
      def canPermutePalindrome(s: String): Boolean = {
        
        val arr = Array.ofDim[Int](128)
        for (ch <- s) {
          arr(ch) += 1
        }
        var count = 0
        arr.foreach { c =>
          count += c % 2
        }
        count <= 1  
      }  
      def canPermutePalindrome2(s: String): Boolean = {
        
        val arr = Array.ofDim[Int](128)
        var count = 0
        
        for (ch <- s) {
          arr(ch) += 1
          if(arr(ch) % 2 == 0) count -= 1 else count += 1
        }
        count <= 1
      }       
    }
    
    /**
     * https://leetcode.com/problems/palindrome-permutation-ii/
     */
    object PalindromePermutations {
      
      def generatePalindromes(s: String): List[String] = {
          var res = collection.mutable.Set[String]()
          val arr = Array.ofDim[Int](128)
          
          def canPermutePalindrome(s: String): Boolean = {
                    
            for (ch <- s) {
              arr(ch) += 1
            }
            var count = 0
            arr.foreach { c =>
              count += c % 2
            }
            count <= 1  
          } 
          
          
          if(!canPermutePalindrome(s)) {
            return res.toList
          }
          
          var oddch: Char = 0  //odd number char if any
          val st = Array.ofDim[Char](s.length()/2) //half of palindrome str characteres; as we need to permute only half
          var k = 0 
          
          for(i <- 0 until arr.length) {          
            if(arr(i) % 2 == 1) oddch = i.toChar //odd no of occurance - 3, 5, 7 ..
            for(j <- 0 until arr(i) / 2) { //for each 2 occurance take one
              st(k) = i.toChar
              k += 1
            } 
          }
          
          var n = st.length
          
          def permuteUnique(start: Int, st: Array[Char]) {
            
            if(start == n) {
              val odd = if(oddch != 0) oddch.toString() else ""
              val str = new String(st) + odd + new String(st).reverse  
              res += str
              return
            }
            
            for(i <- start until n) {
              if(i == start || st(i) != st(start)) { //wo this can stuck in loop for duplicates
                swap(i, start, st)
                permuteUnique(start + 1, st)
                swap(i, start, st)
              }
            }          
          }
          def swap(i: Int, start: Int, tmpLst: Array[Char]) {
            val tmp = tmpLst(i)
            tmpLst(i) = tmpLst(start)
            tmpLst(start) = tmp
          }
          permuteUnique(0, st)
          res.toList
      }
      
    }
    
    /**
     * https://leetcode.com/problems/palindrome-partitioning/
     */
    object PalindromePartitioning {
      
      //https://leetcode.com/problems/palindrome-partitioning/solution/
      def partition(s: String): List[List[String]] = {
        
        var lst = collection.mutable.ListBuffer[List[String]]()
        
        var curlst = collection.mutable.ListBuffer[String]()
        
        def backtracking(start:Int) {
          if(start == s.length()) {
              lst += curlst.toList
              return
          }
          
          for(i <- start until s.length()) {
            if(isPelindrome(s, start, i)) {
              curlst += s.substring(start, i + 1)
              backtracking(i + 1)
              curlst.remove(curlst.size - 1)
            }
          }
        }
        def isPelindrome(str: String, l: Int, h: Int) : Boolean = {
          var lo = l
          var hi = h
          while(lo < hi) {
            if(str.charAt(lo) != str.charAt(hi)) return false
            lo += 1
            hi -= 1
          }
          true
        }
        backtracking(0)
        lst.toList
      }
      
    }
    
    /**
     * https://leetcode.com/problems/combinations/
     */
    object Combinations {
      
      def combine(n: Int, k: Int): List[List[Int]] = {
        
        var lst = collection.mutable.ArrayBuffer[List[Int]]()
        
        if(n <= 0 || k <= 0) return lst.toList
          
        def backtracking(start: Int, curlst: collection.mutable.ArrayBuffer[Int]) {
          
          if(curlst.size == k) {
            lst += curlst.toList
            return
          }
          for(i <- start until n + 1) {
            curlst += i
            backtracking(i + 1, curlst)
            curlst.remove(curlst.size - 1)
          }
        }
        backtracking(1, collection.mutable.ArrayBuffer[Int]())
        lst.toList
      }
      
    }
    
    /**
     * https://leetcode.com/problems/combination-sum/
     */
    object CombinationSum {
      
      def combinationSum(candidates: Array[Int], target: Int): List[List[Int]] = {
        
        var lst = collection.mutable.ArrayBuffer[List[Int]]()
        
        def backtracking(start: Int, remaining: Int, curlst: collection.mutable.ArrayBuffer[Int]) {
          if(remaining < 0) return
          if(remaining == 0) {
              lst += curlst.toList
              return
          }
          for(i <- start until candidates.length) {
            curlst += candidates(i)
            backtracking(i, remaining - candidates(i), curlst) // not i + 1 because we can reuse same elements
            curlst.remove(curlst.size - 1)
            
          }
        }
        backtracking(0, target, collection.mutable.ArrayBuffer[Int]())
        lst.toList
      }
    }
    
    /**
     * https://leetcode.com/problems/combination-sum-ii/
     */
    object CombinationSum2 {
      
      def combinationSum2(candidates: Array[Int], target: Int): List[List[Int]] = {
        
        val lst = collection.mutable.ListBuffer[List[Int]]()
        val numssorted = candidates.sorted
        
        def backtracking(start: Int, remaining: Int, curlst: List[Int]) {
          
          if(remaining < 0) return
          if(remaining == 0) {
            lst += curlst
            return
          }
          for(i <- start until candidates.length){            
            if(i <= start || (numssorted(i) != numssorted(i - 1))) backtracking(i + 1, remaining - numssorted(i), curlst :+ numssorted(i))
          }
        }
        backtracking(0, target, List[Int]())
        lst.toList
      }
      
    }
    
    /**
     * https://leetcode.com/problems/combination-sum-iii/
     */
    object CombinationSum3 {
      
      def combinationSum3(k: Int, n: Int): List[List[Int]] = {
        
        val lst = collection.mutable.ListBuffer[List[Int]]()        
        def backtracking(start: Int, remaining: Int, curlst: collection.mutable.ArrayBuffer[Int]) {
          if(remaining < 0 || curlst.size > k) return
          if(remaining == 0 && curlst.size == k) {
            lst += curlst.toList
            return
          }
          for(i <- start to 9) {
            curlst += i
            backtracking(i + 1, remaining - i, curlst)
            curlst.remove(curlst.size - 1)
          }
        }
        backtracking(1, n, collection.mutable.ArrayBuffer[Int]())
        lst.toList
      }
      
    }
    
    /**
     * https://leetcode.com/problems/beautiful-arrangement/
     */
    object BeautifulArrangement {
      
        def countArrangement(n: Int): Int = {
          
          var count = 0
          var visited = Array.ofDim[Boolean](n + 1)
          
          def backtracking(pos: Int) {
            
            if(pos > n) count += 1 //since pos is 1 based
            
            for(num <- 1 until n + 1) { //since n is 1 based
              if(!visited(num) && (num % pos == 0 || pos % num == 0)) {
                visited(num) = true
                backtracking(pos + 1)
                visited(num) = false
              }
            }
          }
          backtracking(1)
          count
        }
    }
    
    /**
     * https://leetcode.com/problems/beautiful-arrangement-ii/
     */
    object BeautifulArrangement2 {
      
      /**
       * https://leetcode.com/problems/beautiful-arrangement-ii/discuss/106948/C%2B%2B-Java-Clean-Code-4-liner
       * 1,n,2,n-1,3,n-2,4... ==> Diff: n-1, n-2, n-3, n-4, n-5...
       * By following this pattern, k numbers will have k-1 distinct difference values;
       * and all the rest numbers should have |ai - a_i-1| = 1;
       * In total, we will have k-1+1 = k distinct values.
       */
      def constructArray(n: Int, k: Int): Array[Int] = {
        
        var res = Array.ofDim[Int](n)
        var r = n
        var l = 1
        var i = 0
        var nk = k
        
        while(l <= r) {
          
          if(nk > 1){
            if(nk % 2 != 0) {
             res(i) = l
             l += 1
            } else {
             res(i) = r
             r -= 1
            }
            nk -= 1  
          } else {
            res(i) = l
            l += 1
          }
          i += 1
        }
        res
      }
    }
    
}