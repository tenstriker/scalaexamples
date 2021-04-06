package com.scalap.scalaexamples.problems

import scala.collection.Seq

object GraphProblems {
  
  object Medium {
  
    /**
     * https://leetcode.com/problems/find-the-town-judge/
     */
    object FindJudge {
      
      /**
       * Edge list presentation
       * Two integer arrays to represent who people trust and who were trusted.
					After storing all values into the two arrays just go through those two arrays and find the person with 
					0 trust person and being trusted by N - 1 people.
       * https://leetcode.com/problems/find-the-town-judge/discuss/244198/Java-Straight-forward-solution
       * T: O(V + E)
       * S: O(2V)
       */
      def findJudge(N: Int, trust: Array[Array[Int]]): Int = {
        val trust1 = Array.ofDim[Int](N)
        val trusted = Array.ofDim[Int](N)
        
        for(i <- 0 until trust.length) {
         
          val a = trust(i)(0)
          val b = trust(i)(1)
          
          //since numbers starts with 1 we offset for 0 based array; 
          //can be avoided if turst1 is of size N + 1 instead of N
          trust1(a - 1) += 1 
          trusted(b - 1) += 1
        }
        for(i <- 0 until N) {
          if(trust1(i) == 0 && trusted(i) == N - 1) return i + 1
        }
        -1 //if no judge found
      }
      
      //Only with one additional array
      def findJudge2(N: Int, trust: Array[Array[Int]]): Int = {
        
        val trustCount = Array.ofDim[Int](N + 1) // N+1 so no need to do offsetting of N values to 0
        
        for(t <- trust) {
          val a = t(0)
          val b = t(1)
          trustCount(a) -= 1 //if you trust your count get decremented
          trustCount(b) += 1 //if you are trusted your count incremented
        }
        
        for(i <- 1 until N + 1) { // 0 index wont' have anything; either `to N` or `until N+1`
          if(trustCount(i) == N - 1) return i
        }
        -1
      }
      
    }
    
    object AllPathsSourceTarget {
      
      /**
       * https://leetcode.com/problems/all-paths-from-source-to-target/
       * T: O(E) S:O(V)
       */
      def allPathsSourceTarget(graph: Array[Array[Int]]): List[List[Int]] = {
        
        var lstg = collection.mutable.ArrayBuffer[List[Int]]()

        if(graph.isEmpty) return lstg.toList
        
        def dfs(v: Int, lst: collection.mutable.Stack[Int]) {
          lst.push(v)
          if(v == graph.length - 1) {
            lstg += lst.toList.reverse
          } else {
            for(vi <- graph(v)) {
              dfs(vi, lst)
            }
          }
          lst.pop()
        }
        dfs(0, new collection.mutable.Stack[Int])
        lstg.toList
      }
      
      /**
       * T: O(n^2)
       * S: O(n^2)
       */          
      def allPathsSourceTargetBfs(graph: Array[Array[Int]]): List[List[Int]] = {
        
        val N = graph.length - 1
        var lstg = collection.mutable.ArrayBuffer[List[Int]]()
        val que = collection.mutable.Queue[Seq[Int]]()
        que.enqueue(Seq(0))
        
        while(que.nonEmpty) {
          val adjl = que.dequeue()
          for(v <- graph(adjl.last)) {
            if(v == N) lstg += (adjl :+ v).toList
            else que.enqueue(adjl :+ v)
          }
        }        
        lstg.toList
      }      
    }
    
    /*
     * minimumEffortPath
     * 
     */
    object MinimumEffortPath {
      def minimumEffortPath(heights: Array[Array[Int]]): Int = {
        0    
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
        
        var m = grid.length
        var n = grid(0).length
        
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
          //NOTE: bitwise & operator
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
     * https://leetcode.com/problems/possible-bipartition/
     */
    object PossibleBipartition {
      
      def possibleBipartition(N: Int, dislikes: Array[Array[Int]]): Boolean = {
          
        import scala.collection.mutable._
        val graph = ArrayBuffer[ArrayBuffer[Int]]()
        val colors = collection.mutable.Map[Int, Int]()
        
        for(i <- 1 to N + 1) graph += ArrayBuffer()
        
        for(edge <- dislikes) {
          graph(edge(0)) += edge(1)
          graph(edge(1)) += edge(0)
        }
        
        for(node <- 1 to N) {
          if(!colors.contains(node) && !dfs(node, 0)) return false
        }
        
        def dfs(node: Int, color: Int) : Boolean = {
          
          if(colors.contains(node)) return color == colors(node)
          colors.put(node, color)
          
          for(neighbor <- graph(node)) {
            if(!dfs(neighbor, color ^ 1)) return false
          }
          true
        }
          
        true
      }
      
    }
    
  }
  
  object Hard {
    

  }
  
}