package com.scalap.scalaexamples.problems

import scala.collection.Seq

object GraphProblems {
  
  class Node(var value: Int) {
    
    var neighbors: List[Node] = _
    
  }
  
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
     * https://leetcode.com/problems/possible-bipartition/
     */
    object PossibleBipartition {
      
      def possibleBipartition(N: Int, dislikes: Array[Array[Int]]): Boolean = {
          
        import scala.collection.mutable._
        val graph = ArrayBuffer[ArrayBuffer[Int]]()
        val groups = collection.mutable.Map[Int, Int]()
        
        for(i <- 1 to N + 1) graph += ArrayBuffer()
        
        for(edge <- dislikes) {
          graph(edge(0)) += edge(1)
          graph(edge(1)) += edge(0)
        }
        
        for(node <- 1 to N) {
          if(!groups.contains(node) && !dfs(node, 0)) return false
        }
        
        def dfs(node: Int, group: Int) : Boolean = {
          
          if(groups.contains(node)) return group == groups(node)
          groups.put(node, group)
          
          for(neighbor <- graph(node)) {
            if(!dfs(neighbor, group ^ 1)) return false
          }
          true
        }
          
        true
      }
      
    }
    
    /**
     * https://leetcode.com/problems/accounts-merge/solution/
     * https://leetcode.com/problems/accounts-merge/discuss/109158/Java-Solution-(Build-graph-%2B-DFS-search)
     */
    object AccountMerge {
      
      def accountsMerge(accounts: List[List[String]]): List[List[String]] = {
      
        import collection.mutable._
        val res = collection.mutable.ArrayBuffer[List[String]]()
        val emailToName = collection.mutable.Map[String, String]()
        val edgeList = collection.mutable.Map[String, ArrayBuffer[String]]()
        
        for(acc <- accounts) {
          val name = acc(0)
          for(email <- acc.tail) {
            emailToName.put(email, name)
            edgeList.put(email, edgeList.getOrElse(email, ArrayBuffer()) += acc(1))
            edgeList.put(acc(1), edgeList.getOrElse(acc(1), ArrayBuffer()) += email)                
          }
        }
        
        val seen = collection.mutable.Set[String]()
        
        for(email <- edgeList.keySet) {
          if(!seen.contains(email)) {
            seen.add(email)
            val stack = collection.mutable.Stack[String]()
            stack.push(email)
            val merged = collection.mutable.ArrayBuffer[String]()
            while(stack.nonEmpty) {
              val node = stack.pop()
              merged += node
              for(ne <- edgeList(node)){
                if(!seen.contains(ne)) {
                  seen.add(ne)
                  stack.push(ne)
                }
              }
            }
            val sortedMerged = emailToName(email) +: merged.sorted
            res += sortedMerged.toList
          }
        }
        
        res.toList
      }
      /**
       * https://leetcode.com/problems/accounts-merge/solution/
       */
      def accountsMergeUnionFind(accounts: List[List[String]]): List[List[String]] = {
        val res = collection.mutable.ArrayBuffer[List[String]]()
        res.toList
      }
    }
    
    /**
     * https://leetcode.com/problems/clone-graph/
     */
    object CloneGraph {
      
      def cloneGraph(graph: Node): Node = {
        
        var visited = collection.mutable.Map[Node, Node]()
        
        def dfs(node: Node) : Node  = {
          
          if(node == null) return null
          
          if(visited.contains(node)) return visited(node)
          
          val newNode = new Node(node.value)
          visited.put(node, newNode)
          
          val newNeighbors = collection.mutable.ArrayBuffer[Node]()
          
          for(neighbor <- node.neighbors) {
            newNeighbors += cloneGraph(neighbor)  
          }
          
          newNode.neighbors = newNeighbors.toList
          
          newNode
        }
        
        dfs(graph)
      }
    }
    
    /**
     * https://leetcode.com/problems/keys-and-rooms/solution/
     */
    object KeysAndRooms {
      
      def canVisitAllRooms(rooms: List[List[Int]]): Boolean = {
        
        val visited = collection.mutable.Set[Int]()
        
        //DFS recrusive
        def visitRoom(room: Int) {
          
          visited += room
          for(key <- rooms(room)) {
            if(!visited.contains(key)) visitRoom(key)
          }
          
        }
        visitRoom(0)
        visited.size == rooms.size
      }  
      
      def canVisitAllRooms2(rooms: List[List[Int]]): Boolean = {
        
        val visited = collection.mutable.Set[Int]()
        val stk = collection.mutable.Stack[Int]()
        stk.push(0)
        
        //DFS Iterative
        while(stk.nonEmpty) {
          val roomKey = stk.pop
          visited += roomKey
          for(key <- rooms(roomKey)) {
            if(!visited.contains(key)) stk.push(key)
          }
        }
        visited.size == rooms.size
      }    
    }
    
  }
  
  object Hard {
    

  }
  
}