package com.scalap.scalaexamples

object DFSBFSProblems {
  
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

  
}