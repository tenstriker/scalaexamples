package com.scalap.scalaexamples.problems

import scala.annotation.tailrec
import scala.collection.Seq
import scala.runtime.ZippedTraversable2.zippedTraversable2ToTraversable

object StringProblems {
  
  object Easy {
  
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
    
    //https://leetcode.com/problems/implement-strstr/
    //n^2
    def strStrExp(haystack: String, needle: String): Int = {
      
      if(needle.length() == 0) return 0
      if(haystack.length() == 0 || haystack.length() < needle.length()) return -1
      
      for(i <- 0 to haystack.length() - needle.length()) { //no point in checking in haystack after they surpass the difference of length
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
  
  object Medium {
    

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
            set.add(ch)
            j += 1
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
          j +=  1
          max = Math.max(max, j - i)
          map.put(ch, j)
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
    
    object GenerateParenthesis {
      
       def generateParenthesis(n: Int): List[String] = {
       
         List[String]()
       }
       
    }

    
  }
  
  object Hard {
    
    
  }
}