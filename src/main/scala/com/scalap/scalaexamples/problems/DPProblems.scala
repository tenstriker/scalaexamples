package com.scalap.scalaexamples.problems

object DPProblems {
  
    //https://leetcode.com/problems/maximum-subarray/
    /**
     * Given an integer array nums, find the contiguous subarray (containing at least one number) which has 
     * the largest sum and return its sum.
     * 
     * Input: [-2,1,-3,4,-1,2,1,-5,4],
     * Output: 6 Explanation: [4,-1,2,1] has the largest sum = 6.
     * 
     * Linear DP
     * knapsack
     * 
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
      
        //backtracking ; from front
        //https://leetcode.com/problems/maximum-subarray/discuss/213802/Java-Recursive-Solution
        def maxSubArrayRec(nums: Array[Int]): Int = {
          
          if(nums.isEmpty) return 0
          if(nums.size == 1) return nums(0)
          
          var maxSoFar = Int.MinValue
          
          def max(idx: Int) : Int = {
            if(idx == 0) {
              maxSoFar = nums(idx)
              return maxSoFar
            } 
            val preMax = max(idx - 1)
            val curMax = Math.max(nums(idx), nums(idx) + preMax)//if(preMax < 0) nums(idx) else preMax + nums(idx)
            maxSoFar = Math.max(maxSoFar, curMax)
            curMax
          }
          max(nums.length - 1)
          maxSoFar
        }
        
        //backtracking; from back
        def maxSubArrayRec2(nums: Array[Int]): Int = {
          
          if(nums.isEmpty) return 0
          if(nums.size == 1) return nums(0)
          
          var maxSoFar = Int.MinValue
          
          def max(idx: Int) : Int = {
            if(idx == nums.length - 1) {
              maxSoFar = nums(idx)
              return maxSoFar
            } 
            val preMax = max(idx + 1)
            val curMax = Math.max(nums(idx), nums(idx) + preMax)//if(preMax < 0) nums(idx) else preMax + nums(idx)
            maxSoFar = Math.max(maxSoFar, curMax)
            curMax
          }
          max(0)
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
      //1st pass to compute all max subarray; second pass to get max from all
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
     * 
     * Linear DP
     * knapsack
     * 
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
      
      def maxProfitRec(prices: Array[Int]): Int = {
        
        if(prices.length <= 1) return 0
        var maxProfitSofar = 0
        var minBuySoFar = Int.MaxValue
        
        def max(i: Int) : Int = {
          if(i <= 0) {
            minBuySoFar = prices(0) 
            return minBuySoFar
          }
          minBuySoFar = Math.min(max(i-1), prices(i))
          maxProfitSofar = Math.max(maxProfitSofar, prices(i) - minBuySoFar)
          minBuySoFar
        }
        max(prices.length - 1)
        maxProfitSofar
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
       *   
       * Linear DP
       * 
       */
      object MinCostClimbingStairs {
        
        //Recursive: Time limit exceeds
        def minCostClimbingStairsRecr(cost: Array[Int]) : Int = {
          
          def minCost(n : Int) : Int = {
            if(n == 2) return Math.min(cost(0), cost(1)) //not necessary
            if(n < 2) return 0
            Math.min(minCost(n-1) + cost(n-1), minCost(n-2) + cost(n-2)) //overlaping subsequence
          }
          
          minCost(cost.length)
        }
        
        //DP - Memoization
        def minCostClimbingStairsDP(cost: Array[Int]) : Int = {
        
          val memo = Array.fill(cost.length + 1){-1}
          
          def loop(n : Int) : Int = {
            if(n == 2) return Math.min(cost(0), cost(1))
            if(n < 2) return 0
            if(memo(n) > -1) return memo(n)
            memo(n) = Math.min(loop(n-1) + cost(n-1), loop(n-2) + cost(n-2)) //overlaping subsequence
            memo(n)
          }
          loop(cost.length)
        }      
        
        /**
         * knapsack
         */
        def minCostClimbingStairs(cost: Array[Int]) : Int = {
          
          var notTake = cost(0)
          var take = cost(1)
          
          for(i <- 2 until cost.length) {
            val tmp = take
            take = cost(i) + Math.min(notTake, take)
            notTake  = tmp
          }
          
          Math.min(notTake,take)
        }
        
      }
      
      //https://leetcode.com/problems/climbing-stairs/solution/
      /**
       * n steps
       * you can either climb 1 or 2 steps. In how many distinct ways can you climb to the top?
       * 
       * Linear DP
       * Distinct ways
       * 
       * 
       */
      object ClimbStairs {
        
        /**        
         * Rec and memo; forward
         * Explanation: draw a tree or just list out all possible ways in a separate line
         */
        def climbStairsRec(n: Int): Int = {
          
          val memo = Array.fill(n + 1){-1}
          
          def climbStairs(i: Int) : Int = {
            if(i > n) return 0
            if(i == n) return 1
            if(memo(i) > -1) return memo(i)
            memo(i) = climbStairs(i+1) + climbStairs(i+2) //overlaping subsequence
            memo(i)
          }
          climbStairs(0)
        }
        
       /**
        * Rec and memo; backward 
        */
       def climbStairsRec2(n: Int): Int = {
          
          val memo = Array.fill(n + 1){-1}
          
          def climbStairs(i: Int) : Int = {
            if(i < 0) return 0
            if(i == 0) return 1
            if(memo(i) > -1) return memo(i)
            memo(i) = climbStairs(i-1) + climbStairs(i-2) //overlaping subsequence
            memo(i)
          }
          climbStairs(n)
        }
       
       //DP - O(n) time O(1) space
       /**
        * https://leetcode.com/problems/climbing-stairs/solution/
        * you can see 3 can be reach via 1 step of 2 staris and 2 steps of 1 stair, so total 3. 
        * 4 can be reached number of ways you can reach 3 plus number of ways you can reach 2 so total 5 and so on..
        */
       def climbStairsDP(n: Int): Int = {
          
         if(n ==1 ) return 1
         
          var take1 = 1 //no of ways to climb 1 stair
          var take2 = 2 // no of ways to climb 2 stairs
          
          for(i <- 3 to n) {
            val tmp = take2 
            take2 = take1 + take2
            take1 = tmp
          }
          take2
        }
       
       /**
        * DP recursrive - TailRec
        * explanation: draw a tree of subsets/combinations or just list out all possible ways in a separate line
        */
       def climbStairsDPRecursive(n: Int): Int = {
         
         @annotation.tailrec
         def loop(i: Int, a: Int = 0, b: Int = 1): Int = {
           if(i == 0 )  b
           else loop(i - 1, b, a + b)
         }
         loop(n)
       }
     }
      
     
    object Medium {
      
      //https://leetcode.com/problems/house-robber/
      //https://leetcode.com/problems/house-robber/discuss/156523/From-good-to-great.-How-to-approach-most-of-DP-problems.
      /**
       * Given a list of non-negative integers representing the amount of money of each house,
       *  determine the maximum amount of money you can rob tonight
       *  You cannot rob adjacent house
       *  Input: [1,3,1,1,7]
       *  Output: 10
       *  
       *  knapsack
       */
      object RobHouse {
        
        /**
         * https://leetcode.com/problems/house-robber/discuss/156523/From-good-to-great.-How-to-approach-most-of-DP-problems.
         */
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
          var notRob = 0 //prev house not robbed
          var rob = 0 //prev house robbed
          for(n <- nums) {
            val tmp = notRob
            notRob = Math.max(notRob, rob)
            rob = n + tmp
          }
          Math.max(notRob, rob)
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
  
        
        /**
         * https://leetcode.com/problems/paint-house/
         */
        object PaintHouse {
          
          def minCostRec(costs: Array[Array[Int]]): Int = {
  
            if(costs.length == 0) return 0
            
            val n = costs.length
            
            def paint(house: Int, color: Int) : Int = {
              
              var totalCost = costs(house)(color)
              if(house == n -1) {}
              else if(color == 0) totalCost += Math.min(paint(house + 1, 1), paint(house + 1, 2))
              else if(color == 1) totalCost += Math.min(paint(house + 1, 0), paint(house + 1, 2))
              else totalCost += Math.min(paint(house + 1, 0), paint(house + 1, 1))
              totalCost
            }
            Array(paint(0, 0), paint(0, 1), paint(0, 2)).min
          }
          
          def minCostRecMemo(costs: Array[Array[Int]]): Int = {
  
            if(costs.length == 0) return 0
            
            val n = costs.length
            val memo = Array.fill(n, 3)(-1)
            
            def paint(house: Int, color: Int) : Int = {
  
              if(memo(house)(color) > -1) return memo(house)(color)
              
              var totalCost = costs(house)(color)
              if(house == n -1) {}
              else if(color == 0) totalCost += Math.min(paint(house + 1, 1), paint(house + 1, 2))
              else if(color == 1) totalCost += Math.min(paint(house + 1, 0), paint(house + 1, 2))
              else totalCost += Math.min(paint(house + 1, 0), paint(house + 1, 1))
              memo(house)(color) = totalCost
              totalCost
            }
            Array(paint(0, 0), paint(0, 1), paint(0, 2)).min
          }
          
          //Bottom-up Itr using input array to store values
          def minCostDP1(costs: Array[Array[Int]]): Int = {
            
            if(costs.length == 0) return 0
            
            for(i <- costs.length - 2 to 0 by -1) {
            
              costs(i)(0) += Math.min(costs(i + 1)(1), costs(i + 1)(2))
              costs(i)(1) += Math.min(costs(i + 1)(0), costs(i + 1)(2))
              costs(i)(2) += Math.min(costs(i + 1)(0), costs(i + 1)(1))
            }
            
            Math.min(Math.min(costs(0)(0), costs(0)(1)), costs(0)(2))
          }
          
          //Top-down Itr with N variables
          def minCostDP2(costs: Array[Array[Int]]): Int = {
            
            if(costs.length == 0) return 0
            
            var r = costs(0)(0)
            var g = costs(0)(1)
            var b = costs(0)(2)
            
            for(i <- 1 until costs.length ) {
            
              val t1 = costs(i)(0) + Math.min(g, b)
              val t2 = costs(i)(1) + Math.min(r, b)
              val t3 = costs(i)(2) + Math.min(r, g)
              r = t1 
              g = t2
              b = t3
            }
            
            Math.min(Math.min(r, g), b)
          }
          //NOTE: this is not soln. it only works if costs of paint is not same and its not a DP soln either
          def minCostFun1(costs: Array[Array[Int]]): Int = {
            
            costs.foldLeft(0,0)((t2, costarr) => {
              var min = Int.MaxValue
              var minIdx = t2._2
              for(i <- 0 until costarr.length) {
                if(i != t2._2 && costarr(i) < min ) {
                  min = costarr(i)
                  minIdx = i 
                }
              }
              (t2._1 + min, minIdx)
            })._1
            
          }
          
          
        } 
        
        object Subsequence {
          
          
          def isSubsequence(s: String, t: String): Boolean = {
            if(s.isEmpty()) return true
            var i = 0
            for(ch <- t){
              if(ch == s(i)) i += 1
              if(i >= s.length()) return true
            }
            false
          }
          
          def isSubsequenceRec(s: String, t: String): Boolean = {
            
            def loop(si: Int, ti: Int) : Boolean = {
              
              if(si == s.length()) return true
              if(ti == t.length()) return false
              if(s(si) == t(ti)) {
                loop(si + 1, ti + 1)
              } else {
                loop(si, ti + 1)
              }
              
            }
            
            loop(0, 0)
          }
          
          //https://leetcode.com/problems/is-subsequence/solution/
          def isSubsequenceDP(s: String, t: String): Boolean = {
            
            val slen = s.length()
            val tlen = t.length()
          
            if (slen == 0) return true
            
            val dp = Array.fill(slen + 1, tlen + 1){0}
            
            for(column <- 1 to tlen) {
              for(row <- 1 to slen) {
                if(s(row-1) == t(column-1)) {
                  //found match
                  dp(row)(column) = dp(row-1)(column-1) + 1
                } else {
                  dp(row)(column) = Math.max(dp(row-1)(column), dp(row)(column-1))  
                }
              }
              // check if we can consume the entire source string,
              // with the current prefix of the target string.
              if (dp(slen)(column) == slen) return true
            }
            false
          }
          
        }
        
      /**
       * https://leetcode.com/problems/paint-fence/
       */
      object PaintFence {
        
        //https://leetcode.com/problems/paint-fence/discuss/71151/Lucas-formula-maybe-%22O(1)%22-and-34-liners
        def paintFenceRecursive(n: Int, k: Int): Int = {
          
          if(k == 0) return 0
          if(k ==1 && n >2) return 0
          
          def loop(n: Int, k: Int): Int = {
            if(n == 0 || k == 0) return 0 
            if(n == 1) return k
            
            (loop(n-1, k) + loop(n-2, k)) * (k-1)
          }
          loop(n, k)
        }
        
        //https://leetcode.com/problems/paint-fence/discuss/178010/The-only-solution-you-need-to-read
        //DP
        def paintFence(n: Int, k: Int): Int = {
          
          if(n == 0) return 0
          if(n == 1) return k
          if(n == 2) return k * k
          
          val table = Array.ofDim[Int](n+1)
          table(0) = 0
          table(1) = k
          table(2) = k * k
          
          for(i <- 3 to n) {
            table(i) = (table(i-1) + table(i-2)) * (k-1)
          }
          table(n)
        }
        
        //https://leetcode.com/problems/paint-fence/discuss/71150/Python-solution-with-explanation
        def paintFence2(n: Int, k: Int): Int = {
          
          if(n == 0) return 0
          if(n == 1) return k
          if(n == 2) return k * k
          var same = k
          var diff = k * (k - 1)
          
          for(i <- 3 to n) {
            val tmp = diff * 1
            diff = (same + diff) * (k-1)
            same = tmp
          }
          
          same + diff
        }
      }
      
      /**
       * https://leetcode.com/problems/longest-arithmetic-subsequence/
       * 
       */
      object LongestArithSubSeq {
        
        def longestArithSeqLength(A: Array[Int]): Int = {
          var res = 2
          val n = A.length
          val dp = collection.mutable.Map[Int, Int]()
          0
        }        
      }
      
    }
}