package com.scalap.scalaexamples

object DPProblems {
  
    //https://leetcode.com/problems/maximum-subarray/
    /**
     * Given an integer array nums, find the contiguous subarray (containing at least one number) which has 
     * the largest sum and return its sum.
     * 
     * Input: [-2,1,-3,4,-1,2,1,-5,4],
     * Output: 6 Explanation: [4,-1,2,1] has the largest sum = 6.
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

    //https://leetcode.com/problems/house-robber/
    //https://leetcode.com/problems/house-robber/discuss/156523/From-good-to-great.-How-to-approach-most-of-DP-problems.
    /**
     * Given a list of non-negative integers representing the amount of money of each house,
     *  determine the maximum amount of money you can rob tonight
     *  You cannot rob adjacent house
     *  Input: [1,3,1,1,7]
     *  Output: 10
     */
    object RobHouse {
      
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
        var notRob = 0
        var rob = 0
        for(n <- nums) {
          val tmp = notRob
          notRob = Math.max(notRob, rob)
          rob = n + tmp
        }
        Math.max(notRob, rob)
      }
      
      def rob4(nums: Array[Int]) : Int = {
        
        if(nums == null) return 0 
        val dp = Array.fill(nums.length + 1, 2){0}
        for(i <- 1 to nums.length) {
          dp(i)(0) = Math.max(dp(i-1)(0), dp(i-1)(1))
          dp(i)(1) = dp(i-1)(0) + nums(i-1)
        }
        Math.max(dp(nums.length)(0), dp(nums.length)(1))
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
       */
      object MinCostClimbingStairs {
        
        //Recursive: Time limit exceeds
        def minCostClimbingStairsRecr(cost: Array[Int]) : Int = {
          
          def loop(n : Int) : Int = {
            if(n == 2) return Math.min(cost(0), cost(1))
            if(n < 2) return 0
            Math.min(loop(n-1) + cost(n-1), loop(n-2) + cost(n-2))
          }
          
          loop(cost.length)
        }
        
        //DP - Memoization
        def minCostClimbingStairsDP(cost: Array[Int]) : Int = {
        
          val memo = Array.fill(cost.length + 1){-1}
          
          def loop(n : Int) : Int = {
            if(n == 2) return Math.min(cost(0), cost(1))
            if(n < 2) return 0
            if(memo(n) > -1) return memo(n)
            memo(n) = Math.min(loop(n-1) + cost(n-1), loop(n-2) + cost(n-2))
            memo(n)
          }
          loop(cost.length)
        }      
        
        def minCostClimbingStairs(cost: Array[Int]) : Int = {
          
          var f1 = cost(0)
          var f2 = cost(1)
          
          for(i <- 2 until cost.length) {
            val tmp = cost(i) + Math.min(f1, f2)
            f1  = f2
            f2 = tmp
          }
          
          Math.min(f1,f2)
        }
        
  
        
  
        
      }
      
      //https://leetcode.com/problems/climbing-stairs/solution/
      /**
       * n steps
       * you can either climb 1 or 2 steps. In how many distinct ways can you climb to the top?
       */
      object ClimbStairs {
        
        //Rec and memo; forward
        def climbStairsRec(n: Int): Int = {
          
          val memo = Array.fill(n + 1){-1}
          
          def loop(i: Int) : Int = {
            if(i > n) return 0
            if(i == n) return 1
            if(memo(i) > -1) return memo(i)
            memo(i) = loop(i+1) + loop(i+2)
            memo(i)
          }
          loop(0)
        }
        
       //Rec and memo; backward 
       def climbStairsRec2(n: Int): Int = {
          
          val memo = Array.fill(n + 1){-1}
          
          def loop(i: Int) : Int = {
            if(i < 0) return 0
            if(i == 0) return 1
            if(memo(i) > -1) return memo(i)
            memo(i) = loop(i-1) + loop(i-2)
            memo(i)
          }
          loop(n)
        }
       
       //DP - O(n) time O(1) space
       /**
        * explanation: draw a tree; you can see 3 can be reach via 1 and 2 times via 2 so total 3. 
        * 4 can be reached 3 times via 3 and 2 times via 2 so total 5 and so on..
        */
       def climbStairsDP(n: Int): Int = {
          
          var a = 1
          var b = 2
          
          for(i <- 3 to n) {
            val tmp = a + b 
            a = b
            b = tmp
          }
          b
        }
       
       //DP recursrive - TailRec
       def climbStairsDPRecursive(n: Int): Int = {
         
         @annotation.tailrec
         def loop(i: Int, a: Int = 0, b: Int = 1): Int = {
           if(i == 0 )  b
           else loop(i - 1, b, a + b)
         }
         loop(n)
       }
     }    

  
}