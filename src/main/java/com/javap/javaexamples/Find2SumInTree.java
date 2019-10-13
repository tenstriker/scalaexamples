package com.javap.javaexamples;


import java.util.HashMap;
import java.util.Stack;

public class Find2SumInTree {
	
	class TreeNode {
		TreeNode left;
		TreeNode right;
		int val;
		
	}
  
	public int[] twoSum(int[] nums, int target) {

	    HashMap<Integer, Integer> map = new HashMap<Integer, Integer>();
	    int[] eresult = {0,0};
	    
	    for(int i = 0; i < nums.length; i++) {
	        if(map.get(nums[i]) != null) {
	            int[] result = {map.get(nums[i])+1,i+1};
	            return result;
	        }
	        map.put(target-nums[i], i);
	    }
	    return eresult;
	}
	
    public boolean findTarget(TreeNode root, int k) {
        if(root == null) return false;
    	Stack<TreeNode> l_stack = new Stack<>();
    	Stack<TreeNode> r_stack = new Stack<>();
    	stackAdd(l_stack, root, true);
    	stackAdd(r_stack, root, false);
    	while(l_stack.peek() != r_stack.peek()){
    	    int n = l_stack.peek().val + r_stack.peek().val;
    	    if(n == k){
    		return true;
    	    }else if(n > k){
    		stackNext(r_stack, false);
    	    }else{
		stackNext(l_stack, true);
    	    }
    	}
    	return false;
    }
    
    private void stackAdd(Stack<TreeNode> stack, TreeNode node, boolean isLeft){
    	while(node != null){
    	    stack.push(node);
            node = (isLeft) ? node.left : node.right;
    	}
    }

    private void stackNext(Stack<TreeNode> stack, boolean isLeft){
    	TreeNode node = stack.pop();
    	if(isLeft){
    	    stackAdd(stack, node.right, isLeft);
    	}else{
    	    stackAdd(stack, node.left, isLeft);
    	}
    }
	
   
  public static void main(String[] args) {
	  
	  
	  
	  
	 Find2SumInTree test = new Find2SumInTree();
	 
	 
  }
  
 
}
