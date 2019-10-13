package com.javap.javaexamples;


public class Solution {
	
	public class ListNode {
	      int val;
	      ListNode next;
	      ListNode(int x) { val = x; }
	}
	
	/**
	 * copy value of next node and delete next node
	 * @param node
	 */
    public void deleteNode(ListNode node) {
        node.val = node.next.val;
        node.next = node.next.next;
    }
    
    /**
     * https://leetcode.com/problems/intersection-of-two-linked-lists/
     * @param headA
     * @param headB
     * @return
     */
    public ListNode getIntersectionNode(ListNode headA, ListNode headB) {
     
    	if(headA == null || headB == null) return null;
    	
    	int lenA = 0;
    	ListNode a = headA;
    	ListNode b = headB;
    	
    	while(a != null) {
    		lenA ++;
    		a = a.next;
    	}
    	int lenB = 0;
    	while(b != null) {
    		lenB ++;
    		b = b.next;
    	}
    	ListNode curA = headA;
        ListNode curB = headB;
    	if(lenA > lenB) {
    		for(int i=0; i < lenA-lenB; i++){
                curA = curA.next;
            } 
    	}
        if(lenA < lenB)  {
            for(int i=0; i< lenB-lenA; i++){
                curB = curB.next;
            }
        }
        while(curA!=null){
            if(curA == curB) return curA;
            curA = curA.next;
            curB = curB.next;
        }
    	return null;
    }
    //https://leetcode.com/problems/intersection-of-two-linked-lists/discuss/49785/Java-solution-without-knowing-the-difference-in-len!
    public ListNode getIntersectionNode2(ListNode headA, ListNode headB) {
        
    	if(headA == null || headB == null) return null;
    	ListNode a = headA;
    	ListNode b = headB;
    	
    	//if a & b have different len, then we will stop the loop after second iteration
    	while(a != b) {
    		//for the end of first iteration, we just reset the pointer to the head of another linkedlist
    		//if no intersection they will both point to Null eventually
    		a = a == null ? headB : a.next;
    		b = b == null ? headA : b.next;
    	}
    	return a;
    }

}
