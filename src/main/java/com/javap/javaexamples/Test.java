package com.javap.javaexamples;


import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

public class Test {
  
  private static Map<String, String> roleIds = null;
  
  static {
    
    roleIds = new HashMap<String, String>();
    roleIds.put("ACCOUNT_EXECUTIVE", "401");
    roleIds.put("ALLIANCE_PARTNERSHIP_MANAGER", "402");
    roleIds.put("APPLICATIONS_SYSTEM_ENGINEER", "403");
    roleIds.put("FIELD_SALES_REPRESENTATIVE", "404");
    roleIds.put("INSIDE_SALES_REPRESENTATIVE", "405");
    roleIds.put("LEAD_GENERATION_REPRESENTATIVE", "406");
    roleIds.put("SALES_MANAGER", "407");
    roleIds.put("SALES_EXECUTIVE", "408");
    roleIds.put("SALES_SUPPORT_OPS", "409");
    roleIds.put("VICE_PRESEIDENT", "410");
    
  }
  
  class Item {
	  
	String value;
	double weight;
	
	
	public Item(String value, double weight) {
		super();
		this.value = value;
		this.weight = weight;
	}
	public String getValue() {
		return value;
	}
	public void setValue(String value) {
		this.value = value;
	}
	public double getWeight() {
		return weight;
	}
	public void setWeight(double weight) {
		this.weight = weight;
	}
	  
	  
  }
  
   public static void weightedRandom(Item[] items) {
	
	// Compute the total weight of all items together
	double totalWeight = 0.0d;
	for (Item i : items)
	{
	    totalWeight += i.getWeight();
	}
	// Now choose a random item
	int randomIndex = -1;
	double random = Math.random() * totalWeight;
	for (int i = 0; i < items.length; ++i)
	{
	    random -= items[i].getWeight();
	    if (random <= 0.0d)
	    {
	        randomIndex = i;
	        break;
	    }
	}
	Item myRandomItem = items[randomIndex];
  }
   
   public static <E> E getWeightedRandom(Map<E, Double> weights, Random random) {
	    E result = null;
	    double bestValue = Double.MAX_VALUE;

	    for (E element : weights.keySet()) {
	        double value = -Math.log(random.nextDouble()) / weights.get(element);

	        if (value < bestValue) {
	            bestValue = value;
	            result = element;
	        }
	    }

	    return result;
	}
   
  
   /**
    * Compute the discrete cumulative density function (CDF) of your list -- or in simple terms the array of cumulative sums of the weights. 
    * Then generate a random number in the range between 0 and the sum of all weights (might be 1 or 100)
    * @author npatel
    *
    * @param <E>
    */
   public class RandomCollection<E> {
	    private final NavigableMap<Double, E> map = new TreeMap<Double, E>();
	    private Random random;
	    private double total = 0;

	    public RandomCollection() {
	        this(new Random());
	    }

	    public RandomCollection(Random random) {
	        this.random = random;
	    }

	    public RandomCollection<E> add(double weight, E result) {
	        if (weight <= 0) return this;
	        total += weight;
	        map.put(total, result);
	        return this;
	    }

	    public E next() {
	        double value = random.nextDouble() * total;
	        return map.higherEntry(value).getValue();
	    }
	    
	    public void resetRandom() {
	    		this.random = new Random();
	    }
   }
   
   public void randomCollection2() {
	   
	   String [] lvl1_1 = new String[] {"Ujjwal Kumar",
               "Steve Eddy",
                "Seema",
                "Lauren Ruhstorfer",
                 "Karthik Dosapati",
                 "Karthik Baskaran",
                 "Himanshu",
                 "Rajan",
                 "Pramod",
                 "Prashanthi",
                 "Gowri",
                 "ryan2",
                 "Nirav Patel",
	   };
	   
	   String [] lvl1_2 = new String[] {"Ravi Kumar Gaddabathini",
               "Mukul",
               "Anthony", 
               "Himanshu",
               "Kandarp Desai",
               "Gurtek Singh",
               "Justin Khuc",
               "Peter Tsan",
               "Sumit nair",
               "Tino Granados",
               "Daniel Martinez",
               "Jaspal Bhamra",
               "Ryan"
	   };
	   
	   List<String> lvl1_1S = new LinkedList<String>(Arrays.asList(lvl1_1));
	   List<String> lvl1_2S = new LinkedList<String>(Arrays.asList(lvl1_2));
	   
	   Random ran = new Random();
	   
	   for (int i = 0; i < 13; i++) {
		   
		   String player1 = lvl1_1S.remove(ran.nextInt(lvl1_1S.size()));
		   String player2 = lvl1_2S.remove(ran.nextInt(lvl1_2S.size()));
		   System.out.println(player1 +" : " + player2);
	   }
			   
   }
  
   public void randomCollection3() {
	   
	   String [] lvl1 = new String[] {"Ujjwal Kumar",
               "Steve Eddy",
                "Seema",
                "Lauren Ruhstorfer",
                 "Karthik Dosapati",
                 "Karthik Baskaran",
                 "Rajan",
                 "Pramod",
                 "Prashanthi",
                 "ryan2",
                 "ryanIT",
                 "Anjan"
	   };
	   String [] lvl2 = new String[] {"Ravi Kumar Gaddabathini",
	               "Mukul",
	               "Anthony", 
	               "Himanshu",
	               "Kandarp Desai",
	               "Gurtek Singh",
	               "Justin Khuc",
	               "Peter Tsan"
	   };
	   String [] lvl3 = new String[] {"Sumit nair",
	               "Tino Granados",
	               "Daniel Martinez",
	               "Jaspal Bhamra",
	               "Nirav Patel",
	               "RyanSupport"
	   };
	   
	   
	   List<String> lvl1S = new LinkedList<String>(Arrays.asList(lvl1));
	   List<String> lvl2S = new LinkedList<String>(Arrays.asList(lvl2));
	   List<String> lvl3S = new LinkedList<String>(Arrays.asList(lvl3));
	   
	   int lvl1Len = lvl1.length;
	   int lvl2Len = lvl2.length;
	   int lvl3Len = lvl3.length;
	   
	   System.out.println("lvl1Len : " +lvl1Len);
	   System.out.println("lvl2Len : " +lvl2Len);
	   System.out.println("lvl3Len : " +lvl3Len);
	   
	   Map<String, Integer> lvlCounter = new HashMap<String, Integer>();
	   lvlCounter.put("lvl1", lvl1Len);
	   lvlCounter.put("lvl2", lvl2Len);
	   lvlCounter.put("lvl3", lvl3Len);
	   
	   int lvelTotalCounter = 3;
	   
	   RandomCollection<String> rc = new RandomCollection<String>()
               .add(40, "lvl1").add(40, "lvl2").add(20, "lvl3");

/*	   Random rand = new Random();
	   for(int i = 0; i < 12; i++) {
		   System.out.println(rand.nextInt(12));
	   }*/
	   
	   Random listPicker = new Random();
	   
	   for (int i = 0; i < 13; i++) {
		   System.out.println();
		   String cur = "";
		   do {
			   cur = rc.next();
		   } while(lvlCounter.get(cur) == 0); //Don't use Level which value counter cam down to 0
		   lvlCounter.put(cur, lvlCounter.get(cur) - 1);
		   if(lvlCounter.get(cur) == 0) {
			   lvelTotalCounter--;
		   }
		   
		   String next = "";
		   do{
			   next = rc.next();
		   } while((next.equals(cur) || lvlCounter.get(next) == 0) && lvelTotalCounter > 1); //Don't use Level which is same as cur and whose value counter cam down to 0
		   lvlCounter.put(next, lvlCounter.get(next) - 1);
		   if(lvlCounter.get(next) == 0) {
			   lvelTotalCounter--;
		   }
		   
		   System.out.println(cur +" : " + next);
		   String player1 = "";
		   String player2 = "";
		   switch(cur) {
			   case "lvl1" : 
				   //System.out.println(lvl1S.size());
				   player1 = lvl1S.remove(listPicker.nextInt(lvl1S.size()));
				   break;
			   case "lvl2" :
				   //System.out.println(lvl2S.size());
				   player1 = lvl2S.remove(listPicker.nextInt(lvl2S.size()));
				   break;
			   case "lvl3" :
				   //System.out.println(lvl3S.size());
				   player1 = lvl3S.remove(listPicker.nextInt(lvl3S.size()));
				   break;
		   }
		   switch(next) {
		   case "lvl1" : 
			   //System.out.println(lvl1S.size());
			   player2 = lvl1S.remove(listPicker.nextInt(lvl1S.size()));
			   break;
		   case "lvl2" : 
			   //System.out.println(lvl2S.size());
			   player2 = lvl2S.remove(listPicker.nextInt(lvl2S.size()));
			   break;
		   case "lvl3" :
			   //System.out.println(lvl3S.size());
			   player2 = lvl3S.remove(listPicker.nextInt(lvl3S.size()));
			   break;
		   }
		   System.out.println(player1 +" : " + player2);
		   if(lvelTotalCounter == 1) {
			   break;
		   }
	   }
	   
	   List<String> lvlRamining = new LinkedList<String>(lvl1S);
	   lvlRamining.addAll(lvl2S);
	   lvlRamining.addAll(lvl3S);
	   
	   //lvlRamining.forEach( str -> System.out.println(str));
	   randomGrouping(lvlRamining);
	   
	   
   }
   
   public static void randomGrouping(List<String> lvlList) {
	   
	   if(lvlList.size() == 0) return;
	   Random ran = new Random();
	   String player1 = lvlList.remove(ran.nextInt(lvlList.size()));
	   String player2 = lvlList.remove(ran.nextInt(lvlList.size()));
	   System.out.println(player1 +" : " + player2);
	   if(lvlList.size() > 0) randomGrouping(lvlList);
   }
   
  
   
  public static void main(String[] args) {
	  
	 Test test = new Test();
	 test.randomCollection3();
	 
	 //test.randomCollection2();

    /*String dirName = "/Users/npatel/Downloads/tmp/lib";
    File [] files = new File(dirName).listFiles();
    
    try {
      
      for(File file:files){
        ZipFile zipfile = new ZipFile(file);
        Enumeration<? extends ZipEntry> e = zipfile.entries();
        while(e.hasMoreElements()) {
            ZipEntry entry = e.nextElement();
            System.out.println(entry.getName());
        }
        zipfile.close();
      }
        
    } catch(Exception ex) {
        ex.printStackTrace();
    }*/
  }
  
  public static Set<Integer> getRoleSet() {
    Iterator<String> iterator = roleIds.values().iterator();
    Set<Integer> returnSet = new TreeSet<Integer>();
    while (iterator.hasNext()) {
      returnSet.add(Integer.valueOf(iterator.next()));
    }
    returnSet.add(411);
    return returnSet;
  }

}
