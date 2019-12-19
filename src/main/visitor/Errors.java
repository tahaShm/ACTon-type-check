package main.visitor;

import java.util.*;
import java.util.stream.Collectors;

public class Errors {
    private static HashMap<String, Integer> errorMap;

    public Errors () {
        errorMap = new LinkedHashMap<String, Integer>();
    }

    public static void sortByValue() {
        // Create a list from elements of HashMap
        List<Map.Entry<String, Integer> > list =
                new LinkedList<Map.Entry<String, Integer> >(errorMap.entrySet());

        // Sort the list
        Collections.sort(list, new Comparator<Map.Entry<String, Integer> >() {
            public int compare(Map.Entry<String, Integer> o1,
                               Map.Entry<String, Integer> o2)
            {
                return (o1.getValue()).compareTo(o2.getValue());
            }
        });

        // put data from sorted list to hashmap
        HashMap<String, Integer> temp = new LinkedHashMap<String, Integer>();
        for (Map.Entry<String, Integer> aa : list) {
            temp.put(aa.getKey(), aa.getValue());
        }
        errorMap = temp;
    }

    public void put(String errorName, int line) {
        errorMap.put(errorName, line);
    }

    public static Boolean hasErrors() {
        if (errorMap == null)
            return false;
        if (errorMap.isEmpty())
            return false;
        else
            return true;
    }
    public static HashMap<String, Integer> getErrorMap() {
        return errorMap;
    }
}
