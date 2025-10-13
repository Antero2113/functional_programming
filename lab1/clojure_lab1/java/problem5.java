
public class problem5 {
    
    public static long gcd(long a, long b) {
        if (b == 0) {
            return a;
        }
        return gcd(b, a % b);
    }
    
    public static long lcm(long a, long b) {
        return a * b / gcd(a, b);
    }
    
    public static long smallestMultiple(int n) {
        long result = 1;
        for (int i = 2; i <= n; i++) {
            result = lcm(result, i);
        }
        return result;
    }

    // Реализация для задачи 26 (самая длинная повторяющаяся последовательность)
    public static int cycleLength(int n) {
        if (n <= 1) {
            throw new IllegalArgumentException("n must be greater than 1");
        }
        
        int remainder = 1;
        java.util.Map<Integer, Integer> seen = new java.util.HashMap<>();
        int position = 0;
        
        while (remainder != 0) {
            if (seen.containsKey(remainder)) {
                return position - seen.get(remainder);
            }
            seen.put(remainder, position);
            remainder = (remainder * 10) % n;
            position++;
        }
        
        return 0; // Конечная десятичная дробь
    }
    
    public static int longestRecurringCycle(int limit) {
        int maxD = 0;
        int maxLen = 0;
        
        for (int d = 2; d < limit; d++) {
            int len = cycleLength(d);
            if (len > maxLen) {
                maxLen = len;
                maxD = d;
            }
        }
        
        return maxD;
    }
    
    public static void main(String[] args) {
        // Задача 5
        long result = smallestMultiple(20);
        System.out.println("Problem 5 - Smallest multiple: " + result);
        
        // Задача 26
        int cycleResult = longestRecurringCycle(1000);
        System.out.println("Problem 26 - Longest recurring cycle: " + cycleResult);
    }
}