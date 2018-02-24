import java.util.Calendar;
import java.util.GregorianCalendar;
 
public class DaysInYears {
 
 
        public static void main(String[] args) {
                GregorianCalendar cal  = new GregorianCalendar();
                int[][] count = new int[366][7];
                int[] totals = new int[365];
               
                for (int j = 0; j < 7; j++) {
                        totals[j] = 0;
                        for (int i = 0; i < 365; i++) {
                                count[i][j] = 0;
                        }
                }
               
                for (int i = 0; i < 146097; i++) {
                        cal.add(Calendar.DAY_OF_MONTH, 1);
                        count[cal.get(Calendar.DAY_OF_YEAR)-1 + ((!cal.isLeapYear(cal.get(Calendar.YEAR)) && (cal.get(Calendar.MONTH) > Calendar.FEBRUARY))? 1 : 0)][cal.get(Calendar.DAY_OF_WEEK)-1]++;
                }
               
                for (int i = 0; i < 365; i++) {
                        for (int j = 0; j < 7; j++) {
                                totals[i] += count[i][j];
                        }
                }
               
                System.out.println("Day\t0\t1\t2\t3\t4\t5\t6");
                for (int i = 0; i < 366; i++) {
                        System.out.print(i + "\t");
                        for (int j = 0; j < 7; j++) {
                                System.out.print(count[i][j] + "\t");
                        }
                        System.out.println();
                }
        }
 
}