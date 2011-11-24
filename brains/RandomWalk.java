import java.util.ArrayList;
import java.util.Random;
import java.util.Scanner;

public class RandomWalk {

    static Random r = new Random();

    public static void main(String[] args) {

        Scanner s = new Scanner(System.in);

        while (!"".equals(s.nextLine())) {}; // skip map

        while (true)
            oneRound(s);
    }

    public static String randomDirection() {
        switch (r.nextInt(4)) {
        case 0: return "L";
        case 1: return "U";
        case 2: return "R";
        case 3: return "D";
        }
        return null; // not reached
    }

    public static void oneRound(Scanner sc) {

        ArrayList<Things.Thing> things = new ArrayList();

        sc.nextLine(); // skip points

        String l = sc.nextLine();

        while (!"".equals(l)) {
            things.add(Things.parseThing(l));
            l = sc.nextLine();
        }

        for (Things.Thing t : things) {
            if (t instanceof Things.Soldier) {
                Things.Soldier s = (Things.Soldier) t;
                System.out.println(s.name+" "+randomDirection());
            }
        }

        System.out.println("");

    }
}