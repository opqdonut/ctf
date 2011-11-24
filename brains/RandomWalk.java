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

    public static void oneRound(Scanner s) {

        ArrayList<Thing> things = new ArrayList<Thing>();

        Thing.Team team = Thing.parseTeam(s.nextLine());

        s.nextLine(); // skip points

        String l = s.nextLine();

        while (!"".equals(l)) {
            things.add(Thing.parseThing(l));
            l = s.nextLine();
        }

        for (Thing t : things) {
            if (t.type == Thing.Type.SOLDIER && t.team == team) {
                System.out.println(t.origStr.substring(29,30)+" "+randomDirection());
            }
        }

        System.out.println("");

    }
}