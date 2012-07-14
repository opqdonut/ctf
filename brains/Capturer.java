import java.util.ArrayList;
import java.util.Random;
import java.util.Scanner;

public class Capturer {

    static Random r = new Random();

    public static void main(String[] args) {

        Scanner s = new Scanner(System.in);

        while (!"".equals(s.nextLine())) {}; // skip map

        while (true)
            oneRound(s);
    }

    public static String towards(int x, int y, int xdest, int ydest) {
        int xdiff = xdest-x;
        int ydiff = ydest-y;
        if (Math.abs(xdiff) > Math.abs(ydiff)) {
            if (xdiff < 0) {
                return "L";
            }
            return "R";
        } else {
            if (ydiff < 0) {
                return "U";
            }
            return "D";
        }
    }

    public static String randomDirection(int x, int y, int xdest, int ydest) {
        switch (r.nextInt(10)) {
        case 0: return "L";
        case 1: return "U";
        case 2: return "R";
        case 3: return "D";
        default: return towards(x,y,xdest,ydest);
        }
    }

    public static String maybeThrow(int x, int y, ArrayList<Things.Enemy> them) {
        for (Things.Enemy e : them) {
            if (Math.abs(x-e.x)+Math.abs(y-e.y)<=10) {
                int yoff = r.nextInt(3)-1;
                int xoff = r.nextInt(3)-1;
                return "("+(e.x+xoff)+","+(e.y+yoff)+")";
            }
        }
        return "";
    }

    public static void oneRound(Scanner sc) {

        ArrayList<Things.Soldier> us = new ArrayList();
        ArrayList<Things.Enemy> them = new ArrayList();

        sc.nextLine(); // skip points

        String l = sc.nextLine();

        Things.EnemyFlag ef = null;

        while (!"".equals(l)) {
            Things.Thing t = Things.parseThing(l);
            if (t instanceof Things.EnemyFlag) {
                ef = (Things.EnemyFlag) t;
            } else if (t instanceof Things.Enemy) {
                them.add((Things.Enemy) t);
            } else if (t instanceof Things.Soldier) {
                us.add((Things.Soldier) t);
            }
            l = sc.nextLine();
        }
        
        for (Things.Soldier s : us) {
            String dir = randomDirection(s.x,s.y,ef.x,ef.y);
            String gren = maybeThrow(s.x,s.y,them);
            System.out.println(s.name+" "+dir+" "+gren);
        }

        System.out.println("");

    }
}