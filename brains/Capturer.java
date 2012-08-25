import java.util.ArrayList;
import java.util.Random;
import java.util.Scanner;

public class Capturer {

    static Random r = new Random();

    static char[][] map;

    static String team;

    static int homex, homey;

    public static void main(String[] args) {

        Scanner s = new Scanner(System.in);

        ArrayList<String> mapLines = new ArrayList<String>();
        
        team = s.nextLine();

        String line = s.nextLine();
        while (!line.equals("")) {
            mapLines.add(line);
            line = s.nextLine();
        }

        int h = mapLines.size();
        int w = mapLines.get(0).length();

        map = new char[h][w];

        for (int i = 0; i<h; i++) {
            for (int j = 0; j<w; j++) {
                map[i][j] = mapLines.get(i).charAt(j);
                if (map[i][j] == team.toLowerCase().charAt(0)) {
                    homex = j;
                    homey = i;
                }
            }
        }

        while (true)
            oneRound(s);
    }

    public static boolean moveable(int x, int y) {
        return x>=0 && x<map[0].length
            && y>=0 && y<map.length
            && map[y][x] != '#';
    }

    public static String towards(int x, int y, int xdest, int ydest) {
        int xdiff = xdest-x;
        int ydiff = ydest-y;

        if (xdiff < 0 && moveable (x-1,y))
            return "L";
        if (xdiff > 0 && moveable (x+1,y))
            return "R";
        if (ydiff < 0)
            return "U";
        if (ydiff > 0)
            return "D";
        return "S";
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

    public static String maybeThrow(Things.Soldier s, ArrayList<Things.Enemy> them) {
        if (s.cooldown > 0) {
            return "";
        }
        
        for (Things.Enemy e : them) {
            if (Math.abs(s.x-e.x)+Math.abs(s.y-e.y)<=10) {
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
            
            String dir;
            if (s.flag.equals("No")) {
                dir = randomDirection(s.x,s.y,ef.x,ef.y);
            } else {
                dir = randomDirection(s.x,s.y,homex,homey);
            }
            String gren = maybeThrow(s,them);
            System.out.println(s.name+" "+dir+" "+gren);
        }

        System.out.println("");

    }
}