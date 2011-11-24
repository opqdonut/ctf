import java.util.Scanner;
import java.util.TreeMap;
import java.util.Map;

public class Things {

    public static interface Thing {
        //public static Thing parse(Scanner sc); //meh
    }

    public static class Soldier implements Thing {
        public final String name;
        public final int x;
        public final int y;
        public final int cooldown;
        public final boolean alive;
        public final String flag;

        public Soldier(String name, int x, int y, int cooldown, boolean alive, String flag) {
            this.name = name;
            this.x = x;
            this.y = y;
            this.cooldown = cooldown;
            this.alive = alive;
            this.flag = flag;
        }

        public Soldier(Scanner sc) {
            this(sc.next(), sc.nextInt(), sc.nextInt(), sc.nextInt(), sc.next()=="True", sc.next());
        }
    }

    public static class Flag implements Thing {
        public final int x;
        public final int y;

        public Flag(int x, int y) {
            this.x = x;
            this.y = y;
        }

        public Flag(Scanner sc) {
            this(sc.nextInt(), sc.nextInt());
        }
    }

    public static class Grenade implements Thing {
        public final int x;
        public final int y;
        public final int countdown;
        
        public Grenade(int x, int y, int countdown) {
            this.x = x;
            this.y = y;
            this.countdown = countdown;
        }

        public Grenade(Scanner sc) {
            this(sc.nextInt(), sc.nextInt(), sc.nextInt());
        }
    }

    public static Thing parseThing(String s) {
        Scanner sc = new Scanner(s);

        String cl = sc.next();

        if (cl.equals("Soldier"))
            return new Soldier(sc);
        else if (cl.equals("Flag"))
            return new Flag(sc);
        else if (cl.equals("Grenade"))
            return new Grenade(sc);
        else
            return null;
    }
}
