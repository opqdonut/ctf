import java.util.Scanner;
import java.util.TreeMap;
import java.util.Map;

public class Thing {
    public enum Type {SOLDIER, FLAG, GRENADE}

    public final Type type;
    public final String origStr;

    private final Map<String,String> props;

    public Thing(Type type, Map<String,String> props, String origStr) {
        this.type=type;
        this.props=props;
        this.origStr=origStr;
    }

    public String getProp(String key) {
        return props.get(key);
    }

    private static Map<String,String> parseProps(Scanner sc) {
        
        Map<String,String> m = new TreeMap<String,String>();

        String s = sc.nextLine();
        s = s.trim();
        s = s.substring(1,s.length()-1); // discard {}
        String[] pairs = s.split(", ");
        
        for (String p : pairs) {
            String[] x = p.split("=");
            String key = x[0].trim();
            String val = x[1].trim();
            m.put(key,val);
            //System.out.println("DBG p:"+p+" key:"+key+" val:"+val);
        }

        return m;
    }

    public static Thing parseThing(String s) {
        Type type;

        Scanner sc = new Scanner(s);

        String cl = sc.next();

        if (cl.equals("SoldierState"))
            type = Type.SOLDIER;
        else if (cl.equals("Flag"))
            type = Type.FLAG;
        else if (cl.equals("Grenade"))
            type = Type.GRENADE;
        else
            throw new Error("Invalid type: "+cl);

        Map<String,String> props = parseProps(sc);

        return new Thing(type,props,s);
    }
}
