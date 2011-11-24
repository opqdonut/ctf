public class Thing {
    public enum Team {A, B};
    public enum Type {SOLDIER, FLAG, GRENADE}

    public final Type type;
    public final Team team;
    public final int x;
    public final int y;
    public final String origStr;

    public Thing(Type type, Team team, int x, int y, String origStr) {
        this.type=type;
        this.team=team;
        this.x=x;
        this.y=y;
        this.origStr=origStr;
    }

    public static Team parseTeam(String s) {
        if (s.contains("A"))
            return Team.A;
        else if (s.contains("B"))
            return Team.B;
        else 
            throw new Error("Not a team: "+s);
    }

    public static Thing parseThing(String s) {
        Team team;
        Type type;

        if (s.contains("Team = A"))
            team = Team.A;
        else if (s.contains("Team = B"))
            team = Team.B;
        else
            throw new Error("No team: "+s);

        if (s.contains("SoldierState"))
            type = Type.SOLDIER;
        else if (s.contains("Flag"))
            type = Type.FLAG;
        else if (s.contains("Grenade"))
            type = Type.GRENADE;
        else
            throw new Error("Invalid type: "+s);

        // no coords yet, sorry

        return new Thing(type,team,-1,-1,s);
    }
}
