public class Point {
    private int x;
    private int y;

    // Existing constructors and methods remain the same
    public Point() {
        this(0, 0);
    }

    public Point(int x, int y) {
        setX(x);
        setY(y);
    }

    public int getX() {
        return x;
    }
    
    public void setX(int x) {
        this.x = x;
    }
    
    public int getY() {
        return y;
    }

    public void setY(int y) {
        this.y = y;
    }

    public double distance(Point p2) {
        int dx = this.x - p2.x;
        int dy = this.y - p2.y;
        return Math.sqrt(dx * dx + dy * dy);
    }

    public boolean equals(Object other) {
        if (other == null) return false;
        if (other == this) return true;
        if (!(other instanceof Point)) return false;

        Point p2 = (Point)other;
        return (this.x == p2.x) && (this.y == p2.y);
    }
    
    public String toString() {
        return "(" + this.x + "," + this.y + ")";
    }

    // New helper methods for finding closest and farthest points
    public static class PointPair {
        public Point p1;
        public Point p2;
        public double distance;

        public PointPair(Point p1, Point p2, double distance) {
            this.p1 = p1;
            this.p2 = p2;
            this.distance = distance;
        }
    }
}
