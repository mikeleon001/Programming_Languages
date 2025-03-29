public class Point3D {
    private int x;
    private int y;
    private int z;

    // New points default to zero zero if no coordinates
    // are provided.
    public Point3D() {
        // we call the regular constructor inside
        // the zero-arg constructor to reduce redundancy
        this(0, 0, 0);
    }

    public Point3D(int x, int y, int z) {
        setX(x);
        setY(y);
        setZ(z);
    }

    //Defining methods
    //**********************
    
    //Extract X Coordinate from a point object
    public int getX() {
        return x;
    }
    
    //Set X Coordinate of a point object
    public void setX(int x) {
        this.x = x;
    }
    
    //Extract Y Coordinate from a point object
    public int getY() {
        return y;
    }

    //Set Y Coordinate of a point object
    public void setY(int y) {
        this.y = y;
    }

    //Extract Z Coordinate from a point object
    public int getZ() {
        return z;
    }

    //Set Z Coordinate of a point object
    public void setZ(int z) {
        this.z = z;
    }

    //Find the distance between 2 points in 3D space
    public double distance(Point3D p2) {
        int dx = this.x - p2.x;
        int dy = this.y - p2.y;
        int dz = this.z - p2.z;
        return Math.sqrt(dx * dx + dy * dy + dz * dz);
    }

    //Calculate area of triangle formed by three points in 3D space
    public double triangleArea(Point3D p2, Point3D p3) {
        // Using Heron's formula
        double a = this.distance(p2);
        double b = p2.distance(p3);
        double c = p3.distance(this);
        
        double s = (a + b + c) / 2; // semi-perimeter
        return Math.sqrt(s * (s - a) * (s - b) * (s - c));
    }

    //Compares 2 points and returns True if the points are equal and 
    //returns False otherwise
    public boolean equals(Object other) {
        if (other == null) return false;
        if (other == this) return true;
        if (!(other instanceof Point3D)) return false;

        Point3D p2 = (Point3D)other;
        return (this.x == p2.x) && (this.y == p2.y) && (this.z == p2.z);
    }
    
    //convert point to string
    public String toString() {
        return "(" + this.x + "," + this.y + "," + this.z + ")";
    }

    // Helper class for storing point pairs and their distances
    public static class PointPair {
        public Point3D p1;
        public Point3D p2;
        public double distance;

        public PointPair(Point3D p1, Point3D p2, double distance) {
            this.p1 = p1;
            this.p2 = p2;
            this.distance = distance;
        }
    }
}
