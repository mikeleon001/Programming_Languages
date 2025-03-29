/******************************************************************************
 * Course: CMPS 3500
 * Activity 6
 * Date: 11/29/24
 * Username: mchitorog
 * Author: Mihail Chitorog
 *
 * File name: Coordinates.java
 * Works with MainCoordinates.java
 *
 * Description:
 * A coordinate system implementation supporting 2D and 3D point operations,
 * including distance calculations, point comparisons, and triangle area 
 * computations.Processes point data from input files to find closest/farthest 
 * points and largest triangles.
 *****************************************************************************/


public class Coordinates {
    // Point class implementation
    public static class Point {
        private int x;
        private int y;

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
    }

    // Point3D class implementation
    public static class Point3D {
        private int x;
        private int y;
        private int z;

        public Point3D() {
            this(0, 0, 0);
        }

        public Point3D(int x, int y, int z) {
            setX(x);
            setY(y);
            setZ(z);
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

        public int getZ() {
            return z;
        }

        public void setZ(int z) {
            this.z = z;
        }

        public double distance(Point3D p2) {
            int dx = this.x - p2.x;
            int dy = this.y - p2.y;
            int dz = this.z - p2.z;
            return Math.sqrt(dx * dx + dy * dy + dz * dz);
        }

        public boolean equals(Object other) {
            if (other == null) return false;
            if (other == this) return true;
            if (!(other instanceof Point3D)) return false;

            Point3D p2 = (Point3D)other;
            return (this.x == p2.x) && (this.y == p2.y) && (this.z == p2.z);
        }
        
        public String toString() {
            return "(" + this.x + "," + this.y + "," + this.z + ")";
        }
    }

    // Helper classes for storing pairs
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

    public static class Point3DPair {
        public Point3D p1;
        public Point3D p2;
        public double distance;

        public Point3DPair(Point3D p1, Point3D p2, double distance) {
            this.p1 = p1;
            this.p2 = p2;
            this.distance = distance;
        }
    }

    // Helper classes for storing triangles
    public static class Triangle {
        public Point p1, p2, p3;
        public double area;

        public Triangle(Point p1, Point p2, Point p3, double area) {
            this.p1 = p1;
            this.p2 = p2;
            this.p3 = p3;
            this.area = area;
        }
    }

    public static class Triangle3D {
        public Point3D p1, p2, p3;
        public double area;

        public Triangle3D(Point3D p1, Point3D p2, Point3D p3, double area) {
            this.p1 = p1;
            this.p2 = p2;
            this.p3 = p3;
            this.area = area;
        }
    }

    // Triangle area computation methods
    public static double computeTriangleArea(Point p1, Point p2, Point p3) {
        // Using Heron's formula
        double a = p1.distance(p2);
        double b = p2.distance(p3);
        double c = p3.distance(p1);
        
        double s = (a + b + c) / 2; // semi-perimeter
        return Math.sqrt(Math.max(0, s * (s - a) * (s - b) * (s - c)));
    }

    public static double computeTriangleArea(Point3D p1, Point3D p2, Point3D p3) {
        // Using Heron's formula in 3D space
        double a = p1.distance(p2);
        double b = p2.distance(p3);
        double c = p3.distance(p1);
        
        double s = (a + b + c) / 2; // semi-perimeter
        return Math.sqrt(Math.max(0, s * (s - a) * (s - b) * (s - c)));
    }
}
