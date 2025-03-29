import java.io.File;
import java.util.Scanner;
import java.util.ArrayList;
import java.util.List;

public class MainPoint3D {
    public static void main(String[] args) {
        //creating points for demonstration
        Point3D p1 = new Point3D(1, 2, 3);
        Point3D p2 = new Point3D(3, 4, 5);
        Point3D p3 = new Point3D(6, 7, 0);
        Point3D p4 = p1;

        //printing all points
        System.out.println("\nObjects (Points) created from Point3D class:");
        System.out.println("p1: " + p1);
        System.out.println("p2: " + p2);
        System.out.println("p3: " + p3);
        System.out.println("p4: " + p4);

        //comparing some points
        System.out.println("\nComparing Point3Ds:");
        System.out.println("p1 == p1? " + (p1 == p1));
        System.out.println("p1 == p2? " + (p1 == p2));
        System.out.println("p2 == p3? " + (p2 == p3));
        System.out.println("p1 == p4? " + (p1 == p4));

        //calculating distances and area
        double d12 = p1.distance(p2);
        double area = p1.triangleArea(p2, p3);
        System.out.println("\nThe distance between p1 and p2 is: " + d12);
        System.out.println("The area of the triangle formed by p1, p2, and p3 is: " + area);

        // Setting new coordinates on point 1
        System.out.println("\nSetting new coordinates for Point3D 1:");
        p1.setX(-99);
        p1.setY(-1);
        p1.setZ(3);
        System.out.println("p1: " + p1);

        System.out.println("\nChanging some values at the object level:");
        p1 = new Point3D(10, 11, 12);
        p4.setX(5);
        p4.setY(-1);
        p4.setZ(3);

        System.out.println("p1: " + p1);
        System.out.println("p4: " + p4);
        System.out.println("p1.equals(p1)? " + p1.equals(p1));
        System.out.println("p1.equals(p2)? " + p1.equals(p2));
        System.out.println("p2.equals(p3)? " + p2.equals(p3));
        System.out.println("p1.equals(p4)? " + p1.equals(p4));

        // Reading points from file and finding closest/farthest pairs
        ArrayList<Point3D> points = new ArrayList<>();
        try {
            Scanner fileScanner = new Scanner(new File("3dinputpoint.txt"));
            while (fileScanner.hasNextLine()) {
                String line = fileScanner.nextLine();
                String[] coords = line.split(",");
                int x = Integer.parseInt(coords[0].trim());
                int y = Integer.parseInt(coords[1].trim());
                int z = Integer.parseInt(coords[2].trim());
                points.add(new Point3D(x, y, z));
            }
            fileScanner.close();

            // Find closest and farthest points
            double minDistance = Double.MAX_VALUE;
            double maxDistance = 0;
            List<Point3D.PointPair> closestPairs = new ArrayList<>();
            List<Point3D.PointPair> farthestPairs = new ArrayList<>();

            for (int i = 0; i < points.size(); i++) {
                for (int j = i + 1; j < points.size(); j++) {
                    Point3D p_i = points.get(i);
                    Point3D p_j = points.get(j);
                    double distance = p_i.distance(p_j);

                    if (distance < minDistance) {
                        minDistance = distance;
                        closestPairs.clear();
                        closestPairs.add(new Point3D.PointPair(p_i, p_j, distance));
                    } else if (distance == minDistance) {
                        closestPairs.add(new Point3D.PointPair(p_i, p_j, distance));
                    }

                    if (distance > maxDistance) {
                        maxDistance = distance;
                        farthestPairs.clear();
                        farthestPairs.add(new Point3D.PointPair(p_i, p_j, distance));
                    } else if (distance == maxDistance) {
                        farthestPairs.add(new Point3D.PointPair(p_i, p_j, distance));
                    }
                }
            }

            // Print results
            System.out.println("\nClosest Points:");
            System.out.println("***************");
            System.out.println("All points closest to each other at a minimum distance of " + minDistance + " are:");
            for (Point3D.PointPair pair : closestPairs) {
                System.out.println(pair.p1 + ", " + pair.p2);
            }

            System.out.println("\nFarthest Points:");
            System.out.println("***************");
            System.out.println("All points farthest to each other at a maximum distance of " + maxDistance + " are:");
            for (Point3D.PointPair pair : farthestPairs) {
                System.out.println(pair.p1 + ", " + pair.p2);
            }

        } catch (Exception e) {
            System.out.println("Error processing file: " + e.getMessage());
        }
    }
}
