import java.io.File;
import java.util.Scanner;
import java.util.ArrayList;
import java.util.List;

public class MainPoint {
    public static void main(String[] args) {
        // Original demo code remains the same
        Point origin = new Point();
        Point p1 = new Point(1, 2);
        Point p2 = new Point(3, 4);
        Point p3 = new Point(3, 4);
        Point p4 = p1;
        double d12;

        System.out.println("\nObjects (Points) created from Point class:");
        System.out.println("p1: " + p1);
        System.out.println("p2: " + p2);
        System.out.println("p3: " + p3);
        System.out.println("p4: " + p4);

        System.out.println("\nComparing Points:");
        System.out.println("p1 == p1? " + (p1 == p1));
        System.out.println("p1 == p2? " + (p1 == p2));
        System.out.println("p2 == p3? " + (p2 == p3));
        System.out.println("p1 == p4? " + (p1 == p4));

        d12 = p1.distance(p2);
        System.out.println("\nThe distance between p1 and p2 is: " + d12 + "\n");

        System.out.println("\nSetting new coordinates for point 1: ");
        p1.setX(-99);
        p1.setY(-1);
        System.out.println("p1: " + p1);

        System.out.println("\nChanging some values at the object level:");
        p1.setX(5);
        p1 = new Point(7, 8);

        System.out.println("p1: " + p1);
        System.out.println("p4: " + p4);

        System.out.println("p1.equals(p1)? " + p1.equals(p1));
        System.out.println("p1.equals(p2)? " + p1.equals(p2));
        System.out.println("p2.equals(p3)? " + p2.equals(p3));
        System.out.println("p1.equals(p4)? " + p1.equals(p4));

        // New code for reading points from file and finding closest/farthest pairs
        ArrayList<Point> points = new ArrayList<>();
        try {
            Scanner fileScanner = new Scanner(new File("2dinputpoint.txt"));
            while (fileScanner.hasNextLine()) {
                String line = fileScanner.nextLine();
                String[] coords = line.split(",");
                int x = Integer.parseInt(coords[0].trim());
                int y = Integer.parseInt(coords[1].trim());
                points.add(new Point(x, y));
            }
            fileScanner.close();

            // Find closest and farthest points
            double minDistance = Double.MAX_VALUE;
            double maxDistance = 0;
            List<Point.PointPair> closestPairs = new ArrayList<>();
            List<Point.PointPair> farthestPairs = new ArrayList<>();

            for (int i = 0; i < points.size(); i++) {
                for (int j = i + 1; j < points.size(); j++) {
                    Point p_i = points.get(i);
                    Point p_j = points.get(j);
                    double distance = p_i.distance(p_j);

                    if (distance < minDistance) {
                        minDistance = distance;
                        closestPairs.clear();
                        closestPairs.add(new Point.PointPair(p_i, p_j, distance));
                    } else if (distance == minDistance) {
                        closestPairs.add(new Point.PointPair(p_i, p_j, distance));
                    }

                    if (distance > maxDistance) {
                        maxDistance = distance;
                        farthestPairs.clear();
                        farthestPairs.add(new Point.PointPair(p_i, p_j, distance));
                    } else if (distance == maxDistance) {
                        farthestPairs.add(new Point.PointPair(p_i, p_j, distance));
                    }
                }
            }

            // Print results
            System.out.println("\nClosest Points:");
            System.out.println("***************");
            System.out.println("All points closest to each other at a minimum distance of " + minDistance + " are:");
            for (Point.PointPair pair : closestPairs) {
                System.out.println(pair.p1 + ", " + pair.p2);
            }

            System.out.println("\nFarthest Points:");
            System.out.println("***************");
            System.out.println("All points farthest to each other at a maximum distance of " + maxDistance + " are:");
            for (Point.PointPair pair : farthestPairs) {
                System.out.println(pair.p1 + ", " + pair.p2);
            }

        } catch (Exception e) {
            System.out.println("Error processing file: " + e.getMessage());
        }
    }
}
