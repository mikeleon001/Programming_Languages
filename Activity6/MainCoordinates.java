/*********************************************************************************
 * Course: CMPS 3500
 * Activity 6
 * Date: 11/29/24
 * Username: mchitorog
 * Author: Mihail Chitorog
 *
 * File name: MainCoordinates.java
 * Works with Coordinates.java
 * 
 * Description:
 * Main driver program for testing point operations 
 * A coordinate system implementation supporting 2D and 3D point operations,
 * including distance calculations, point comparisons, and triangle area 
 * computations.Processes point data from input files to find closest/farthest 
 * points and largest triangles.
 *****************************************************************************/



import java.io.File;
import java.util.Scanner;
import java.util.ArrayList;
import java.util.List;

public class MainCoordinates {
    public static void main(String[] args) {
        System.out.println("\nTesting 2D Points:");
        System.out.println("******************\n");
        test2DPoints();

        System.out.println("\nTesting 3D Points:");
        System.out.println("******************");
        test3DPoints();
    }

    private static void test2DPoints() {
        // Create and test 2D points
        Coordinates.Point p1 = new Coordinates.Point(1, 2);
        Coordinates.Point p2 = new Coordinates.Point(3, 4);
        Coordinates.Point p3 = new Coordinates.Point(3, 4);
        Coordinates.Point p4 = p1;

        System.out.println("Objects (Points) created from Point class:");
        System.out.println("p1: " + p1);
        System.out.println("p2: " + p2);
        System.out.println("p3: " + p3);
        System.out.println("p4: " + p4);

        System.out.println("\nComparing Points:");
        System.out.println("p1 == p1? " + (p1 == p1));
        System.out.println("p1 == p2? " + (p1 == p2));
        System.out.println("p2 == p3? " + (p2 == p3));
        System.out.println("p1 == p4? " + (p1 == p4));

        double d12 = p1.distance(p2);
        System.out.println("\nThe distance between p1 and p2 is: " + d12 + "\n");

        System.out.println("\nSetting new coordinates for point 1:");
        p1.setX(-99);
        p1.setY(-1);
        System.out.println("p1: " + p1);

        System.out.println("\nChanging some values at the object level:");
        p1.setX(5);
        p1 = new Coordinates.Point(7, 8);

        System.out.println("p1: " + p1);
        System.out.println("p4: " + p4);
        System.out.println("p1.equals(p1)? " + p1.equals(p1));
        System.out.println("p1.equals(p2)? " + p1.equals(p2));
        System.out.println("p2.equals(p3)? " + p2.equals(p3));
        System.out.println("p1.equals(p4)? " + p1.equals(p4));

        // Process 2D points from file
        try {
            ArrayList<Coordinates.Point> points = new ArrayList<>();
            Scanner fileScanner = new Scanner(new File("2dinputpoint.txt"));
            while (fileScanner.hasNextLine()) {
                String line = fileScanner.nextLine();
                String[] coords = line.split(",");
                points.add(new Coordinates.Point(
                    Integer.parseInt(coords[0].trim()),
                    Integer.parseInt(coords[1].trim())
                ));
            }
            fileScanner.close();

            // Find closest and farthest points
            findClosestFarthest2DPoints(points);
            
            // Find largest triangle
            findLargestTriangle2D(points);

        } catch (Exception e) {
            System.out.println("Error processing 2D points: " + e.getMessage());
        }
    }

    private static void findClosestFarthest2DPoints(ArrayList<Coordinates.Point> points) {
        double minDistance = Double.MAX_VALUE;
        double maxDistance = 0;
        List<Coordinates.PointPair> closestPairs = new ArrayList<>();
        List<Coordinates.PointPair> farthestPairs = new ArrayList<>();

        for (int i = 0; i < points.size(); i++) {
            for (int j = i + 1; j < points.size(); j++) {
                Coordinates.Point p_i = points.get(i);
                Coordinates.Point p_j = points.get(j);
                double distance = p_i.distance(p_j);

                if (distance < minDistance) {
                    minDistance = distance;
                    closestPairs.clear();
                    closestPairs.add(new Coordinates.PointPair(p_i, p_j, distance));
                } else if (distance == minDistance) {
                    closestPairs.add(new Coordinates.PointPair(p_i, p_j, distance));
                }

                if (distance > maxDistance) {
                    maxDistance = distance;
                    farthestPairs.clear();
                    farthestPairs.add(new Coordinates.PointPair(p_i, p_j, distance));
                } else if (distance == maxDistance) {
                    farthestPairs.add(new Coordinates.PointPair(p_i, p_j, distance));
                }
            }
        }

        System.out.println("\nClosest Points:");
        System.out.println("---------------");
        System.out.println("All points closest to each other at a minimum distance of " + minDistance + " are:");
        for (Coordinates.PointPair pair : closestPairs) {
            System.out.println(pair.p1 + ", " + pair.p2);
        }

        System.out.println("\nFarthest Points:");
        System.out.println("---------------");
        System.out.println("All points farthest to each other at a maximum distance of " + maxDistance + " are:");
        for (Coordinates.PointPair pair : farthestPairs) {
            System.out.println(pair.p1 + ", " + pair.p2);
        }
    }

    private static void findLargestTriangle2D(ArrayList<Coordinates.Point> points) {
        double maxArea = 0;
        List<Coordinates.Triangle> largestTriangles = new ArrayList<>();

        for (int i = 0; i < points.size(); i++) {
            for (int j = i + 1; j < points.size(); j++) {
                for (int k = j + 1; k < points.size(); k++) {
                    Coordinates.Point p1 = points.get(i);
                    Coordinates.Point p2 = points.get(j);
                    Coordinates.Point p3 = points.get(k);
                    
                    double area = Coordinates.computeTriangleArea(p1, p2, p3);
                    
                    if (area > maxArea) {
                        maxArea = area;
                        largestTriangles.clear();
                        largestTriangles.add(new Coordinates.Triangle(p1, p2, p3, area));
                    } else if (area == maxArea) {
                        largestTriangles.add(new Coordinates.Triangle(p1, p2, p3, area));
                    }
                }
            }
        }

        System.out.println("\nLargest Triangle Area:");
        System.out.println("---------------------");
        System.out.println("All points from all triangles of maximum area of " + maxArea + " are:");
        for (Coordinates.Triangle triangle : largestTriangles) {
            System.out.println(triangle.p1 + ", " + triangle.p2 + ", " + triangle.p3);
        }
    }

    private static void test3DPoints() {
        // Create and test 3D points
        Coordinates.Point3D p1 = new Coordinates.Point3D(1, 2, 3);
        Coordinates.Point3D p2 = new Coordinates.Point3D(3, 4, 5);
        Coordinates.Point3D p3 = new Coordinates.Point3D(6, 7, 0);
        Coordinates.Point3D p4 = p1;

        System.out.println("Objects (Points) created from Point3D class:");
        System.out.println("p1: " + p1);
        System.out.println("p2: " + p2);
        System.out.println("p3: " + p3);
        System.out.println("p4: " + p4);

        System.out.println("\nComparing Point3Ds:");
        System.out.println("p1 == p1? " + (p1 == p1));
        System.out.println("p1 == p2? " + (p1 == p2));
        System.out.println("p2 == p3? " + (p2 == p3));
        System.out.println("p1 == p4? " + (p1 == p4));

        double d12 = p1.distance(p2);
        double area = Coordinates.computeTriangleArea(p1, p2, p3);
        System.out.println("\nThe distance between p1 and p2 is: " + d12);
        System.out.println("The area of the triangle formed by p1, p2, and p3 is: " + area);

        System.out.println("\nSetting new coordinates for Point3D 1:");
        p1.setX(-99);
        p1.setY(-1);
        p1.setZ(3);
        System.out.println("p1: " + p1);

        System.out.println("\nChanging some values at the object level:");
        p1 = new Coordinates.Point3D(10, 11, 12);
        p4.setX(5);
        p4.setY(-1);
        p4.setZ(3);

        System.out.println("p1: " + p1);
        System.out.println("p4: " + p4);
        System.out.println("p1.equals(p1)? " + p1.equals(p1));
        System.out.println("p1.equals(p2)? " + p1.equals(p2));
        System.out.println("p2.equals(p3)? " + p2.equals(p3));
        System.out.println("p1.equals(p4)? " + p1.equals(p4));

        // Process 3D points from file
        try {
            ArrayList<Coordinates.Point3D> points = new ArrayList<>();
            Scanner fileScanner = new Scanner(new File("3dinputpoint.txt"));
            while (fileScanner.hasNextLine()) {
                String line = fileScanner.nextLine();
                String[] coords = line.split(",");
                points.add(new Coordinates.Point3D(
                    Integer.parseInt(coords[0].trim()),
                    Integer.parseInt(coords[1].trim()),
                    Integer.parseInt(coords[2].trim())
                ));
            }
            fileScanner.close();

            // Find closest and farthest points
            findClosestFarthest3DPoints(points);
            
            // Find largest triangle
            findLargestTriangle3D(points);

        } catch (Exception e) {
            System.out.println("Error processing 3D points: " + e.getMessage());
        }
    }

    private static void findClosestFarthest3DPoints(ArrayList<Coordinates.Point3D> points) {
        double minDistance = Double.MAX_VALUE;
        double maxDistance = 0;
        List<Coordinates.Point3DPair> closestPairs = new ArrayList<>();
        List<Coordinates.Point3DPair> farthestPairs = new ArrayList<>();

        for (int i = 0; i < points.size(); i++) {
            for (int j = i + 1; j < points.size(); j++) {
                Coordinates.Point3D p_i = points.get(i);
                Coordinates.Point3D p_j = points.get(j);
                double distance = p_i.distance(p_j);

                if (distance < minDistance) {
                    minDistance = distance;
                    closestPairs.clear();
                    closestPairs.add(new Coordinates.Point3DPair(p_i, p_j, distance));
                } else if (distance == minDistance) {
                    closestPairs.add(new Coordinates.Point3DPair(p_i, p_j, distance));
                }

                if (distance > maxDistance) {
                    maxDistance = distance;
                    farthestPairs.clear();
                    farthestPairs.add(new Coordinates.Point3DPair(p_i, p_j, distance));
                } else if (distance == maxDistance) {
                    farthestPairs.add(new Coordinates.Point3DPair(p_i, p_j, distance));
                }
            }
        }

        System.out.println("\nClosest Points:");
        System.out.println("***************");
        System.out.println("All points closest to each other at a minimum distance of " + minDistance + " are:");
        for (Coordinates.Point3DPair pair : closestPairs) {
            System.out.println(pair.p1 + ", " + pair.p2);
        }

        System.out.println("\nFarthest Points:");
        System.out.println("***************");
        System.out.println("All points farthest to each other at a maximum distance of " + maxDistance + " are:");
        for (Coordinates.Point3DPair pair : farthestPairs) {
            System.out.println(pair.p1 + ", " + pair.p2);
        }
    }

    private static void findLargestTriangle3D(ArrayList<Coordinates.Point3D> points) {
        double maxArea = 0;
        List<Coordinates.Triangle3D> largestTriangles = new ArrayList<>();

        for (int i = 0; i < points.size(); i++) {
            for (int j = i + 1; j < points.size(); j++) {
                for (int k = j + 1; k < points.size(); k++) {
                    Coordinates.Point3D p1 = points.get(i);
                    Coordinates.Point3D p2 = points.get(j);
                    Coordinates.Point3D p3 = points.get(k);
                    
                    double area = Coordinates.computeTriangleArea(p1, p2, p3);
                    
                    if (area > maxArea) {
                        maxArea = area;
                        largestTriangles.clear();
                        largestTriangles.add(new Coordinates.Triangle3D(p1, p2, p3, area));
                    } else if (area == maxArea) {
                        largestTriangles.add(new Coordinates.Triangle3D(p1, p2, p3, area));
                    }
                }
            }
        }

        System.out.println("\nLargest Triangle Area:");
        System.out.println("---------------------");
        System.out.println("All points from all 3D triangles of maximum area of " + maxArea + " are:");
        for (Coordinates.Triangle3D triangle : largestTriangles) {
            System.out.println(triangle.p1 + ", " + triangle.p2 + ", " + triangle.p3);
        }
    }
}
