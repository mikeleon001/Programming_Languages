-- NAME: Mihail Chitorog
-- ASGT: Activity 4
-- ORGN: CSUB - CMPS 3500
-- FILE: solverfile.adb
-- DATE: 10/26/2024

with Ada.Text_IO; 
use Ada.Text_IO;
with Ada.Float_Text_IO; 
use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; 
use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; 
use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; 
use Ada.Strings.Fixed;
with Ada.Strings; 
use Ada.Strings;
with Gnat.Regpat; 
use Gnat.Regpat;

procedure Solverfile is
   package Pat renames Gnat.Regpat;
   
   -- Constants
   N : constant Integer := 5;  -- Size of the matrix (5x5)
   
   -- Types
   type Matrix is array (1..N, 1..N) of Float;
   type Vector is array (1..N) of Float;
   
   -- Variables
   A : Matrix;  -- Coefficient matrix
   B : Vector;  -- Constants vector
   X : Vector;  -- Solution vector

   -- Forward declarations
   procedure Read_System(Filename: String);
   function Inverse(M : Matrix) return Matrix;
   function Determinant(M : Matrix; Size : Integer) return Float;
   function Adjoint(M : Matrix) return Matrix;
   function Cofactor(M : Matrix) return Matrix;
   
   -- Function to remove all spaces from a string
   function Remove_Spaces(S: String) return String is
      Result: String(1..S'Length);
      Length: Natural := 0;
   begin
      for I in S'Range loop
         if S(I) /= ' ' then
            Length := Length + 1;
            Result(Length) := S(I);
         end if;
      end loop;
      return Result(1..Length);
   end Remove_Spaces;

   -- Pattern matching procedure
   procedure Search_For_Pattern(Compiled_Expression: Pat.Pattern_Matcher;
                              Search_In: String;
                              First, Last: out Positive;
                              Found: out Boolean) is
      Result: Pat.Match_Array (0 .. 1);
   begin
      Pat.Match(Compiled_Expression, Search_In, Result);
      Found := not Pat."="(Result(1), Pat.No_Match);
      if Found then
         First := Result(1).First;
         Last := Result(1).Last;
      end if;
   end Search_For_Pattern;

   -- Function to get variable index
   function Get_Variable_Index(Var: Character) return Integer is
   begin
      case Var is
         when 'a' => return 1;
         when 'b' => return 2;
         when 'c' => return 3;
         when 'd' => return 4;
         when 'e' => return 5;
         when others => return 0;
      end case;
   end Get_Variable_Index;

   -- Matrix operations functions
   function Determinant(M : Matrix; Size : Integer) return Float is
      Det : Float := 0.0;
      Submatrix : Matrix;
      Sign : Float;
   begin
      if Size = 1 then
         return M(1, 1);
      end if;
      
      for Col in 1..Size loop
         -- Create submatrix
         for I in 2..Size loop
            for J in 1..Size loop
               if J < Col then
                  Submatrix(I-1, J) := M(I, J);
               elsif J > Col then
                  Submatrix(I-1, J-1) := M(I, J);
               end if;
            end loop;
         end loop;
         
         if Col mod 2 = 0 then
            Sign := -1.0;
         else
            Sign := 1.0;
         end if;
         
         Det := Det + Sign * M(1, Col) * Determinant(Submatrix, Size-1);
      end loop;
      
      return Det;
   end Determinant;

   -- Function to find cofactor matrix
   function Cofactor(M : Matrix) return Matrix is
      Result : Matrix;
      Submatrix : Matrix;
      Sign : Float;
   begin
      for I in 1..N loop
         for J in 1..N loop
            -- Create submatrix
            for Row in 1..N-1 loop
               for Col in 1..N-1 loop
                  if Row < I then
                     if Col < J then
                        Submatrix(Row, Col) := M(Row, Col);
                     else
                        Submatrix(Row, Col) := M(Row, Col+1);
                     end if;
                  else
                     if Col < J then
                        Submatrix(Row, Col) := M(Row+1, Col);
                     else
                        Submatrix(Row, Col) := M(Row+1, Col+1);
                     end if;
                  end if;
               end loop;
            end loop;
            
            if (I + J) mod 2 = 0 then
               Sign := 1.0;
            else
               Sign := -1.0;
            end if;
            
            Result(I, J) := Sign * Determinant(Submatrix, N-1);
         end loop;
      end loop;
      
      return Result;
   end Cofactor;

   -- Function to find adjoint matrix
   function Adjoint(M : Matrix) return Matrix is
      Cofactor_Matrix : Matrix := Cofactor(M);
      Result : Matrix;
   begin
      for I in 1..N loop
         for J in 1..N loop
            Result(I, J) := Cofactor_Matrix(J, I);
         end loop;
      end loop;
      return Result;
   end Adjoint;

   -- Function to find inverse matrix
   function Inverse(M : Matrix) return Matrix is
      Det : Float := Determinant(M, N);
      Adj : Matrix := Adjoint(M);
      Result : Matrix;
   begin
      for I in 1..N loop
         for J in 1..N loop
            Result(I, J) := Adj(I, J) / Det;
         end loop;
      end loop;
      return Result;
   end Inverse;

   -- Procedure to parse equation and update matrices
   procedure Parse_Equation(Equation: String; Row: Integer) is
      -- Patterns for matching
      Term_Pattern: constant String := "([+-]?\s*\d*[abcde])";
      Equals_Pattern: constant String := "=\s*([+-]?\d+)";
      
      Current_First: Positive := Equation'First;
      First, Last: Positive;
      Found: Boolean;
      
      -- Extract coefficient and variable from a term
      procedure Process_Term(Term: String) is
         Clean_Term: String := Remove_Spaces(Term);
         Coeff: Float := 1.0;
         Var_Index: Integer;
         Sign: Float := 1.0;
         Start_Idx: Integer := Clean_Term'First;
      begin
         -- Handle sign
         if Clean_Term(Clean_Term'First) = '-' then
            Sign := -1.0;
            Start_Idx := Clean_Term'First + 1;
         elsif Clean_Term(Clean_Term'First) = '+' then
            Start_Idx := Clean_Term'First + 1;
         end if;
         
         -- Find variable (last character)
         Var_Index := Get_Variable_Index(Clean_Term(Clean_Term'Last));
         
         -- Extract coefficient if present
         if Start_Idx < Clean_Term'Last then
            declare
               Num_Part: String := Clean_Term(Start_Idx .. Clean_Term'Last - 1);
            begin
               if Num_Part'Length > 0 then
                  Coeff := Float'Value(Num_Part);
               end if;
            end;
         end if;
         
         -- Update coefficient matrix
         A(Row, Var_Index) := Sign * Coeff;
      end Process_Term;
      
   begin
      -- First find all terms before equals sign
      while Current_First < Equation'Last loop
         Search_For_Pattern(Pat.Compile(Term_Pattern),
                          Equation(Current_First .. Equation'Last),
                          First, Last,
                          Found);
         exit when not Found;
         
         Process_Term(Equation(First .. Last));
         Current_First := Last + 1;
      end loop;
      
      -- Find constant after equals sign
      declare
         Equal_Pos : Integer := 1;
      begin
         -- Find position of equals sign
         for I in Equation'Range loop
            if Equation(I) = '=' then
               Equal_Pos := I;
               exit;
            end if;
         end loop;
         
         -- Get everything after equals sign and convert to float
         declare
            Value_Str : String := Remove_Spaces(Equation(Equal_Pos + 1 .. Equation'Last));
         begin
            B(Row) := Float'Value(Value_Str);
         end;
      end;
   end Parse_Equation;

   -- Procedure to read system from file
   procedure Read_System(Filename: String) is
      File: File_Type;
      Line: String(1..100);
      Last: Natural;
      Row: Integer := 1;
   begin
      -- Initialize matrices to 0
      for I in 1..N loop
         for J in 1..N loop
            A(I, J) := 0.0;
         end loop;
         B(I) := 0.0;
      end loop;
      
      Open(File, In_File, Filename);
      while not End_Of_File(File) and Row <= N loop
         Get_Line(File, Line, Last);
         Parse_Equation(Line(1..Last), Row);
         Row := Row + 1;
      end loop;
      Close(File);
   end Read_System;

   -- Krammer's Rule solver
   procedure KrammerSolver(A: in Matrix; B: in Vector) is
      D : Float;  -- Determinant of coefficient matrix
      Temp : Matrix;  -- For creating modified matrices
      Solutions : Vector;  -- To store solutions
   begin
      New_Line;
      Put_Line("The solution of the system using Kramer's Rule is:");
      New_Line;
      -- Calculate determinant of coefficient matrix
      D := Determinant(A, N);
      
      -- For each variable
      for I in 1..N loop
         -- Create matrix with one column replaced by constants
         Temp := A;
         for J in 1..N loop
            Temp(J, I) := B(J);
         end loop;
         
         -- Calculate determinant of modified matrix
         Solutions(I) := Determinant(Temp, N);
         
         -- Print result with fraction and decimal form
         case I is
            when 1 => Put("a = ");
            when 2 => Put("b = ");
            when 3 => Put("c = ");
            when 4 => Put("d = ");
            when 5 => Put("e = ");
         end case;
         
         Put(Solutions(I), Fore => 1, Aft => 4, Exp => 0);
         Put("/");
         Put(D, Fore => 1, Aft => 4, Exp => 0);
         Put(" = ");
         Put(Solutions(I)/D, Fore => 1, Aft => 4, Exp => 0);
         New_Line;
      end loop;
      New_Line;
   end KrammerSolver;

begin
   -- Main program
   Read_System("system1.txt");
   
   -- Print the matrices for verification
   Put_Line("The matrix of coefficients is:");
   New_Line;
   for I in 1..N loop
      for J in 1..N loop
         Put(A(I, J), Fore => 3, Aft => 4, Exp => 0);
      end loop;
      New_Line;
   end loop;
   New_Line(2);
   
   Put_Line("The inverse of the matrix of coefficients is:");
   New_Line;
   declare
      A_Inverse : Matrix := Inverse(A);
   begin
      for I in 1..N loop
         for J in 1..N loop
            Put(A_Inverse(I, J), Fore => 3, Aft => 4, Exp => 0);
         end loop;
         New_Line;
      end loop;
   end;
   New_Line(2);
   
   Put_Line("The solution of the system is:");
   New_Line;
   declare
      A_Inverse : Matrix := Inverse(A);
      Sum : Float;
   begin
      for I in 1..N loop
         Sum := 0.0;
         for J in 1..N loop
            Sum := Sum + A_Inverse(I, J) * B(J);
         end loop;
         X(I) := Sum;
      end loop;
      
      -- Output results with exactly 4 decimal places
      for I in 1..N loop
         case I is
            when 1 => Put("a");
            when 2 => Put("b");
            when 3 => Put("c");
            when 4 => Put("d");
            when 5 => Put("e");
            when others => null;
         end case;
         Put(" = ");
         Put(X(I), Fore => 1, Aft => 4, Exp => 0);
      New_Line;
      end loop;
   New_Line;
   end;
   
   -- Call Krammer's Rule solver
   KrammerSolver(A, B);
   New_Line;
end Solverfile;
