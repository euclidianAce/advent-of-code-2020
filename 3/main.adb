
with Ada.Text_IO;
with Ada.Exceptions;

procedure Main is
	package T_IO renames Ada.Text_IO;

	File_Name   : constant String := "input.txt";
	File_Handle : T_IO.File_Type;

	type Map_Item is ('.',  '#');
	-- TODO: Don't hard code these ranges
	subtype MapXRange is Integer range 0 .. 30;
	subtype MapYRange is Positive range 1 .. 323;
	type Map is array (MapXRange, MapYRange) of Map_Item;
	type Point is
		record
			X : Natural := 0;
			Y : Positive := 1;
		end record;
	function "+" (A, B : in Point) return Point is
	begin
		return (
			X => (A.X + B.X) mod (MapXRange'Last+1),
			Y => A.Y + B.Y
		);
	end "+";

	Pos : Point;
	Slopes : constant array (Natural range <>) of Point := (
		(X => 1, Y => 1),
		(X => 3, Y => 1),
		(X => 5, Y => 1),
		(X => 7, Y => 1),
		(X => 1, Y => 2)
	);

	M : Map;
	C : Character;
	X : Natural := 0;
	Y : Natural := 1;
	Current_Num_Trees : Long_Long_Integer := 0;
	Product           : Long_Long_Integer := 1;
begin
	-- Get the map
	T_IO.Open (File_Handle, T_IO.In_File, File_Name);
	loop
		T_IO.Get (File_Handle, C);
		if C = '.' then
			M (X, Y) := '.';
		elsif C = '#' then
			M (X, Y) := '#';
		else
			raise Constraint_Error;
		end if;
		if X = MapXRange'Last then
			Y := Y + 1;
			X := 0;
		else
			X := X + 1;
		end if;
		exit when Y > MapYRange'Last;
	end loop;
	T_IO.Close (File_Handle);

	-- Do that calculation thing
	for Vel of Slopes loop
		Current_Num_Trees := 0;
		Pos := (X => 0, Y => 1);
		loop
			exit when Pos.Y > MapYRange'Last;
			if M (Pos.X, Pos.Y) = '#' then
				Current_Num_Trees := Current_Num_Trees + 1;
			end if;
			Pos := Pos + Vel;
		end loop;
		T_IO.Put_Line("Found " & Current_Num_Trees'Image & " Trees for slope " & Vel.X'Image & ", " & Vel.Y'Image);
		Product := Product * Current_Num_Trees;
	end loop;

	T_IO.Put_Line (Product'Image);
end Main;
