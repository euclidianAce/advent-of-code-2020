
with Ada.Text_IO; use Ada.Text_Io;
with Ada.Exceptions;

procedure Main is
	File_Name   : constant String := "input.txt";
	File_Handle : File_Type;
	subtype Row is Natural range 0 .. 127;
	subtype Col is Natural range 0 .. 7;

	Row_Lower_Bound : Row := Row'First;
	Row_Upper_Bound : Row := Row'Last;
	Col_Lower_Bound : Col := Col'First;
	Col_Upper_Bound : Col := Col'Last;

	function Get_Row_Interval return Row is
	begin
		return (Row_Upper_Bound - Row_Lower_Bound) / 2 + 1;
	end Get_Row_Interval;
	function Get_Col_Interval return Col is
	begin
		return (Col_Upper_Bound - Col_Lower_Bound) / 2 + 1;
	end Get_Col_Interval;

	Line : String (1 .. 10);

	Current_Row : Row := 0;
	Current_Col : Col := 0;

	Smallest_Seat_ID : Natural := Natural'Last;
	Largest_Seat_ID  : Natural := 0;
	Seat_ID          : Natural := 0;
	ID_Set           : array (0 .. 1000) of Boolean;
begin
	Open (File_Handle, In_File, File_Name);
	loop
		Line := Get_Line (File_Handle);
		for C of Line loop
			case C is
				when 'F' => Row_Upper_Bound := Row_Upper_Bound - Get_Row_Interval;
				when 'B' => Row_Lower_Bound := Row_Lower_Bound + Get_Row_Interval;

				when 'L' => Col_Upper_Bound := Col_Upper_Bound - Get_Col_Interval;
				when 'R' => Col_Lower_Bound := Col_Lower_Bound + Get_Col_Interval;
				when others => raise Constraint_Error;
			end case;
		end loop;

		Seat_ID := Natural (Row_Upper_Bound * 8 + Col_Upper_Bound);
		if Seat_ID > Largest_Seat_ID then
			Largest_Seat_ID := Seat_ID;
		end if;
		if Seat_ID < Smallest_Seat_ID then
			Smallest_Seat_ID := Seat_ID;
		end if;
		ID_Set (Seat_ID) := True;
		Row_Lower_Bound := Row'First;
		Row_Upper_Bound := Row'Last;
		Col_Lower_Bound := Col'First;
		Col_Upper_Bound := Col'Last;

		exit when End_Of_File (File_Handle);
	end loop;
	Close (File_Handle);

	Put_Line ("Largest Seat ID: " & Largest_Seat_ID'Image);

	for ID in Smallest_Seat_ID .. Largest_Seat_ID loop
		Seat_ID := ID;
		exit when not ID_Set (ID);
	end loop;

	Put_Line ("Missing Seat ID: " & Seat_ID'Image);
end Main;
