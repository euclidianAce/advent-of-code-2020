
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Vectors;

procedure Expense_Report is

	package T_IO renames Ada.Text_IO;
	package I_IO renames Ada.Integer_Text_IO;

	package Pos_Vec is new Ada.Containers.Vectors(
		Element_Type => Natural,
		Index_Type   => Positive
	);

	File_Name   : constant String := "input.txt";
	File_Handle : T_IO.File_Type;
	V           : Pos_Vec.Vector;
	A           : Natural;

begin

	T_IO.Open(File_Handle, Ada.Text_IO.In_File, File_Name);
	while not Ada.Text_IO.End_Of_File(File_Handle) loop
		I_IO.Get(File_Handle, A);
		V.Append(A);
	end loop;
	T_IO.Close(File_Handle);

	Outer_1:
	for I in V.First_Index .. V.Last_Index - 1 loop
		for J in I + 1 .. V.Last_Index loop
			if V(I) + V(J) = 2020 then
				I_IO.Put(V(I));
				T_IO.Put(" + ");
				I_IO.Put(V(J));
				T_IO.Put(" = ");
				I_IO.Put(V(I) + V(J));
				T_IO.New_Line(1);
				I_IO.Put(V(I));
				T_IO.Put(" * ");
				I_IO.Put(V(J));
				T_IO.Put(" = ");
				I_IO.Put(V(I) * V(J));
				T_IO.New_Line(1);
				exit Outer_1;
			end if;
		end loop;
	end loop Outer_1;

	Outer_2:
	for I in V.First_Index .. V.Last_Index - 2 loop
		for J in I + 1 .. V.Last_Index - 1 loop
			for K in J + 1 .. V.Last_Index loop
				if V(I) + V(J) + V(K) = 2020 then
					I_IO.Put(V(I));
					T_IO.Put(" + ");
					I_IO.Put(V(J));
					T_IO.Put(" + ");
					I_IO.Put(V(K));
					T_IO.Put(" = ");
					I_IO.Put(V(I) + V(J) + V(K));
					T_IO.New_Line(1);

					I_IO.Put(V(I));
					T_IO.Put(" * ");
					I_IO.Put(V(J));
					T_IO.Put(" * ");
					I_IO.Put(V(K));
					T_IO.Put(" = ");
					I_IO.Put(V(I) * V(J) * V(K));
					T_IO.New_Line(1);
					exit Outer_2;
				end if;
			end loop;
		end loop;
	end loop Outer_2;

end Expense_Report;
