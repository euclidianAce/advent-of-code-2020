
with ada.text_io; use ada.text_io;
with ada.exceptions;

procedure main is
	file_name   : constant string := "input.txt";
	file_handle : file_type;
	subtype row is natural range 0 .. 127;
	subtype col is natural range 0 .. 7;

	row_lower_bound : row := row'first;
	row_upper_bound : row := row'last;
	col_lower_bound : col := col'first;
	col_upper_bound : col := col'last;

	function get_row_interval return row is
	begin
		return (row_upper_bound - row_lower_bound) / 2 + 1;
	end get_row_interval;
	function get_col_interval return col is
	begin
		return (col_upper_bound - col_lower_bound) / 2 + 1;
	end get_col_interval;

	line : string (1 .. 10);

	current_row : row := 0;
	current_col : col := 0;

	smallest_seat_id : natural := natural'last;
	largest_seat_id  : natural := 0;
	seat_id          : natural := 0;
	id_set           : array (0 .. 1000) of boolean;
begin
	open (file_handle, in_file, file_name);
	loop
		line := get_line (file_handle);
		for c of line loop
			case c is
				when 'F' => row_upper_bound := row_upper_bound - get_row_interval;
				when 'B' => row_lower_bound := row_lower_bound + get_row_interval;

				when 'L' => col_upper_bound := col_upper_bound - get_col_interval;
				when 'R' => col_lower_bound := col_lower_bound + get_col_interval;
				when others => raise constraint_error;
			end case;
		end loop;

		seat_id := natural (row_upper_bound * 8 + col_upper_bound);
		if seat_id > largest_seat_id then
			largest_seat_id := seat_id;
		end if;
		if seat_id < smallest_seat_id then
			smallest_seat_id := seat_id;
		end if;
		id_set (seat_id) := true;
		row_lower_bound := row'first;
		row_upper_bound := row'last;
		col_lower_bound := col'first;
		col_upper_bound := col'last;

		exit when end_of_file (file_handle);
	end loop;
	close (file_handle);

	put_line ("largest seat id: " & largest_seat_id'image);

	for id in smallest_seat_id .. largest_seat_id loop
		seat_id := id;
		exit when not id_set (id);
	end loop;

	put_line ("missing seat id: " & seat_id'image);
end main;
