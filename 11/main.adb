
with ada.text_io, ada.strings.unbounded, ada.exceptions;

procedure main is
	package tio renames ada.text_io;
	package us renames ada.strings.unbounded;

	file_handle  : tio.file_type;
	file_name    : constant string := "input.txt";
	file_content : us.unbounded_string;
	
	type seat_cell is ('#', '.', 'L');
	type seat_grid is array (positive range <>, positive range <>) of seat_cell;
	line       : us.unbounded_string;
	rows, cols : natural := 0;
	last       : natural := 0;

	function init_grid(r, c : positive; s : us.unbounded_string) return seat_grid is
		grid : seat_grid (1 .. r, 1 .. c) := (others => (others => '.'));
	begin
		return grid;
	end init_grid;
begin
	tio.open(file_handle, tio.in_file, file_name);
	loop
		line := us.to_unbounded_string(tio.get_line(file_handle));
		us.append(file_content, line);
		cols := us.length(line);
		rows := rows + 1;
		exit when tio.end_of_file(file_handle);
	end loop;
	tio.close(file_handle);

	declare
		subtype row_type is natural range 1 .. rows;
		subtype col_type is natural range 1 .. cols;
		subtype this_grid is seat_grid (row_type, col_type);

		a, b : this_grid;

		function get_alive_neighbors(g : this_grid; row : row_type; col : col_type) return natural is
			n : natural := 0;
		begin
			for r in -1 .. 1 loop
				for c in -1 .. 1 loop
					if not (r = 0 and c = 0) then
						declare
							dist : positive := 1;
						begin
							blk:
							while (row + r * dist) in row_type
								and (col + c * dist) in col_type
							loop
								case g(row + r * dist, col + c * dist) is
									when '#' => n := n + 1; exit blk;
									when 'L' => exit blk;
									when '.' => dist := dist + 1;
								end case;
							end loop blk;
						end;
					end if;
				end loop;
			end loop;
			return n;
		end get_alive_neighbors;
		function update_grid (g : in this_grid) return this_grid is
			-- If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
			-- If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
			-- Otherwise, the seat's state does not change.
			new_grid : this_grid := g;
		begin
			for r in row_type loop
				for c in col_type loop
					case g(r, c) is
						when 'L' =>
							if get_alive_neighbors(g, r, c) = 0 then
								new_grid(r, c) := '#';
							end if;
						when '#' =>
							if get_alive_neighbors(g, r, c) >= 5 then
								new_grid(r, c) := 'L';
							end if;
						when '.' => null;
					end case;
				end loop;
			end loop;
			return new_grid;
		end update_grid;

		function "=" (g1, g2: in this_grid) return boolean is
		begin
			for r in row_type loop
				for c in col_type loop
					if g1(r, c) /= g2(r, c) then
						return false;
					end if;
				end loop;
			end loop;
			return true;
		end "=";

		r : row_type := 1;
		c : col_type := 1;
	begin
		for char of us.to_string(file_content) loop
			a(r, c) := seat_cell'value(char'image); -- char'image here feels hacky
			b(r, c) := seat_cell'value(char'image);
			if c = col_type'last then
				if r < row_type'last then
					r := r + 1;
				end if;
				c := 1;
			else
				c := c + 1;
			end if;
		end loop;

		declare
			alive : natural := 0;
		begin
			loop
				b := update_grid(a);
				exit when a = b;
				a := b;
			end loop;
			for r in row_type loop
				for c in col_type loop
					if a(r, c) = '#' then
						alive := alive + 1;
					end if;
				end loop;
			end loop;
			tio.put_line("Seats taken: " & alive'image);
		end;

	end;
end main;

