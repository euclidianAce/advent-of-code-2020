
local pos1: {number, number} = { 0, 0 }
local pos2: {number, number} = { 0, 0 }

local facing: {number, number} = setmetatable({ 1, 0 }, {
   __eq = function(a, b)
      return a[1] == b[1] and a[2] == b[2]
   end
})

local waypoint: {number, number} = { 10, 1 }

local function sin(deg: number): number
   deg = deg % 360
   if deg == 0 or deg == 180 then
      return 0
   elseif deg == 90 then
      return 1
   elseif deg == 270 then
      return -1
   end
end

local function cos(deg: number): number
   return sin(deg + 90)
end

--[[

   * \vec{v}
   ^
   |
   |
   | (theta \degree)
   * -------> *
  \vec{p}     \vec{w}
  given p, w, and |w-p| = |v-p|, find v

  R(theta) = {{ cos(theta), -sin(theta) },
             { -sin(theta), cos(theta) }}

  v = p + R(theta) * (w-p)

  v_x = p_x + cos(theta) * (w_x - p_x) - sin(theta) * (w_y - p_y)
  v_y = p_y - sin(theta) * (w_y - p_y) + cos(theta) * (w_y - p_y)

]]

local function rotateWaypoint(degrees: number)
   local c <const> = cos(degrees)
   local s <const> = sin(degrees)
   waypoint = {
      pos2[1] + c * (waypoint[1] - pos2[1]) - s * (waypoint[2] - pos2[2]),
      pos2[2] - s * (waypoint[1] - pos2[1]) + c * (waypoint[2] - pos2[2])
   }
end

local function rotate(widdershins: boolean)
   -- maually do a cross product here cuz floating point error with sin/cos accumulates too much here
   local factor <const> = widdershins and 1 or -1
   if facing == { 1, 0 } then
      facing[1] = 0
      facing[2] = factor
   elseif facing == { 0, 1 } then
      facing[1] = -factor
      facing[2] = 0
   elseif facing == { -1, 0 } then
      facing[1] = 0
      facing[2] = -factor
   elseif facing == { 0, -1 } then
      facing[1] = factor
      facing[2] = 0
   end
end

local example = {
"F10",
"N3",
"F7",
"R90",
"F11",
}

for _, line in ipairs(example) --[[io.lines("input.txt")]] do
   local instruction, _num = assert(line:match("^([NSEWLRF])(%d+)$"))
   local num = tonumber(_num)
   if instruction == "N" then
      pos1[2] = pos1[2] + num

      waypoint[2] = waypoint[2] + num
   elseif instruction == "S" then
      pos1[2] = pos1[2] - num

      waypoint[2] = waypoint[2] - num
   elseif instruction == "E" then
      pos1[1] = pos1[1] + num
      waypoint[1] = waypoint[1] + num
   elseif instruction == "W" then
      pos1[1] = pos1[1] - num
      waypoint[1] = waypoint[1] - num
   elseif instruction == "L" then
      for i = 1, num // 90 do
         rotate(true)
      end

      rotateWaypoint(num)
   elseif instruction == "R" then
      for i = 1, num // 90 do
         rotate(false)
      end

      rotateWaypoint(-num)
   elseif instruction == "F" then
      pos1[1] = pos1[1] + num * facing[1]
      pos1[2] = pos1[2] + num * facing[2]

      pos2[1] = waypoint[1] * num
      pos2[2] = waypoint[2] * num
   end
end

print(math.abs(pos1[1]) + math.abs(pos1[2]))
print(math.abs(pos2[1]) + math.abs(pos2[2]))
