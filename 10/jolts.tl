
local joltages <const>: {number} = { 0 }

for line in io.lines("input.txt") do
   table.insert(joltages, assert(tonumber(line)))
end

table.sort(joltages)
joltages[#joltages + 1] = joltages[#joltages] + 3

local diff1, diff3 = 0, 0
for i = 2, #joltages do
   local diff <const> = joltages[i] - joltages[i-1]
   assert(diff > 0, "non-positive diff")
   if diff == 3 then
      diff3 = diff3 + 1
   elseif diff == 1 then
      diff1 = diff1 + 1
   end
end

print(diff1, "*", diff3, "=", diff1 * diff3)

local adapters: {number:{number}} = {}
for i = 1, #joltages do
   local n = 1
   adapters[i] = {}
   while joltages[i + n] and joltages[i + n] - joltages[i] <= 3 do
      table.insert(adapters[i], i + n)
      n = n + 1
   end
end

local mem: {number:number} = {}
local function calcChain(n: number): number
   if not n then return 0 end
   if mem[n] then return mem[n] end

   local res: number = 0
   if #adapters[n] == 0 then
      res = 1
   else
      for i, v in ipairs(adapters[n]) do
         res = res + calcChain(v)
      end
   end
   mem[n] = res
   return res
end

print(calcChain(1))

