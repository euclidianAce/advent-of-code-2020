
local nums: {number} = {}
for n in io.lines("input.txt") do
   table.insert(nums, assert(tonumber(n)))
end

local function printf(fmt: string, ...: any)
   print(string.format(fmt, ...))
end

-- generic nesting iterator, very necessary, definitely
local function nestedIter<T>(list: {T}, num: number): function(): T...
   local len <const> = #list
   local indexes = { 0 }
   for i = 2, num do
      indexes[i] = 1
   end
   return function(): T...
      local i = 1
      while true do
         indexes[i] = indexes[i] + 1
         if indexes[i] > len then
            indexes[i] = indexes[i+1]
            i = i + 1
            if i > num then
               return nil
            end
         else
            break
         end
      end
      local rets: {T} = {}
      for _, idx in ipairs(indexes) do
         table.insert(rets, list[idx])
      end
      return table.unpack(rets)
   end
end

for a, b in nestedIter(nums, 2) do
   if a + b == 2020 then
      printf("%d + %d = 2020", a, b)
      printf("%d * %d = %d", a, b, a * b)
      break
   end
end

for a, b, c in nestedIter(nums, 3) do
   if a + b + c == 2020 then
      printf("%d + %d + %d = 2020", a, b, c)
      printf("%d * %d * %d = %d", a, b, c, a * b * c)
      break
   end
end
