
local nums: {number} = {}
for n in io.lines("input.txt") do
   table.insert(nums, assert(tonumber(n)))
end

local function printf(fmt: string, ...: any)
   print(string.format(fmt, ...))
end

for i = 1, #nums do
   for j = i+1, #nums do
      local a <const>, b <const> = nums[i], nums[j]
      if a + b == 2020 then
         printf("%d + %d = 2020\n%d * %d = %d", a, b, a, b, a * b)
      end
   end
end

for i = 1, #nums do
   for j = i, #nums do
      for k = j, #nums do
         local a <const>, b <const>, c <const> = nums[i], nums[j], nums[k]
         if a + b + c == 2020 then
            printf("%d + %d + %d = 2020\n%d * %d * %d = %d", a, b, c, a, b, c, a * b * c)
         end
      end
   end
end
