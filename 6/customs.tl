
local groupSize = 0
local currentSet <const>: {string:number} = setmetatable({}, {
   __index = function(self: {string:number}, key: string): number
      rawset(self, key, 0)
      return 0
   end
})
local sum1, sum2 = 0, 0

local nextLine <const> = io.lines("input.txt")

repeat
   local line <const> = nextLine()
   if not line or #line == 0 then
      for letter, numAnswers in pairs(currentSet) do
         if numAnswers > 0 then
            sum1 = sum1 + 1
         end
         if numAnswers == groupSize then
            sum2 = sum2 + 1
         end
         currentSet[letter] = 0
      end
      groupSize = 0
   else
      for letter in line:gmatch("%a") do
         currentSet[letter] = currentSet[letter] + 1
      end
      groupSize = groupSize + 1
   end
until not line

print(sum1, sum2)

