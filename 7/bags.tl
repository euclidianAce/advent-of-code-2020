
local rules: {string:{string:number}} = {}
for line in io.lines("input.txt") do
   local key, value = line:match("^(.-) bags contain (.*).$") as (string, string)
   rules[key] = {}
   for num, color in value:gmatch("(%d+) (.-) bag") as function(): (string, string) do
      rules[key][color] = tonumber(num)
   end
end

local function canContain(outerBag: string, innerBag: string): boolean
   local rule <const> = rules[outerBag]
   if rule[innerBag] then
      return true
   end
   for newOuterBag in pairs(rule) do
      if canContain(newOuterBag, innerBag) then
         return true
      end
   end

   return false
end

local function numContained(bag: string): number
   local rule <const> = rules[bag]
   local num = 0
   for newBag, n in pairs(rule) do
      num = num + n * (numContained(newBag) + 1)
   end

   return num
end

local numBags = 0
for bag, innerBags in pairs(rules) do
   if canContain(bag, "shiny gold") then
      numBags = numBags + 1
   end
end
print(numBags)
print(numContained("shiny gold"))
