
local fh <const> = io.open("input.txt", "r")
local fileContent <const> = fh:read("a") .. "\n"
fh:close()

local function inRange(n: number, min: number, max: number): boolean
      return min <= n and n <= max
end

local hclPatt <const> = "^#" .. ("[0-9a-f]"):rep(6) .. "$"
local validEyeColors <const>: {string:boolean} = {
   amb = true, blu = true, brn = true, gry = true,
   grn = true, hzl = true, oth = true,
}
local pitPatt <const> = "^" .. ("%d"):rep(9) .. "$"
local checkValidity: {string:function(string):boolean} = {
   byr = function(s: string): boolean
      local year = tonumber(s)
      return #s == 4 and inRange(year, 1920, 2002)
   end,
   iyr = function(s: string): boolean
      local year = tonumber(s)
      return #s == 4 and inRange(year, 2010, 2020)
   end,
   eyr = function(s: string): boolean
      local year = tonumber(s)
      return #s == 4 and inRange(year, 2020, 2030)
   end,
   hgt = function(s: string): boolean
      local ns, unit = s:match("^(%d+)(..)$")
      local n = tonumber(ns)
      if unit == "in" then
         return inRange(n, 59, 76)
      elseif unit == "cm" then
         return inRange(n, 150, 193)
      end
      return false
   end,
   hcl = function(s: string): boolean
      return s:match(hclPatt) as boolean
   end,
   ecl = function(s: string): boolean
      return validEyeColors[s]
   end,
   pid = function(s: string): boolean
      return s:match(pitPatt) as boolean
   end,
}

local numValid1, numValid2 = 0, 0
for passportTxt in fileContent:gmatch("(.-)\n\n") do
   local passport: {string:string} = {}
   for key, value in passportTxt:gmatch("(%a%a%a):(%S+)") do
      -- FIXME upstream: value is <unknown type> here?
      passport[key] = value as string
   end
   local valid1, valid2 = true, true
   for k, isValid in pairs(checkValidity) do
      if not passport[k] then
         valid1 = false
      end
      valid2 = valid2 and passport[k] and isValid(passport[k])
   end
   if valid1 then
      numValid1 = numValid1 + 1
   end
   if valid2 then
      numValid2 = numValid2 + 1
   end
end

print(numValid1, numValid2)
