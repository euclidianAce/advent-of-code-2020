
-- One indexing? In my coding problem?
-- It's more likely than you think

local function xor(a: boolean, b: boolean): boolean
   return (a and not b) or (not a and b)
end

local valid_p1, valid_p2 = 0, 0
for ln in io.lines("input.txt") do
   local sa, sb, letter, passwd = ln:match"^(%d+)%-(%d+) (%a): (.*)$"
   local a = tonumber(sa)
   local b = tonumber(sb)

   local _, num_letters = passwd:gsub(letter, "")
   if a <= num_letters and num_letters <= b then
      valid_p1 = valid_p1 + 1
   end

   if xor(passwd:sub(a, a) == letter, passwd:sub(b, b) == letter) then
      valid_p2 = valid_p2 + 1
   end
end
print(valid_p1, valid_p2)
