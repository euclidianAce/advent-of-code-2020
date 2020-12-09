
local enum OpCode
   "nop"
   "acc"
   "jmp"
end
-- Oh boy, tuples
local type Program = {{OpCode, number}}

local program1: Program = {}
local program2: Program = {}

for line in io.lines("input.txt") do
   local op, num = line:match("^(...) ([+-]%d+)$")
   table.insert(program1, {op as OpCode, assert(tonumber(num), num .. " not a number")})
   table.insert(program2, {op as OpCode, assert(tonumber(num), num .. " not a number")})
end

do
   local accumulator = 0
   local pc = 1
   local prog = program1

   while prog[pc] do
      local op, num = prog[pc][1], prog[pc][2]
      prog[pc] = nil
      if op == "nop" then
         -- :P
         pc = pc + 1
      elseif op == "acc" then
         accumulator = accumulator + num
         pc = pc + 1
      elseif op == "jmp" then
         pc = pc + num
      end
   end

   print(accumulator)
end

local function runProg(prog: Program, steps: number): number, boolean
   steps = steps or 10000
   local accumulator = 0
   local pc = 1
   local stepsTaken = 0

   while prog[pc] and stepsTaken <= steps do
      stepsTaken = stepsTaken + 1
      local op, num = prog[pc][1], prog[pc][2]
      if op == "nop" then
         pc = pc + 1
      elseif op == "acc" then
         accumulator = accumulator + num
         pc = pc + 1
      elseif op == "jmp" then
         pc = pc + num
      end
   end

   return accumulator, stepsTaken <= steps
end

-- just brute force it
local function swap(s: string): OpCode
   return s == "nop" and "jmp" or "nop"
end
local function substitutions(p: Program): function(): Program
   return coroutine.wrap(function()
      for i, step in ipairs(p) do
         local ins = step[1]
         if ins == "nop" or ins == "jmp" then
            p[i][1] = swap(ins)
            coroutine.yield(p)
            p[i][1] = ins
         end
      end
   end) as function(): Program
end

local steps <const> = 50000
local i = 0
for p in substitutions(program2) do
   i = i + 1
   local acc, halted = runProg(p, steps)
   if halted then
      print("halted with accumulator at " .. tostring(acc))
   end
end
print(tostring(i) .. " instruction swaps checked")
