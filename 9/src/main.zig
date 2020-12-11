const std = @import("std");
const print = std.debug.print;

const input = @embedFile("../input.txt");

const preambleSize: comptime usize = 25;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;

    const numLines: usize = blk: {
        var i: usize = 0;
        for (input) |c| {
            if (c == '\n') {
                i += 1;
            }
        }
        break :blk i;
    };

    var nums = try allocator.alloc(u64, numLines);
    defer allocator.free(nums);

    var it = std.mem.tokenize(input, "\n");

    var preamble: [preambleSize]u64 = [_]u64{0} ** preambleSize;
    var i: usize = 0;
    while (i < preambleSize) : (i += 1) {
        preamble[i] = try parseu64(it.next().?);
        nums[i] = preamble[i];
    }

    var found = false;
    var numberFound: u64 = undefined;
    while (it.next()) |line| : (i += 1) {
        const n = try parseu64(line);
        nums[i] = n;
        if (!found) {
            if (!isValid(n, preamble)) {
                print("Found number: {}\n", .{n});
                numberFound = n;
                found = true;
            }
            shift(&preamble, n);
        }
    }

    var len: usize = 2;
    var idx: usize = 0;
    loop: while (len < nums.len) : (len += 1) {
        idx = 0;
        while (idx < nums.len - len) : (idx += 1) {
            var sum: usize = 0;
            var currentIdx: usize = idx;
            while (currentIdx < idx + len) : (currentIdx += 1) {
                sum += nums[currentIdx];
            }
            if (sum == numberFound) {
                break :loop;
            }
        }
    }

    var smallest: u64 = nums[idx];
    var largest: u64 = nums[idx];
    for (nums[(idx + 1)..(idx + len)]) |n| {
        if (n > largest) {
            largest = n;
        }
        if (n < smallest) {
            smallest = n;
        }
    }

    print("encryption weakness: {}\n", .{smallest + largest});
}

fn parseu64(str: []const u8) !u64 {
    return std.fmt.parseUnsigned(u64, str, 10);
}

fn isValid(n: u64, preamble: [preambleSize]u64) bool {
    for (preamble[0..(preambleSize - 1)]) |a, idx| {
        for (preamble[(idx + 1)..preambleSize]) |b| {
            if (a != b and a + b == n) {
                return true;
            }
        }
    }
    return false;
}

// efficiency? who is she? i dont know her
fn shift(preamble: *[preambleSize]u64, nxt: u64) void {
    var i: usize = 1;
    while (i < preambleSize) : (i += 1) {
        preamble[i - 1] = preamble[i];
    }
    preamble[preambleSize - 1] = nxt;
}

fn dumpPreamble(preamble: *[preambleSize]u64, n: u64) void {
    for (preamble) |val| {
        print("{}, ", .{val});
    }
    print("-> {}\n", .{n});
}
