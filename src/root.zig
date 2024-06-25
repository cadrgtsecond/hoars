const std = @import("std");
const testing = std.testing;

const Token = union(enum) {
    word: []const u8,
    string: []const u8,
    number: i64,
};

fn consume_whitespace(input: *[]const u8) void {
    while (input.len > 0 and input.*[0] == ' ') {
        input.* = input.*[1..];
    }
}

/// Gets the next token. This may modify the input
fn next_token(input: *[]const u8) ?Token {
    consume_whitespace(input);
    if (input.len == 0) {
        return null;
    }
    if(input.*[0] == '\'' or input.*[0] == '"') {
      const next_quote = (std.mem.indexOf(u8, input.*[1..], input.*[0..1]) orelse input.len) + 1;
      return .{ .string = input.*[1..next_quote] };
    }
    const next_whitespace = std.mem.indexOfAny(u8, input.*, &std.ascii.whitespace) orelse input.len;
    defer input.* = input.*[next_whitespace..];
    if (input.len > 0 and std.ascii.isDigit(input.*[0])) {
        return .{ .number = std.fmt.parseInt(i64, input.*[0..next_whitespace], 10) catch {
            @panic("Invalid number");
        } };
    }
    return .{ .word = input.*[0..next_whitespace] };
}

const CodePtr = *const fn(*VM) void;
const Pointer = union {
  native_code: CodePtr,
  threaded_code: WordPtr,
};
const WordPtr = [*]const Pointer;
/// A virtual machine that can compile and run code
const VM = struct {
  const Dictionary = std.StringHashMap(WordPtr);
  run_dict: Dictionary,
  code_alloc: std.heap.ArenaAllocator,

  pub fn init(alloc: std.mem.Allocator) VM {
    var arena = std.heap.ArenaAllocator.init(alloc);
    return .{ .run_dict = Dictionary.init(arena.allocator()), .code_alloc = arena };
  }
  pub fn execute(vm: *VM, word: WordPtr) void {
    word[0].native_code(vm);
  }
  pub fn deinit(vm: VM) void {
    defer vm.code_alloc.deinit();
  }

  fn hello_world(vm: *VM) void {
    _ = vm;
    std.debug.print("Hello World", .{});
  }
  test "Basic execution" {
    var vm = VM.init(std.heap.page_allocator);
    const code = [_]Pointer{Pointer { .native_code = &hello_world }};
    vm.execute(&code);
    defer vm.deinit();
  }
};

test  {
  _ = VM;
}

test "Tokenizing strings" {
    var input: []const u8 = "let x be 10 then print x then print 'Hello world!'";
    try std.testing.expectEqualStrings("let", next_token(&input).?.word);
    try std.testing.expectEqualStrings("x", next_token(&input).?.word);
    try std.testing.expectEqualStrings("be", next_token(&input).?.word);
    try std.testing.expectEqual(@as(i64, 10), next_token(&input).?.number);
    try std.testing.expectEqualStrings("then", next_token(&input).?.word);
    try std.testing.expectEqualStrings("print", next_token(&input).?.word);
    try std.testing.expectEqualStrings("x", next_token(&input).?.word);
    try std.testing.expectEqualStrings("then", next_token(&input).?.word);
    try std.testing.expectEqualStrings("print", next_token(&input).?.word);
    try std.testing.expectEqualStrings("Hello world!", next_token(&input).?.string);
}
