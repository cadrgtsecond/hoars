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
    if (input.*[0] == '\'' or input.*[0] == '"') {
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

// ///////////////////////////////
// Built in instructions
// ///////////////////////////////

fn docol(vm: *VM) void {
    vm.curr_word = vm.curr_word.? + 1;
    vm.execute(vm.curr_word.?[0].threaded_code);
}
fn hello_world(vm: *VM) void {
    _ = vm.stdout.write("Hello world\n") catch {
        @panic("IO Error");
    };
}

// ///////////////////////////////
// Virtual Machine
// ///////////////////////////////

const CodePtr = *const fn (*VM) void;
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
    curr_word: ?WordPtr,
    stdout: std.io.AnyWriter,

    pub fn init(alloc: std.mem.Allocator) VM {
        var arena = std.heap.ArenaAllocator.init(alloc);
        return .{ .run_dict = Dictionary.init(arena.allocator()), .code_alloc = arena, .curr_word = null, .stdout = std.io.getStdOut().writer().any() };
    }
    pub fn initialize_words(vm: *VM) !void {
        try vm.run_dict.put("hello_world", &[_]Pointer{.{ .native_code = &hello_world }});
    }
    pub fn execute(vm: *VM, word: WordPtr) void {
        vm.curr_word = word;
        word[0].native_code(vm);
    }
    pub fn deinit(vm: VM) void {
        defer vm.code_alloc.deinit();
    }

    pub fn compile_expr(vm: *VM, input: *[]const u8) !WordPtr {
        _ = input;
        var compile_target = std.ArrayList(Pointer).init(vm.code_alloc.allocator());
        try compile_target.append(.{ .native_code = docol });
        return compile_target.items.ptr;
    }

    test "Basic execution" {
        var vm = VM.init(std.heap.page_allocator);
        defer vm.deinit();
        try vm.initialize_words();

        var output = std.ArrayList(u8).init(std.testing.allocator);
        defer output.deinit();
        vm.stdout = output.writer().any();

        const code = [_]Pointer{ .{ .native_code = &docol }, .{ .threaded_code = vm.run_dict.get("hello_world").? } };
        vm.execute(&code);

        try std.testing.expectEqualStrings("Hello world\n", output.items);
    }
    // test "Basic compilation" {
    // var vm = VM.init(std.heap.page_allocator);
    // defer vm.deinit();

    // var code: []const u8 = "hello_world";
    // const word = try vm.compile_expr(&code);
    // try std.testing.expectEqualDeep(&[_]Pointer{.{ .native_code = &docol }}, word);
    // vm.execute(word);
    // }
};

test {
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
