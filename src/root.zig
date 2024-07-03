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

fn docol(state: *State) void {
    const oldstate = state.curr_word;
    defer state.curr_word = oldstate;

    state.curr_word = state.curr_word[0].threaded_code[1..];
    state.execute_threaded();
}
fn hello_world(state: *State) void {
    _ = state.stdout.write("Hello world\n") catch {
        @panic("IO Error");
    };
}
fn push_int(state: *State) void {
    if(state.curr_word == state.vm.run_dict.get("push_int").?) {
      @panic("Some kind of error");
    }
    state.stack.appendAssumeCapacity(state.curr_word[1].number);
    state.curr_word += 1;
}

// ///////////////////////////////
// Virtual Machine
// ///////////////////////////////

const CodePtr = *const fn (*State) void;
const Pointer = union {
    native_code: CodePtr,
    threaded_code: WordPtr,
    number: usize,
};
const WordPtr = [*]const Pointer;

/// The state passed to each thread
const State = struct {
    const Stack = std.ArrayList(usize);
    const STACK_SIZE = 256;

    vm: *VM,
    /// A pointer to a pointer to the current code being executed
    curr_word: WordPtr,
    stdout: std.io.AnyWriter,
    stack: Stack,

    /// Creates a `State` that executes some threaded_code `word` as `curr_word`
    pub fn init(vm: *VM, word: WordPtr, heap_alloc: std.mem.Allocator) !State {
        return .{ .vm = vm, .curr_word = word, .stdout = std.io.getStdOut().writer().any(), .stack = try Stack.initCapacity(heap_alloc, STACK_SIZE) };
    }
    pub fn deinit(state: *State) void {
        state.stack.deinit();
    }
    /// Shortcut for `init` followed by `execute`
    pub fn init_execute(vm: *VM, word: WordPtr, heap_alloc: std.mem.Allocator) !State {
        var state = try State.init(vm, word, heap_alloc);
        state.execute_threaded();
        return state;
    }

    /// Runs `curr_word`
    pub fn execute_threaded(state: *State) void {
        state.curr_word[0].threaded_code[0].native_code(state);
    }
};

/// A virtual machine that can compile code
const VM = struct {
    const Dictionary = std.StringHashMapUnmanaged(WordPtr);
    run_dict: Dictionary,
    code_alloc: std.heap.ArenaAllocator,

    pub fn init(alloc: std.mem.Allocator) VM {
        return .{ .run_dict = Dictionary {}, .code_alloc = std.heap.ArenaAllocator.init(alloc) };
    }
    pub fn deinit(vm: VM) void {
        vm.code_alloc.deinit();
    }
    pub fn initialize_words(vm: *VM) !void {
        const alloc = vm.code_alloc.allocator();
        try vm.run_dict.put(alloc, "push_int", &[_]Pointer{.{ .native_code = &push_int }});
        try vm.run_dict.put(alloc, "hello_world", &[_]Pointer{.{ .native_code = &hello_world }});
    }

    const CompileError = error{expectedWord};
    pub fn compile_expr(vm: *VM, input: *[]const u8) !WordPtr {
        var compile_target = std.ArrayList(Pointer).init(vm.code_alloc.allocator());
        try compile_target.append(.{ .native_code = docol });

        switch (next_token(input) orelse return CompileError.expectedWord) {
            .word => {},
            .number => |n| {
                _ = n;
                try compile_target.append(.{ .threaded_code = vm.run_dict.get("push_int").? });
            },
            .string => {},
        }
        return compile_target.items.ptr;
    }

    test "Basic execution" {
        var vm = VM.init(std.heap.page_allocator);
        defer vm.deinit();
        try vm.initialize_words();

        var output = std.ArrayList(u8).init(std.testing.allocator);
        defer output.deinit();

        const code = [_]Pointer{ .{ .native_code = &docol }, .{ .threaded_code = vm.run_dict.get("hello_world").? } };
        var state = try State.init(&vm, &.{ .{ .threaded_code = &code } }, std.testing.allocator);
        defer state.deinit();
        state.stdout = output.writer().any();
        state.execute_threaded();

        try std.testing.expectEqualStrings("Hello world\n", output.items);
    }
    test "Pushing numbers" {
        var vm = VM.init(std.heap.page_allocator);
        defer vm.deinit();
        try vm.initialize_words();
        const code = [_]Pointer{ .{ .native_code = &docol }, .{ .threaded_code = vm.run_dict.get("push_int").? }, .{ .number = 10 } };

        var state = try State.init_execute(&vm, &.{ .{ .threaded_code = &code } }, std.testing.allocator);
        defer state.deinit();
        try std.testing.expectEqual(10, state.stack.pop());
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
