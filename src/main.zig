const std = @import("std");
const zig = std.zig;
const Ast = zig.Ast;
const Node = Ast.Node;
const Token = Ast.TokenIndex;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub const ZigSource = struct {
    arena: std.heap.ArenaAllocator,

    decls: []const Decl,

    pub fn deinit(self: *ZigSource) void {
        self.arena.deinit();
    }
};

pub const Decl = union(enum) {
    Struct: ContainerDecl,
    Union: ContainerDecl,
    Fun: FunDecl,
};

pub const ContainerDecl = struct {
    name: []const u8,
    fields: []const ContainerField,
};

pub const ContainerField = struct {
    name: []const u8,
    type: []const u8,
};

pub const FunDecl = struct {
    const Param = struct {
        name: []const u8,
        type: []const u8,
    };

    name: []const u8,
    params: []const Param,
    return_type: []const u8,
    is_export: bool,
};

pub const ZigType = union {
    raw: []const u8,
    signed_integer: struct { size: usize },
    unsigned_integer: struct { size: usize },
    float: struct { size: usize },
    array: ArrayType,
    ptr: PtrType,
};

pub const ArrayType = struct {
    sentinel: ?[]const u8,
    elem_type: ZigType,
    elem_count: usize,
};

pub const PtrType = struct {
    size: std.builtin.TypeInfo.Pointer.Size,
};

pub fn parseZigSource(allocator: Allocator, source: [:0]const u8) !ZigSource {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();

    var tree = try zig.parse(allocator, source);
    defer tree.deinit(allocator);

    var decls = ArrayList(Decl).init(arena.allocator());
    errdefer decls.deinit();

    const root_decls = tree.rootDecls();
    const tags = tree.nodes.items(.tag);
    const datas = tree.nodes.items(.data);

    for (root_decls) |node| {
        const node_tag = tags[node];
        switch (node_tag) {
            .simple_var_decl => {
                const s = try parseVarDecl(arena.allocator(), tree, tree.simpleVarDecl(node));
                if (s) |s2| try decls.append(s2);
            },
            .fn_decl => {
                const fn_proto = datas[node].lhs;
                switch (tags[fn_proto]) {
                    .fn_proto => {
                        const fn_decl = try parseFnProto(arena.allocator(), tree, tree.fnProto(fn_proto));
                        if (fn_decl) |decl| try decls.append(decl);
                    },
                    .fn_proto_one => {
                        var buffer: [1]Node.Index = undefined;
                        const fn_decl = try parseFnProto(arena.allocator(), tree, tree.fnProtoOne(&buffer, fn_proto));
                        if (fn_decl) |decl| try decls.append(decl);
                    },
                    .fn_proto_simple => {
                        var buffer: [1]Node.Index = undefined;
                        const fn_decl = try parseFnProto(arena.allocator(), tree, tree.fnProtoSimple(&buffer, fn_proto));
                        if (fn_decl) |decl| try decls.append(decl);
                    },
                    .fn_proto_multi => {
                        const fn_decl = try parseFnProto(arena.allocator(), tree, tree.fnProtoMulti(fn_proto));
                        if (fn_decl) |decl| try decls.append(decl);
                    },
                    else => {
                        std.log.warn("Unknown fn_proto {}", .{tags[fn_proto]});
                    },
                }
            },
            else => {},
        }
    }

    return ZigSource{
        .arena = arena,
        .decls = decls.toOwnedSlice(),
    };
}

fn parseFnProto(allocator: Allocator, tree: Ast, fn_proto: Ast.full.FnProto) !?Decl {
    const token_tags = tree.tokens.items(.tag);

    const name = try allocator.dupe(u8, tree.tokenSlice(fn_proto.ast.fn_token + 1));

    var params = ArrayList(FunDecl.Param).init(allocator);
    errdefer params.deinit();

    var last_param = fn_proto.lparen;
    while (true) {
        last_param += 1;
        switch (token_tags[last_param]) {
            .r_paren => break,
            .identifier => {
                if (token_tags[last_param + 1] == .colon) {
                    const param_name = tree.tokenSlice(last_param);
                    const param_type = tree.tokenSlice(last_param + 2);
                    last_param += 2;

                    try params.append(.{ .name = param_name, .type = param_type });
                }
            },
            else => {},
        }
    }

    const return_token = tree.firstToken(fn_proto.ast.return_type);
    const return_type = try allocator.dupe(u8, tree.tokenSlice(return_token));

    // TODO May have multiple tokens here, so we should scan until we hit the fn token probably
    const is_export = if (fn_proto.extern_export_inline_token) |t| blk: {
        const t2 = token_tags[t];
        break :blk t2 == .keyword_export;
    } else false;

    return Decl{
        .Fun = .{
            .name = name,
            .params = params.toOwnedSlice(),
            .return_type = return_type,
            .is_export = is_export,
        },
    };
}

fn extractContainerData(allocator: Allocator, tree: Ast, decl: Ast.full.VarDecl, container_decl: Ast.full.ContainerDecl) !ContainerDecl {
    const node_tags = tree.nodes.items(.tag);
    const main_tokens = tree.nodes.items(.main_token);
    const container_name = try allocator.dupe(u8, tree.tokenSlice(decl.ast.mut_token + 1));
    const container_members = container_decl.ast.members;

    var container_fields = ArrayList(ContainerField).init(allocator);
    errdefer container_fields.deinit();

    for (container_members) |member_node| {
        const member_tag = node_tags[member_node];
        if (member_tag != .container_field_init) continue;

        const container_field_init = tree.containerFieldInit(member_node);
        const type_main_token = main_tokens[container_field_init.ast.type_expr];
        const name_token = container_field_init.ast.name_token;

        const field_type = try allocator.dupe(u8, tree.tokenSlice(type_main_token));
        const field_name = try allocator.dupe(u8, tree.tokenSlice(name_token));
        try container_fields.append(.{ .name = field_name, .type = field_type });
    }

    return ContainerDecl{
        .name = container_name,
        .fields = container_fields.toOwnedSlice(),
    };
}

fn parseContainerDecl(allocator: Allocator, tree: Ast, decl: Ast.full.VarDecl, container_decl: Ast.full.ContainerDecl) !?Decl {
    const token_tags = tree.tokens.items(.tag);
    const token_tag = token_tags[container_decl.ast.main_token];

    switch (token_tag) {
        .keyword_struct => {
            return Decl{ .Struct = try extractContainerData(allocator, tree, decl, container_decl) };
        },
        .keyword_union => {
            return Decl{ .Union = try extractContainerData(allocator, tree, decl, container_decl) };
        },
        else => {},
    }

    return null;
}

fn parseVarDecl(allocator: Allocator, tree: Ast, decl: Ast.full.VarDecl) !?Decl {
    if (decl.ast.init_node == 0) return null;

    const node_tags = tree.nodes.items(.tag);
    const init_node_tag = node_tags[decl.ast.init_node];

    switch (init_node_tag) {
        .container_decl,
        .container_decl_trailing,
        => {
            const container_decl = tree.containerDecl(decl.ast.init_node);
            return try parseContainerDecl(allocator, tree, decl, container_decl);
        },

        .container_decl_two,
        .container_decl_two_trailing,
        => {
            var buffer: [2]Node.Index = undefined;
            const container_decl = tree.containerDeclTwo(&buffer, decl.ast.init_node);
            return try parseContainerDecl(allocator, tree, decl, container_decl);
        },
        else => {},
    }

    return null;
}

// * Tests

const testing = std.testing;
const test_allocator = testing.allocator;
const expect = testing.expect;
const expectEqual = testing.expectEqual;
const expectEqualStrings = testing.expectEqualStrings;
const expectEqualSlices = testing.expectEqualSlices;

test "struct decl" {
    const source =
        \\const Test = struct {
        \\    a: i32,
        \\};
        \\
        \\const Test2 = struct {
        \\    a: i32,
        \\    b: i32,
        \\};
        \\
        \\const Test3 = struct {
        \\    a: i32,
        \\    b: i32,
        \\    c: i32,
        \\};
    ;

    var result = try parseZigSource(test_allocator, source);
    defer result.deinit();

    try expect(deepEql(
        result.decls,
        &.{
            .{
                .Struct = .{
                    .name = "Test",
                    .fields = &.{
                        .{ .name = "a", .type = "i32" },
                    },
                },
            },
            .{
                .Struct = .{
                    .name = "Test2",
                    .fields = &.{
                        .{ .name = "a", .type = "i32" },
                        .{ .name = "b", .type = "i32" },
                    },
                },
            },
            .{
                .Struct = .{
                    .name = "Test3",
                    .fields = &.{
                        .{ .name = "a", .type = "i32" },
                        .{ .name = "b", .type = "i32" },
                        .{ .name = "c", .type = "i32" },
                    },
                },
            },
        },
    ));
}

test "union decl" {
    const source =
        \\const Test = union {
        \\    a: i32,
        \\};
        \\
        \\const Test2 = union {
        \\    a: i32,
        \\    b: u32,
        \\};
        \\
        \\const Test3 = union {
        \\    a: i32,
        \\    b: u32,
        \\    c: u32,
        \\};
    ;

    var result = try parseZigSource(test_allocator, source);
    defer result.deinit();

    try expect(deepEql(
        result.decls,
        &.{
            .{
                .Union = .{
                    .name = "Test",
                    .fields = &.{
                        .{ .name = "a", .type = "i32" },
                    },
                },
            },
            .{
                .Union = .{
                    .name = "Test2",
                    .fields = &.{
                        .{ .name = "a", .type = "i32" },
                        .{ .name = "b", .type = "u32" },
                    },
                },
            },
            .{
                .Union = .{
                    .name = "Test3",
                    .fields = &.{
                        .{ .name = "a", .type = "i32" },
                        .{ .name = "b", .type = "u32" },
                        .{ .name = "c", .type = "u32" },
                    },
                },
            },
        },
    ));
}

test "fn decl simple" {
    const source =
        \\fn testfn(a: i32) u32 {
        \\    return a + 1;
        \\}
    ;

    var result = try parseZigSource(test_allocator, source);
    defer result.deinit();

    try expect(deepEql(
        result.decls,
        &.{
            .{
                .Fun = .{
                    .name = "testfn",
                    .params = &.{.{ .name = "a", .type = "i32" }},
                    .return_type = "u32",
                    .is_export = false,
                },
            },
        },
    ));
}

test "fn decl multi" {
    const source =
        \\fn testfn(a: i32, b: i32) u32 {
        \\    return a + b;
        \\}
    ;

    var result = try parseZigSource(test_allocator, source);
    defer result.deinit();

    try expect(deepEql(
        result.decls,
        &.{
            .{
                .Fun = .{
                    .name = "testfn",
                    .params = &.{
                        .{ .name = "a", .type = "i32" },
                        .{ .name = "b", .type = "i32" },
                    },
                    .return_type = "u32",
                    .is_export = false,
                },
            },
        },
    ));
}

test "fn decl export" {
    const source =
        \\export fn testfn(a: i32) u32 {
        \\    return a + 1;
        \\}
    ;

    var result = try parseZigSource(test_allocator, source);
    defer result.deinit();

    try expect(deepEql(
        result.decls,
        &.{
            .{
                .Fun = .{
                    .name = "testfn",
                    .params = &.{.{ .name = "a", .type = "i32" }},
                    .return_type = "u32",
                    .is_export = true,
                },
            },
        },
    ));
}

/// Like std.meta.eql, but follows pointers where possible.
fn deepEql(a: anytype, b: @TypeOf(a)) bool {
    const T = @TypeOf(a);

    switch (@typeInfo(T)) {
        .Pointer => |info| {
            return switch (info.size) {
                .One => deepEql(a.*, b.*),
                .Many, .C => a == b, // We don't know how many items are pointed to, so just compare addresses
                .Slice => {
                    if (a.len != b.len) return false;
                    for (a) |item, index| {
                        if (!deepEql(b[index], item)) {
                            // std.log.warn("not eql {} {}", .{b[index], item});
                            return false;
                        }
                    }
                    return true;
                },
            };
        },
        // The rest are copied from std.meta.eql (but calls to deepEql instead)
        .Struct => |info| {
            inline for (info.fields) |field_info| {
                if (!deepEql(@field(a, field_info.name), @field(b, field_info.name))) return false;
            }
            return true;
        },
        .ErrorUnion => {
            if (a) |a_p| {
                if (b) |b_p| return deepEql(a_p, b_p) else |_| return false;
            } else |a_e| {
                if (b) |_| return false else |b_e| return a_e == b_e;
            }
        },
        .Union => |info| {
            if (info.tag_type) |UnionTag| {
                const tag_a = std.meta.activeTag(a);
                const tag_b = std.meta.activeTag(b);
                if (tag_a != tag_b) return false;

                inline for (info.fields) |field_info| {
                    if (@field(UnionTag, field_info.name) == tag_a) {
                        return deepEql(@field(a, field_info.name), @field(b, field_info.name));
                    }
                }
                return false;
            }

            @compileError("cannot compare untagged union type " ++ @typeName(T));
        },
        .Array => {
            if (a.len != b.len) return false;
            for (a) |e, i|
                if (!deepEql(e, b[i])) return false;
            return true;
        },
        .Vector => |info| {
            var i: usize = 0;
            while (i < info.len) : (i += 1) {
                if (!deepEql(a[i], b[i])) return false;
            }
            return true;
        },
        .Optional => {
            if (a == null and b == null) return true;
            if (a == null or b == null) return false;
            return deepEql(a.?, b.?);
        },
        else => return a == b,
    }
}
