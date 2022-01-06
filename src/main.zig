const std = @import("std");
const zig = std.zig;
const Ast = zig.Ast;
const Node = Ast.Node;
const Token = Ast.TokenIndex;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub const ZigSource = struct {
    arena: std.heap.ArenaAllocator,

    structs: []const StructDecl,
    fns: []const FunDecl,

    pub fn deinit(self: *ZigSource) void {
        self.arena.deinit();
    }
};

pub const StructDecl = struct {
    name: []const u8,
    fields: []const StructField,
};

pub const StructField = struct {
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
};

pub fn parseZigSource(allocator: Allocator, source: [:0]const u8) !ZigSource {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();

    var tree = try zig.parse(allocator, source);
    defer tree.deinit(allocator);

    var structs = ArrayList(StructDecl).init(arena.allocator());
    var fns = ArrayList(FunDecl).init(arena.allocator());

    const root_decls = tree.rootDecls();
    const tags = tree.nodes.items(.tag);
    const datas = tree.nodes.items(.data);

    for (root_decls) |node| {
        const node_tag = tags[node];
        switch (node_tag) {
            .simple_var_decl => {
                const s = try parseVarDecl(arena.allocator(), tree, tree.simpleVarDecl(node));
                if (s) |s2| try structs.append(s2);
            },
            .fn_decl => {
                const fn_proto = datas[node].lhs;
                switch (tags[fn_proto]) {
                    .fn_proto => {
                        const fn_decl = try parseFnProto(arena.allocator(), tree, tree.fnProto(fn_proto));
                        if (fn_decl) |decl| try fns.append(decl);
                    },
                    .fn_proto_one => {
                        var buffer: [1]Node.Index = undefined;
                        const fn_decl = try parseFnProto(arena.allocator(), tree, tree.fnProtoOne(&buffer, fn_proto));
                        if (fn_decl) |decl| try fns.append(decl);
                    },
                    .fn_proto_simple => {
                        var buffer: [1]Node.Index = undefined;
                        const fn_decl = try parseFnProto(arena.allocator(), tree, tree.fnProtoSimple(&buffer, fn_proto));
                        if (fn_decl) |decl| try fns.append(decl);
                    },
                    .fn_proto_multi => {
                        const fn_decl = try parseFnProto(arena.allocator(), tree, tree.fnProtoMulti(fn_proto));
                        if (fn_decl) |decl| try fns.append(decl);
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
        .structs = structs.toOwnedSlice(),
        .fns = fns.toOwnedSlice(),
    };
}

fn parseFnProto(allocator: Allocator, tree: Ast, fn_proto: Ast.full.FnProto) !?FunDecl {
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
    return FunDecl{
        .name = name,
        .params = params.toOwnedSlice(),
        .return_type = return_type,
    };
}

fn parseVarDecl(allocator: Allocator, tree: Ast, decl: Ast.full.VarDecl) !?StructDecl {
    if (decl.ast.init_node == 0) return null;

    const node_tags = tree.nodes.items(.tag);
    const token_tags = tree.tokens.items(.tag);
    const main_tokens = tree.nodes.items(.main_token);

    const init_node_tag = node_tags[decl.ast.init_node];
    if (init_node_tag == .container_decl_two or init_node_tag == .container_decl_two_trailing) {
        var buffer: [2]Node.Index = undefined;
        const container_decl = tree.containerDeclTwo(&buffer, decl.ast.init_node);
        const main_token = main_tokens[container_decl.ast.main_token];
        const token_tag = token_tags[main_token];
        if (token_tag == .keyword_struct) {
            const struct_name = try allocator.dupe(u8, tree.tokenSlice(decl.ast.mut_token + 1));
            const struct_members = container_decl.ast.members;

            var struct_fields = ArrayList(StructField).init(allocator);
            errdefer struct_fields.deinit();

            for (struct_members) |member_node| {
                const member_tag = node_tags[member_node];
                if (member_tag != .container_field_init) continue;

                const container_field_init = tree.containerFieldInit(member_node);
                const type_main_token = main_tokens[container_field_init.ast.type_expr];
                const name_token = container_field_init.ast.name_token;

                const field_type = try allocator.dupe(u8, tree.tokenSlice(type_main_token));
                const field_name = try allocator.dupe(u8, tree.tokenSlice(name_token));
                try struct_fields.append(.{ .name = field_name, .type = field_type });
            }
            return StructDecl{
                .name = struct_name,
                .fields = struct_fields.toOwnedSlice(),
            };
        }
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
        \\    field1: i32,
        \\};
    ;

    var result = try parseZigSource(test_allocator, source);
    defer result.deinit();

    try expectEqual(result.structs.len, 1);

    const decl = result.structs[0];
    try expectEqualStrings("Test", decl.name);

    try expectEqual(decl.fields.len, 1);
    try expectEqualStrings("field1", decl.fields[0].name);
    try expectEqualStrings("i32", decl.fields[0].type);
}

test "fn decl simple" {
    const source =
        \\fn testfn(a: i32) u32 {
        \\    return a + 1;
        \\}
    ;

    var result = try parseZigSource(test_allocator, source);
    defer result.deinit();

    try expectEqual(result.fns.len, 1);

    const f = result.fns[0];
    try expectEqualStrings("testfn", f.name);
    try expectEqualStrings("u32", f.return_type);

    try expectEqual(f.params.len, 1);
    try expectEqualStrings("a", f.params[0].name);
    try expectEqualStrings("i32", f.params[0].type);
}

test "fn decl multi" {
    const source =
        \\fn testfn(a: i32, b: i32) u32 {
        \\    return a + b;
        \\}
    ;

    var result = try parseZigSource(test_allocator, source);
    defer result.deinit();

    try expectEqual(result.fns.len, 1);

    const f = result.fns[0];
    try expectEqualStrings("testfn", f.name);
    try expectEqualStrings("u32", f.return_type);

    try expectEqual(f.params.len, 2);
    try expectEqualStrings("a", f.params[0].name);
    try expectEqualStrings("i32", f.params[0].type);
    try expectEqualStrings("b", f.params[1].name);
    try expectEqualStrings("i32", f.params[1].type);
}
