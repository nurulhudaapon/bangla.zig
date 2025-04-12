const std = @import("std");
const TokenKind = @import("tokenization.zig").TokenKind;
const Token = @import("tokenization.zig").Token;
const DocumentNode = @import("tokenization.zig").DocumentNode;
const parseTextToAST = @import("tokenization.zig").parseTextToAST;
const printAST = @import("tokenization.zig").printAST;
const freeAST = @import("tokenization.zig").freeAST;

pub const TokenizerError = error{
    UnexpectedEndOfInput,
    UnexpectedToken,
};

pub const PrintFormat = enum {
    json,
    readable,
};

pub const Tokenizer = struct {
    input: []const u8,
    position: usize = 0,
    current_token: ?Token = null,
    allocator: std.mem.Allocator,
    ast: ?DocumentNode = null,

    pub fn init(input: []const u8, allocator: std.mem.Allocator) Tokenizer {
        return Tokenizer{
            .input = input,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Tokenizer) void {
        if (self.ast) |ast| {
            freeAST(ast, self.allocator);
            self.ast = null;
        }
    }

    pub fn parse(self: *Tokenizer) !DocumentNode {
        // Reset position and current token
        self.position = 0;
        self.current_token = null;

        // Parse the input into AST
        if (self.ast) |ast| {
            freeAST(ast, self.allocator);
        }
        self.ast = try parseTextToAST(self.input, self.allocator);

        // Get the first token for token-by-token processing
        self.current_token = try self.nextInternal();

        return self.ast.?;
    }

    pub fn print(self: *Tokenizer, format: PrintFormat) !void {
        if (self.ast == null) {
            _ = try self.parse();
        }

        switch (format) {
            .readable => try printAST(self.ast.?, self.allocator),
            .json => try printJSON(self.ast.?, self.allocator),
        }
    }

    fn escapeJsonString(str: []const u8, writer: anytype) !void {
        for (str) |c| {
            switch (c) {
                '"' => try writer.writeAll("\\\""),
                '\\' => try writer.writeAll("\\\\"),
                '\n' => try writer.writeAll("\\n"),
                '\r' => try writer.writeAll("\\r"),
                '\t' => try writer.writeAll("\\t"),
                else => try writer.writeByte(c),
            }
        }
    }

    fn printJSON(doc: DocumentNode, allocator: std.mem.Allocator) !void {
        var stdout = std.io.getStdOut().writer();
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();

        var json_str = std.ArrayList(u8).init(arena.allocator());
        try std.json.stringify(doc, .{ .whitespace = .indent_2 }, json_str.writer());
        try stdout.print("{s}\n", .{json_str.items});
    }

    pub fn next(self: *Tokenizer) !?Token {
        const current = self.current_token;
        self.current_token = try self.nextInternal();
        return current;
    }

    fn nextInternal(self: *Tokenizer) !?Token {
        // Skip whitespace
        while (self.position < self.input.len) {
            var iter = std.unicode.Utf8Iterator{ .bytes = self.input, .i = self.position };
            const codepoint_slice = iter.nextCodepointSlice() orelse break;
            const codepoint = std.unicode.utf8Decode(codepoint_slice) catch |err| {
                std.debug.print("Warning: UTF-8 decode error in tokenizer: {any}\n", .{err});
                self.position += codepoint_slice.len;
                continue;
            };

            if (getCharKind(codepoint) != .whitespace) {
                break;
            }
            self.position += codepoint_slice.len;
        }

        if (self.position >= self.input.len) {
            return null;
        }

        const start_pos = self.position;
        var iter = std.unicode.Utf8Iterator{ .bytes = self.input, .i = self.position };
        const first_codepoint_slice = iter.nextCodepointSlice() orelse return null;
        const first_codepoint = std.unicode.utf8Decode(first_codepoint_slice) catch |err| {
            std.debug.print("Warning: UTF-8 decode error in tokenizer: {any}\n", .{err});
            self.position += first_codepoint_slice.len;
            return Token{
                .kind = .foreign,
                .value = first_codepoint_slice,
            };
        };

        const kind = getCharKind(first_codepoint);
        var end_pos = start_pos + first_codepoint_slice.len;

        // For symbols, we only take one character
        if (kind == .symbol) {
            self.position = end_pos;
            return Token{
                .kind = kind,
                .value = self.input[start_pos..end_pos],
            };
        }

        // For other kinds, we collect all consecutive characters of the same kind
        while (end_pos < self.input.len) {
            var next_iter = std.unicode.Utf8Iterator{ .bytes = self.input, .i = end_pos };
            const next_slice = next_iter.nextCodepointSlice() orelse break;
            const next_codepoint = std.unicode.utf8Decode(next_slice) catch break;
            const next_kind = getCharKind(next_codepoint);

            if (next_kind == kind) {
                end_pos += next_slice.len;
            } else {
                break;
            }
        }

        self.position = end_pos;
        return Token{
            .kind = kind,
            .value = self.input[start_pos..end_pos],
        };
    }

    pub fn eat(self: *Tokenizer, expected_kind: TokenKind) !Token {
        if (self.current_token) |token| {
            if (token.kind == expected_kind) {
                // Consume the current token and get the next one
                self.current_token = try self.nextInternal();
                return token;
            } else {
                return TokenizerError.UnexpectedToken;
            }
        }
        return TokenizerError.UnexpectedEndOfInput;
    }

    pub fn peek(self: *Tokenizer) ?Token {
        return self.current_token;
    }

    // Helper function to classify codepoints
    fn getCharKind(codepoint: u21) TokenKind {
        return switch (codepoint) {
            // Bengali Letters (Vowels, Consonants, Signs contributing to words)
            0x0981...0x0983, // Candrabindu, Anusvara, Visarga
            0x0985...0x098C, // Vowels A-L
            0x098F...0x0990, // Vowels E, AI
            0x0993...0x09AE, // Vowels O, AU, Consonants Ka-Ma
            0x09AF...0x09B0, // Consonants Ya, Ra
            0x09B2, // Consonant La
            0x09B6...0x09B9, // Consonants Sha-Ha
            0x09BC...0x09C4, // Signs Nukta, Avagraha, Vowel Signs AA-VOCALIC_RR
            0x09C7...0x09C8, // Vowel Signs E, AI
            0x09CB...0x09CE, // Vowel Signs O, AU, Virama, Khanda Ta
            0x09D7, // AU Length Mark
            0x09DC...0x09DD, // Letters RRA, RHA
            0x09DF...0x09E3, // Letter YYA, Vocalic RR, LL, Vowel Signs VOCALIC L, LL
            0x09FA...0x09FB, // Isshar, Ganda Mark
            0x09FE, // Sandhi Mark
            => .word,

            // Bengali Digits
            0x09E6...0x09EF => .digit,

            // Common Symbols/Punctuation
            '(',
            ')',
            ',',
            ';',
            ':',
            '"',
            '\'',
            '[',
            ']',
            '{',
            '}',
            '/',
            '\\',
            '-',
            '+',
            '=',
            '*',
            '&',
            '^',
            '%',
            '$',
            '#',
            '@',
            '`',
            '~',
            '।', // Bengali full stop (Danda)
            '?',
            '!',
            // Bengali Currency Symbols etc.
            0x09F2...0x09F9,
            0x09FD, // Abbreviation Sign
            => .symbol,

            // ASCII Space and common whitespace
            ' ',
            '\t',
            '\r',
            '\n',
            => .whitespace,

            // Everything else is Foreign
            else => .foreign,
        };
    }
};

test "tokenizer parse and print" {
    const allocator = std.testing.allocator;
    const text =
        \\আমি ভাত খাই। তুমি কী করো?
        \\সে স্কুলে যায়!
    ;

    var tokenizer = Tokenizer.init(text, allocator);
    defer tokenizer.deinit();

    const ast = try tokenizer.parse();

    // Test printing in both formats
    try tokenizer.print(.readable);
    try tokenizer.print(.json);

    // Verify AST structure
    try std.testing.expectEqual(@as(usize, 2), ast.tokens.len);
    try std.testing.expectEqual(@as(usize, 2), ast.tokens[0].tokens.len);
    try std.testing.expectEqual(@as(usize, 1), ast.tokens[1].tokens.len);
}

test "tokenizer basic functionality" {
    const allocator = std.testing.allocator;
    const text = "আমি ভাত খাই।";

    var tokenizer = Tokenizer.init(text, allocator);
    defer tokenizer.deinit();
    _ = try tokenizer.parse();

    // Test first token
    const first = try tokenizer.next();
    try std.testing.expect(first != null);
    try std.testing.expectEqual(TokenKind.word, first.?.kind);
    try std.testing.expectEqualStrings("আমি", first.?.value);

    // Test second token
    const second = try tokenizer.next();
    try std.testing.expect(second != null);
    try std.testing.expectEqual(TokenKind.word, second.?.kind);
    try std.testing.expectEqualStrings("ভাত", second.?.value);

    // Test third token
    const third = try tokenizer.next();
    try std.testing.expect(third != null);
    try std.testing.expectEqual(TokenKind.word, third.?.kind);
    try std.testing.expectEqualStrings("খাই", third.?.value);

    // Test symbol token
    const symbol = try tokenizer.next();
    try std.testing.expect(symbol != null);
    try std.testing.expectEqual(TokenKind.symbol, symbol.?.kind);
    try std.testing.expectEqualStrings("।", symbol.?.value);

    // Test end of input
    const end = try tokenizer.next();
    try std.testing.expect(end == null);
}

test "tokenizer handles mixed content" {
    const allocator = std.testing.allocator;
    const text = "আমি ১২৩ খাই!";

    var tokenizer = Tokenizer.init(text, allocator);
    defer tokenizer.deinit();
    _ = try tokenizer.parse();

    // Test Word token
    const word_token = try tokenizer.eat(.word);
    try std.testing.expectEqualStrings("আমি", word_token.value);

    // Test Digit token
    const digit_token = try tokenizer.eat(.digit);
    try std.testing.expectEqualStrings("১২৩", digit_token.value);

    // Test Word token
    const word_token2 = try tokenizer.eat(.word);
    try std.testing.expectEqualStrings("খাই", word_token2.value);

    // Test Symbol token
    const symbol_token = try tokenizer.eat(.symbol);
    try std.testing.expectEqualStrings("!", symbol_token.value);

    // Test end of input
    try std.testing.expectError(error.UnexpectedEndOfInput, tokenizer.eat(.word));
}

test "tokenizer handles whitespace correctly" {
    const allocator = std.testing.allocator;
    const text = "  আমি  \t  ভাত  \n  খাই  ";

    var tokenizer = Tokenizer.init(text, allocator);
    defer tokenizer.deinit();
    _ = try tokenizer.parse();

    // Should skip initial whitespace and get first word
    const first = try tokenizer.next();
    try std.testing.expect(first != null);
    try std.testing.expectEqual(TokenKind.word, first.?.kind);
    try std.testing.expectEqualStrings("আমি", first.?.value);

    // Should skip whitespace and tab, get second word
    const second = try tokenizer.next();
    try std.testing.expect(second != null);
    try std.testing.expectEqual(TokenKind.word, second.?.kind);
    try std.testing.expectEqualStrings("ভাত", second.?.value);

    // Should skip whitespace and newline, get third word
    const third = try tokenizer.next();
    try std.testing.expect(third != null);
    try std.testing.expectEqual(TokenKind.word, third.?.kind);
    try std.testing.expectEqualStrings("খাই", third.?.value);

    // Should reach end after skipping trailing whitespace
    const end = try tokenizer.next();
    try std.testing.expect(end == null);
}

test "tokenizer peek method" {
    const allocator = std.testing.allocator;
    const text = "আমি ভাত";

    var tokenizer = Tokenizer.init(text, allocator);
    defer tokenizer.deinit();
    _ = try tokenizer.parse();

    // First peek should show first token
    const first_peek = tokenizer.peek();
    try std.testing.expect(first_peek != null);
    try std.testing.expectEqual(TokenKind.word, first_peek.?.kind);
    try std.testing.expectEqualStrings("আমি", first_peek.?.value);

    // After next(), peek should show second token
    _ = try tokenizer.next();
    const second_peek = tokenizer.peek();
    try std.testing.expect(second_peek != null);
    try std.testing.expectEqual(TokenKind.word, second_peek.?.kind);
    try std.testing.expectEqualStrings("ভাত", second_peek.?.value);
}
