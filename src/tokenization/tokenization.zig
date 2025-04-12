const std = @import("std");

pub const TokenKind = enum {
    Document,
    Paragraph,
    Sentence,
    Word, // Bengali words
    Symbol, // Punctuation, currency signs etc.
    Digit, // Bengali digits
    Foreign, // Non-Bengali characters/words
    Whitespace, // Added temporarily for logic, might remove later
};

pub const Token = struct {
    kind: TokenKind,
    value: []const u8,
};

pub const WordNode = struct {
    token: Token,
};

pub const SentenceNode = struct {
    token: Token,
    words: []WordNode,
};

pub const ParagraphNode = struct {
    token: Token,
    sentences: []SentenceNode,
};

pub const DocumentNode = struct {
    token: Token,
    paragraphs: []ParagraphNode,
};

// Define error for helper function
const TokenizeError = error{
    EmptySentence,
} || std.mem.Allocator.Error; // Include allocator errors

// Helper to classify codepoints for tokenization within sentences
// Note: Sentence terminators (।?!) are handled in parseTextToAST
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
        // Note: Assamese chars 0x09F0-0x09F1 are often included but treated as Foreign here unless specifically needed
        0x09FA...0x09FB, // Isshar, Ganda Mark
        0x09FE, // Sandhi Mark
        => .Word,

        // Bengali Digits
        0x09E6...0x09EF => .Digit,

        // Common Symbols/Punctuation (excluding sentence terminators)
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
        // Bengali Currency Symbols etc.
        0x09F2...0x09F9,
        0x09FD, // Abbreviation Sign
        => .Symbol,

        // ASCII Space and common whitespace
        ' ',
        '\t',
        '\r', // Exclude \n as it's handled in paragraph splitting
        => .Whitespace,

        // Everything else is Foreign
        else => .Foreign,
    };
}

// Helper function to tokenize words, digits, symbols, and foreign chars for a given sentence text
fn tokenizeWordsAndCreateNode(sentence_text: []const u8, allocator: std.mem.Allocator) !SentenceNode {
    const trimmed_sent = std.mem.trim(u8, sentence_text, " \n\r\t");
    if (trimmed_sent.len == 0) return error.EmptySentence;

    const sent_token = Token{ .kind = .Sentence, .value = trimmed_sent };
    var node_list = std.ArrayList(WordNode).init(allocator);
    var success = false;
    defer if (!success) node_list.deinit(); // Ensure deinit on error

    var cursor: usize = 0;
    while (cursor < trimmed_sent.len) {
        // 1. Decode the character at the cursor
        var iter = std.unicode.Utf8Iterator{ .bytes = trimmed_sent, .i = cursor };
        const codepoint_slice = iter.nextCodepointSlice() orelse break; // End of string
        const current_codepoint = std.unicode.utf8Decode(codepoint_slice) catch |err| {
            // Handle decoding error - treat the invalid sequence as a 'Foreign' token.
            std.debug.print("Warning: UTF-8 decode error in word tokenizer: {any}\n", .{err});
            // Assume the error is caused by the bytes in codepoint_slice (usually length 1 for initial errors)
            const token_text = codepoint_slice;
            if (token_text.len > 0) {
                const token = Token{ .kind = .Foreign, .value = token_text };
                try node_list.append(.{ .token = token });
            }
            cursor += token_text.len; // Advance past the invalid slice
            continue;
        };
        const current_kind = getCharKind(current_codepoint);

        // 2. Skip Whitespace
        if (current_kind == .Whitespace) {
            cursor += codepoint_slice.len;
            continue;
        }

        // 3. Tokenize based on kind
        const token_start = cursor;
        var token_end = cursor + codepoint_slice.len; // Start with the current char

        if (current_kind == .Symbol) {
            // Symbols are treated as single-character tokens
            const token = Token{ .kind = .Symbol, .value = codepoint_slice };
            try node_list.append(WordNode{ .token = token });
            cursor = token_end; // Move past this symbol
        } else {
            // Kind is Word, Digit, or Foreign - find the end of the sequence
            while (token_end < trimmed_sent.len) {
                var next_iter = std.unicode.Utf8Iterator{ .bytes = trimmed_sent, .i = token_end };
                const next_slice = next_iter.nextCodepointSlice() orelse break;
                const next_codepoint = std.unicode.utf8Decode(next_slice) catch break; // Stop if invalid UTF-8 found
                const next_kind = getCharKind(next_codepoint);

                if (next_kind == current_kind) {
                    // Continue sequence
                    token_end += next_slice.len;
                } else {
                    // Sequence ended
                    break;
                }
            }
            // Create token for the sequence
            const token_text = trimmed_sent[token_start..token_end];
            // We don't trim here because leading/trailing space handled by whitespace skipping
            if (token_text.len > 0) {
                // Use the kind of the first character for the whole sequence
                const token = Token{ .kind = current_kind, .value = token_text };
                try node_list.append(WordNode{ .token = token });
            }
            cursor = token_end; // Move past the sequence
        }
    }

    const nodes_slice = try node_list.toOwnedSlice();
    success = true; // Mark success before returning

    return SentenceNode{
        .token = sent_token,
        .words = nodes_slice, // Rename 'words' field later if confusing
    };
}

test "tokenize txt" {
    const allocator = std.testing.allocator; // Use testing allocator for tests

    const text =
        \\আমি ভাত খাই। (ব্রাস) ১টী তুমি কী sdf করো?
        \\সে স্কুলে যায়! আজ খুব গরম।।
        \\
        \\
        \\এইটা তৃতীয় অনুচ্ছেদ।
    ;

    const ast = try parseTextToAST(text, allocator);
    defer freeAST(ast, allocator);

    try printAST(ast, allocator);
}

fn parseTextToAST(text: []const u8, allocator: std.mem.Allocator) !DocumentNode {
    var para_list = std.ArrayList(ParagraphNode).init(allocator);
    defer para_list.deinit(); // Defer cleanup for paragraph list

    var para_iter = std.mem.tokenizeAny(u8, text, "\n");
    while (para_iter.next()) |para_text_untrimmed| {
        const para_text = std.mem.trim(u8, para_text_untrimmed, " \n\r\t");
        if (para_text.len == 0) continue; // Skip empty paragraphs

        const para_token = Token{ .kind = .Paragraph, .value = para_text }; // Store trimmed paragraph text
        var sentence_list = std.ArrayList(SentenceNode).init(allocator);
        // Defer sentence_list cleanup *inside* the loop if it doesn't get moved
        var para_success = false;
        defer if (!para_success) sentence_list.deinit();

        // --- Unicode-aware sentence tokenization ---
        var sentence_start_index: usize = 0;
        var iter = std.unicode.Utf8Iterator{ .bytes = para_text, .i = 0 };
        var current_index: usize = 0;

        while (iter.nextCodepointSlice()) |codepoint_slice| {
            // Decode the codepoint from the returned slice
            const codepoint = std.unicode.utf8Decode(codepoint_slice) catch |err| {
                // Handle potential decoding error, e.g., log and skip
                std.debug.print("Warning: UTF-8 decode error in sentence tokenizer: {}\n", .{err});
                // Advance current index even on error before continuing
                current_index += codepoint_slice.len;
                continue;
            };

            const is_delimiter = switch (codepoint) {
                0x0964, // । (Bengali Danda U+0964)
                '?',
                '!',
                => true,
                else => false,
            };

            if (is_delimiter) {
                const sent_text = para_text[sentence_start_index..current_index];
                // Use the helper function
                if (tokenizeWordsAndCreateNode(sent_text, allocator)) |node| {
                    try sentence_list.append(node);
                } else |err| {
                    // Propagate unexpected errors, ignore empty sentences
                    if (err != error.EmptySentence) return err;
                }
                // Move start index past the delimiter
                sentence_start_index = current_index + codepoint_slice.len;
            }
            // Advance current index by the number of bytes in the codepoint
            current_index += codepoint_slice.len;
        }

        // Handle the last sentence if the paragraph doesn't end with a delimiter
        if (sentence_start_index < para_text.len) {
            const sent_text = para_text[sentence_start_index..];
            // Use the helper function
            if (tokenizeWordsAndCreateNode(sent_text, allocator)) |node| {
                try sentence_list.append(node);
            } else |err| {
                // Propagate unexpected errors, ignore empty sentences
                if (err != error.EmptySentence) return err;
            }
        }
        // --- End Unicode-aware sentence tokenization ---

        // Append the paragraph node if sentences were successfully processed
        const sentences_slice = try sentence_list.toOwnedSlice();
        try para_list.append(ParagraphNode{
            .token = para_token,
            .sentences = sentences_slice,
        });
        para_success = true; // Mark success for this paragraph

    } // End paragraph loop

    const doc_token = Token{ .kind = .Document, .value = text };

    // Move para_list items to the final DocumentNode
    const paragraphs_slice = try para_list.toOwnedSlice();
    // Need to set para_list.items = &[_] {} before the outer defer runs if we manually move ownership.
    // Using toOwnedSlice is cleaner.

    return DocumentNode{
        .token = doc_token,
        .paragraphs = paragraphs_slice,
    };
}

fn freeAST(doc: DocumentNode, allocator: std.mem.Allocator) void {
    for (doc.paragraphs) |para| {
        for (para.sentences) |sent| {
            allocator.free(sent.words);
        }
        allocator.free(para.sentences);
    }
    allocator.free(doc.paragraphs);
}

fn printAST(doc: DocumentNode, allocator: std.mem.Allocator) !void {
    _ = allocator;

    var stdout_text = std.io.getStdOut().writer();
    try stdout_text.print("--- AST Structure ---\n", .{});

    for (doc.paragraphs, 0..) |para, i| {
        const para_text = para.token.value;
        const para_str = if (std.unicode.utf8ValidateSlice(para_text)) para_text else "(Invalid UTF-8)";
        try stdout_text.print("Paragraph {d} (Len: {d}): {s}\n", .{ i, para_text.len, para_str });

        for (para.sentences, 0..) |sent, j| {
            const sent_text = sent.token.value;
            const sent_str = if (std.unicode.utf8ValidateSlice(sent_text)) sent_text else "(Invalid UTF-8)";
            try stdout_text.print("  Sentence {d} (Len: {d}): {s}\n", .{ j, sent_text.len, sent_str });

            // Updated loop to print different token kinds
            for (sent.words, 0..) |node, k| { // 'sent.words' contains WordNode structs
                const token = node.token;
                const token_text = token.value;
                const token_kind_str = @tagName(token.kind);
                const word_str = if (std.unicode.utf8ValidateSlice(token_text)) token_text else "(Invalid UTF-8)";

                if (token_text.len == 0) {
                    try stdout_text.print("    {s} {d}: (Empty String)\n", .{ token_kind_str, k });
                } else {
                    try stdout_text.print("    {s} {d} (Len: {d}): {s}\n", .{ token_kind_str, k, token_text.len, word_str });
                }
            }
        }
    }
}
