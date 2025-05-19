const std = @import("std");
const pos_tag = @import("main.zig").pos_tag;
const ner_tag = @import("main.zig").ner_tag;
const http = std.http;
const json = std.json;
const fs = std.fs;
const mem = std.mem;
const ArrayList = std.ArrayList;

const WordAnalysis = struct {
    pos_tag: []const u8,
    root_word: []const u8,
    ner: []const u8,
};

const OllamaResponse = struct {
    model: []const u8,
    created_at: []const u8,
    response: []const u8,
    @"error": ?[]const u8 = null,
    done: ?bool = null,
};

const WordPair = struct {
    bangla: []const u8,
    romanized: []const u8,
};

const BatchSize = 100;

pub fn main() !void {
    // Where we are going we need dynamic allocation
    const alloc = std.heap.page_allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    const allocator = arena.allocator();
    defer arena.deinit();

    // Open the input file
    const input_file = try fs.cwd().openFile("src/assets/romanized_words.csv", .{});
    defer input_file.close();

    // Check if output file exists and read processed words
    var processed_words = std.StringHashMap(void).init(allocator);
    defer processed_words.deinit();

    const output_file_path = "src/assets/posed_words.csv";
    if (fs.cwd().openFile(output_file_path, .{})) |output_file| {
        defer output_file.close();
        const file_content = try output_file.readToEndAlloc(allocator, std.math.maxInt(usize));
        defer allocator.free(file_content);

        var lines = mem.splitScalar(u8, file_content, '\n');
        _ = lines.next(); // Skip header
        while (lines.next()) |line| {
            if (line.len > 0) {
                var columns = mem.splitScalar(u8, line, ',');
                if (columns.next()) |bangla| {
                    try processed_words.put(bangla, {});
                }
            }
        }
    } else |err| {
        if (err != error.FileNotFound) {
            return err;
        }
    }

    // Open output file in append mode
    const output_file = fs.cwd().createFile(output_file_path, .{ .read = true }) catch |err| switch (err) {
        error.PathAlreadyExists => try fs.cwd().openFile(output_file_path, .{ .mode = .read_write }),
        else => return err,
    };
    defer output_file.close();

    // If file is empty, write header
    const file_size = try output_file.getEndPos();
    if (file_size == 0) {
        try output_file.writeAll("bangla,romanized,pos_tag,root_word,ner\n");
    }

    var words = ArrayList(WordPair).init(allocator);
    defer {
        for (words.items) |pair| {
            allocator.free(pair.bangla);
            allocator.free(pair.romanized);
        }
        words.deinit();
    }

    // Read words from file
    const file_content = try input_file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(file_content);

    var input_lines = mem.splitScalar(u8, file_content, '\n');
    while (input_lines.next()) |line| {
        if (line.len > 0) {
            var columns = mem.splitScalar(u8, line, ',');
            const bangla = columns.next() orelse continue;
            const romanized = columns.next() orelse continue;

            // Skip if already processed
            if (processed_words.contains(bangla)) {
                continue;
            }

            const word_pair = WordPair{
                .bangla = try allocator.dupe(u8, bangla),
                .romanized = try allocator.dupe(u8, romanized),
            };
            try words.append(word_pair);
        }
    }

    std.debug.print("Processing {d} remaining words in batches of {d}\n", .{ words.items.len, BatchSize });

    // Process words in batches
    var i: usize = 0;
    while (i < words.items.len) : (i += BatchSize) {
        const end = @min(i + BatchSize, words.items.len);
        const batch = words.items[i..end];

        std.debug.print("Processing batch {d}-{d}\n", .{ i, end });

        // Create prompt for this batch
        var prompt = ArrayList(u8).init(allocator);
        defer prompt.deinit();

        try prompt.appendSlice("For each of the following Bangla words, determine its Part-of-Speech (POS) tag. Use ONLY these exact tags: ADJ, ADP, ADV, AUX, CCONJ, DET, INTJ, NOUN, NUM, PART, PRON, PROPN, PUNCT, SCONJ, SYM, VERB, X. Return ONLY the word and its tag in CSV format, one per line. Do not include any explanations or additional text. Words:\n");

        for (batch) |pair| {
            try prompt.appendSlice(pair.bangla);
            try prompt.appendSlice("\n");
        }

        // Query Ollama API
        var client = std.http.Client{ .allocator = allocator };
        defer client.deinit();

        const request_body = try std.json.stringifyAlloc(allocator, .{
            .model = "gemma3",
            .prompt = prompt.items,
            .stream = false,
            .format = .{
                .type = "object",
                .properties = .{
                    .pos_tag = .{
                        .type = "string",
                        .@"enum" = &[_][]const u8{ "ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET", "INTJ", "NOUN", "NUM", "PART", "PRON", "PROPN", "PUNCT", "SCONJ", "SYM", "VERB", "X" },
                    },
                    .root_word = .{
                        .type = "string",
                    },
                    .ner = .{
                        .type = "string",
                        .@"enum" = &[_][]const u8{ "PERSON", "ORGANIZATION", "LOCATION", "DATE", "TIME", "MONEY", "QUANTITY", "PRODUCT", "EVENT", "WORK_OF_ART", "LAW", "LANGUAGE" },
                    },
                },
                .required = &[_][]const u8{ "pos_tag", "root_word", "ner" },
            },
        }, .{});
        defer allocator.free(request_body);

        var response_body = ArrayList(u8).init(allocator);
        defer response_body.deinit();

        const headers = &[_]std.http.Header{
            .{ .name = "Content-Type", .value = "application/json" },
        };

        const response = try client.fetch(.{
            .method = .POST,
            .location = .{ .url = "http://localhost:11434/api/generate" },
            .extra_headers = headers,
            .payload = request_body,
            .response_storage = .{ .dynamic = &response_body },
        });

        if (response.status != .ok) {
            std.debug.print("Error: HTTP status {d}\n", .{response.status});
            continue;
        }

        // Parse the structured response
        var parsed = std.json.parseFromSlice(OllamaResponse, allocator, response_body.items, .{ .ignore_unknown_fields = true }) catch |err| {
            std.debug.print("Error: Failed to parse JSON response: {}\n", .{err});
            continue;
        };
        defer parsed.deinit();

        if (parsed.value.@"error" != null) {
            std.debug.print("Error from API: {s}\n", .{parsed.value.@"error".?});
            continue;
        }

        // Parse the response field as JSON
        var content_parsed = std.json.parseFromSlice(WordAnalysis, allocator, parsed.value.response, .{ .ignore_unknown_fields = true }) catch |err| {
            std.debug.print("Error: Failed to parse content JSON: {}\n", .{err});
            continue;
        };
        defer content_parsed.deinit();

        // Process each word in the batch
        for (batch) |pair| {
            const analysis = content_parsed.value;

            // Validate the POS tag
            const valid_tags = [_][]const u8{ "ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET", "INTJ", "NOUN", "NUM", "PART", "PRON", "PROPN", "PUNCT", "SCONJ", "SYM", "VERB", "X" };

            const is_valid_tag = blk: {
                for (valid_tags) |valid_tag| {
                    if (std.mem.eql(u8, analysis.pos_tag, valid_tag)) {
                        break :blk true;
                    }
                }
                break :blk false;
            };

            if (!is_valid_tag) {
                std.debug.print("Warning: Invalid POS tag '{s}' for word '{s}'\n", .{ analysis.pos_tag, pair.bangla });
                continue;
            }

            // Validate the NER tag
            const valid_ner_tags = [_][]const u8{ "PERSON", "ORGANIZATION", "LOCATION", "DATE", "TIME", "MONEY", "QUANTITY", "PRODUCT", "EVENT", "WORK_OF_ART", "LAW", "LANGUAGE" };

            const is_valid_ner = blk: {
                for (valid_ner_tags) |valid_tag| {
                    if (std.mem.eql(u8, analysis.ner, valid_tag)) {
                        break :blk true;
                    }
                }
                break :blk false;
            };

            if (!is_valid_ner) {
                std.debug.print("Warning: Invalid NER tag '{s}' for word '{s}'\n", .{ analysis.ner, pair.bangla });
                continue;
            }

            // Write the original Bangla word, romanized word, POS tag, root word, and NER
            try output_file.writeAll(pair.bangla);
            try output_file.writeAll(",");
            try output_file.writeAll(pair.romanized);
            try output_file.writeAll(",");
            try output_file.writeAll(analysis.pos_tag);
            try output_file.writeAll(",");
            try output_file.writeAll(analysis.root_word);
            try output_file.writeAll(",");
            try output_file.writeAll(analysis.ner);
            try output_file.writeAll("\n");
        }
    }
}
