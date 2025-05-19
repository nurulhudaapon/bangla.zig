/// Part-of-Speech (POS) tags based on Universal Dependencies (UD) v2 specification
/// Source: https://universaldependencies.org/u/pos/
pub const pos_tag = enum {
    /// Adjectives are words that typically modify nouns
    /// Example: big, old, green (বড়/boro, পুরানো/purano, সবুজ/sobuj)
    ADJ,
    /// Adpositions are prepositions and postpositions
    /// Example: in, to, during (মধ্যে/moddhe, থেকে/theke, জন্য/jonno)
    ADP,
    /// Adverbs are words that typically modify verbs, adjectives or other adverbs
    /// Example: very, well, exactly (খুব/khub, ভালোভাবে/bhalobhabe, ঠিক/thik)
    ADV,
    /// Auxiliary verbs are used to form tenses, moods, etc.
    /// Example: is, have, will (আছে/ache, হবে/hobe)
    AUX,
    /// Coordinating conjunctions connect words, phrases, clauses of equal status
    /// Example: and, or, but (এবং/ebong, বা/ba, কিন্তু/kintu)
    CCONJ,
    /// Determiners are words that modify nouns or noun phrases
    /// Example: the, a, this (টি/ti, টা/ta, এই/ei)
    DET,
    /// Interjections are exclamatory words
    /// Example: oh, wow, hey (ওহ/oh, বাহ/bah, এই/ei)
    INTJ,
    /// Nouns are words denoting all physical objects and materials
    /// Example: cat, rock, intelligence (বিড়াল/biral, পাথর/pathor, বুদ্ধি/buddhi)
    NOUN,
    /// Numerals represent numbers, quantities, etc.
    /// Example: one, first, 1 (এক/ek, প্রথম/prothom, ১/1)
    NUM,
    /// Particles are function words that must be associated with another word
    /// Example: not, to (না/na, তো/to)
    PART,
    /// Pronouns substitute for nouns or noun phrases
    /// Example: I, you, he, she (আমি/ami, তুমি/tumi, সে/se)
    PRON,
    /// Proper nouns are names of specific persons, places, organizations
    /// Example: John, Dhaka, Google (জন/john, ঢাকা/dhaka, গুগল/google)
    PROPN,
    /// Punctuation marks
    /// Example: ., ?, !, , (।, ?, !, ,)
    PUNCT,
    /// Subordinating conjunctions link dependent clauses to independent ones
    /// Example: if, because, while (যদি/jodi, কারণ/karon, যখন/jokhon)
    SCONJ,
    /// Symbols represent currency, math operators, etc.
    /// Example: $, +, = (৳, +, =)
    SYM,
    /// Verbs denote actions and processes
    /// Example: run, eat, think (দৌড়ানো/dourano, খাওয়া/khawa, ভাবা/bhaba)
    VERB,
    /// Other words that don't fit into above categories
    /// Example: foreign words, abbreviations (etc/ইত্যাদি/ittyadi)
    X,
};

/// Named Entity Recognition (NER) tags based on CoNLL-2003 shared task specification
/// Source: https://www.clips.uantwerpen.be/conll2003/ner/
pub const ner_tag = enum {
    /// Names of persons (e.g., "John Smith", "আব্দুল করিম")
    PERSON,
    /// Names of organizations (e.g., "United Nations", "বাংলাদেশ সরকার")
    ORGANIZATION,
    /// Names of locations/geo-political entities (e.g., "Paris", "ঢাকা")
    LOCATION,
    /// Temporal expressions - dates (e.g., "June 1st, 2024", "১লা বৈশাখ")
    DATE,
    /// Temporal expressions - times (e.g., "3:30 PM", "সকাল ৯টা")
    TIME,
    /// Monetary values and quantities (e.g., "$100", "১০০ টাকা")
    MONEY,
    /// Percentages and other numerical quantities (e.g., "50%", "১০ কেজি")
    QUANTITY,
    /// Names of products and services (e.g., "iPhone", "পদ্মা সেতু")
    PRODUCT,
    /// Names of events (e.g., "World War II", "মুক্তিযুদ্ধ")
    EVENT,
    /// Works of art, books, songs etc. (e.g., "Hamlet", "পথের পাঁচালী")
    WORK_OF_ART,
    /// Laws, regulations, official documents (e.g., "Constitution", "সংবিধান")
    LAW,
    /// Languages and nationalities (e.g., "English", "বাঙালি")
    LANGUAGE,
};

const std = @import("std");
const fs = std.fs;
const mem = std.mem;
const ArrayList = std.ArrayList;
const HashMap = std.HashMap;
const bnlib = @import("bangla");

const WordPos = struct {
    bangla: []const u8,
    pos: pos_tag,
};

pub fn main() !void {
    const alloc = std.heap.page_allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    const allocator = arena.allocator();
    defer arena.deinit();

    // Read command line arguments
    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();
    const prog_name = args.next() orelse "pos_trainer"; // Get program name

    const sentence = args.next() orelse {
        std.debug.print("Usage: {s} \"<bangla_sentence>\"\n", .{prog_name});
        return;
    };

    if (sentence.len == 0) {
        std.debug.print("Error: Empty sentence provided\n", .{});
        return;
    }

    // Load the posed words into a hash map
    const posed_file = try fs.cwd().openFile("src/assets/posed_words.csv", .{});
    defer posed_file.close();

    const file_content = try posed_file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(file_content);

    var word_pos_map = HashMap([]const u8, pos_tag, std.hash_map.StringContext, 80).init(allocator);
    defer word_pos_map.deinit();

    var lines = mem.splitScalar(u8, file_content, '\n');
    _ = lines.next(); // Skip header
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        var columns = mem.splitScalar(u8, line, ',');
        const bangla = columns.next() orelse continue;
        _ = columns.next() orelse continue; // Skip romanized
        const pos_str = columns.next() orelse continue;

        // Clean up the POS tag
        var pos = pos_str;
        while (pos.len > 0 and (pos[0] == '"' or pos[0] == ' ')) {
            pos = pos[1..];
        }
        while (pos.len > 0 and (pos[pos.len - 1] == '"' or pos[pos.len - 1] == ' ')) {
            pos = pos[0 .. pos.len - 1];
        }

        // Convert string to enum using a switch
        const tag = switch (pos[0]) {
            'A' => if (std.mem.eql(u8, pos, "ADJ")) pos_tag.ADJ else if (std.mem.eql(u8, pos, "ADP")) pos_tag.ADP else if (std.mem.eql(u8, pos, "ADV")) pos_tag.ADV else if (std.mem.eql(u8, pos, "AUX")) pos_tag.AUX else pos_tag.X,
            'C' => if (std.mem.eql(u8, pos, "CCONJ")) pos_tag.CCONJ else pos_tag.X,
            'D' => if (std.mem.eql(u8, pos, "DET")) pos_tag.DET else pos_tag.X,
            'I' => if (std.mem.eql(u8, pos, "INTJ")) pos_tag.INTJ else pos_tag.X,
            'N' => if (std.mem.eql(u8, pos, "NOUN")) pos_tag.NOUN else if (std.mem.eql(u8, pos, "NUM")) pos_tag.NUM else pos_tag.X,
            'P' => if (std.mem.eql(u8, pos, "PART")) pos_tag.PART else if (std.mem.eql(u8, pos, "PRON")) pos_tag.PRON else if (std.mem.eql(u8, pos, "PROPN")) pos_tag.PROPN else if (std.mem.eql(u8, pos, "PUNCT")) pos_tag.PUNCT else pos_tag.X,
            'S' => if (std.mem.eql(u8, pos, "SCONJ")) pos_tag.SCONJ else if (std.mem.eql(u8, pos, "SYM")) pos_tag.SYM else pos_tag.X,
            'V' => if (std.mem.eql(u8, pos, "VERB")) pos_tag.VERB else pos_tag.X,
            'X' => pos_tag.X,
            else => pos_tag.X,
        };

        try word_pos_map.put(try allocator.dupe(u8, bangla), tag);
    }

    // Tokenize the sentence
    var tokenizer = bnlib.Tokenization.Tokenizer.init(sentence, allocator);
    defer tokenizer.deinit();

    const tokens = tokenizer.getWords(.{}) catch unreachable;

    // Look up POS tags and print results
    std.debug.print("[\n", .{});
    for (tokens, 0..) |token, idx| {
        const pos = word_pos_map.get(token) orelse pos_tag.X;
        std.debug.print("  [\"{s}\", \"{s}\"]", .{ token, @tagName(pos) });
        if (idx < tokens.len - 1) {
            std.debug.print(",\n", .{});
        } else {
            std.debug.print("\n", .{});
        }
    }
    std.debug.print("]\n", .{});
}
