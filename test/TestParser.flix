use Parser.{ParseResult, ParseError}
use Parser.Char
use Parser.String
use Parser.Combinator

@Test
def testAnyCharOk(): Unit \ Assert =
    let r = Parser.runParser(Char.anyChar(), "abc");
    Assert.assertEq(expected = ParseResult.Success('a'), r)

@Test
def testAnyCharEmpty(): Unit \ Assert =
    let r = Parser.runParser(Char.anyChar(), "");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

@Test
def testCharMismatch(): Unit \ Assert =
    // 'b' != 'a': recoverable raise -> Failure with UnexpectedChar
    let r = Parser.runParser(Char.char('a'), "bc");
    match r {
        case ParseResult.Failure(s) =>
            Assert.assertTrue(Set.memberOf(ParseError.UnexpectedChar('a', 'b'), s))
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

@Test
def testStringOk(): Unit \ Assert =
    let r = Parser.runParser(String.string("hello"), "hello world");
    Assert.assertEq(expected = ParseResult.Success("hello"), r)

@Test
def testStringMismatch(): Unit \ Assert =
    let r = Parser.runParser(String.string("foo"), "barbaz");
    match r {
        case ParseResult.Failure(s) =>
            Assert.assertTrue(Set.memberOf(ParseError.UnexpectedString("foo", "bar"), s))
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

// ── Parser.Combinator.choice tests ──────────────────────────────────────────

@Test
def testChoiceFirstSucceeds(): Unit \ Assert =
    // p1 matches, return p1's result
    let p = Combinator.choice(Char.char('a'), Char.char('b'));
    let r = Parser.runParser(p, "abc");
    Assert.assertEq(expected = ParseResult.Success('a'), r)

@Test
def testChoiceFallbackNonFatal(): Unit \ Assert =
    // p1 fails (non-fatal UnexpectedChar), falls back to p2 which succeeds
    let p = Combinator.choice(Char.char('a'), Char.char('b'));
    let r = Parser.runParser(p, "bc");
    Assert.assertEq(expected = ParseResult.Success('b'), r)

@Test
def testChoiceFallbackFatal(): Unit \ Assert =
    // p1 fails (fatal EmptyString), falls back to p2 which also fails (fatal) → Failure
    let p = Combinator.choice(Char.anyChar(), Char.char('a'));
    let r = Parser.runParser(p, "");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

@Test
def testChoiceBothRaise(): Unit \ Assert =
    // p1 raises (non-fatal), falls back to p2 which also raises (non-fatal)
    // p2's error propagates to runParser
    let p = Combinator.choice(Char.char('a'), Char.char('b'));
    let r = Parser.runParser(p, "cd");
    match r {
        case ParseResult.Failure(s) =>
            Assert.assertTrue(Set.memberOf(ParseError.UnexpectedChar('b', 'c'), s))
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

@Test
def testChoiceStringFirstSucceeds(): Unit \ Assert =
    // p1 matches with string parser
    let p = Combinator.choice(String.string("hello"), String.string("world"));
    let r = Parser.runParser(p, "hello!");
    Assert.assertEq(expected = ParseResult.Success("hello"), r)

@Test
def testChoiceStringFallback(): Unit \ Assert =
    // p1 fails (non-fatal), falls back to p2 which succeeds
    let p = Combinator.choice(String.string("foo"), String.string("bar"));
    let r = Parser.runParser(p, "bar");
    Assert.assertEq(expected = ParseResult.Success("bar"), r)

@Test
def testChoiceStringFallbackShorter(): Unit \ Assert =
    // p1 fails (non-fatal), falls back to p2 which matches a shorter string
    let p = Combinator.choice(String.string("ab"), String.string("a"));
    let r = Parser.runParser(p, "a");
    Assert.assertEq(expected = ParseResult.Success("a"), r)

// ── Parser.Combinator.andThen tests ─────────────────────────────────────────

@Test
def testAndThenTwoChars(): Unit \ Assert =
    // both match => Success((a, b))
    let p = Combinator.andThen(Char.char('a'), Char.char('b'));
    let r = Parser.runParser(p, "abc");
    Assert.assertEq(expected = ParseResult.Success(('a', 'b')), r)

@Test
def testAndThenCharAndAnyChar(): Unit \ Assert =
    // first matches 'a', second matches any remaining char
    let p = Combinator.andThen(Char.char('a'), Char.anyChar());
    let r = Parser.runParser(p, "ab");
    Assert.assertEq(expected = ParseResult.Success(('a', 'b')), r)

@Test
def testAndThenStringAndChar(): Unit \ Assert =
    // string then char, both match
    let p = Combinator.andThen(String.string("hi"), Char.char('!'));
    let r = Parser.runParser(p, "hi!");
    Assert.assertEq(expected = ParseResult.Success(("hi", '!')), r)

@Test
def testAndThenP1Fatal(): Unit \ Assert =
    // p1 fatals on empty input => Failure
    let p = Combinator.andThen(Char.char('a'), Char.char('b'));
    let r = Parser.runParser(p, "");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

@Test
def testAndThenP2Fatal(): Unit \ Assert =
    // p1 succeeds, p2 fatals on empty rest => Failure
    let p = Combinator.andThen(Char.anyChar(), Char.char('b'));
    let r = Parser.runParser(p, "x");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

@Test
def testAndThenBothNonFatal(): Unit \ Assert =
    // both raise non-fatally (wrong chars), both errors collected
    let p = Combinator.andThen(Char.char('a'), Char.char('b'));
    let r = Parser.runParser(p, "cd");
    match r {
        case ParseResult.Failure(s) =>
            Assert.assertTrue(Set.memberOf(ParseError.UnexpectedChar('a', 'c'), s));
            Assert.assertTrue(Set.memberOf(ParseError.UnexpectedChar('b', 'd'), s))
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

@Test
def testAndThenP2NonFatal(): Unit \ Assert =
    // p1 succeeds, p2 raises non-fatally => Failure with p2's error
    let p = Combinator.andThen(Char.char('a'), Char.char('b'));
    let r = Parser.runParser(p, "ac");
    match r {
        case ParseResult.Failure(s) =>
            Assert.assertTrue(Set.memberOf(ParseError.UnexpectedChar('b', 'c'), s))
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }