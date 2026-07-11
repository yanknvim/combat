use Combat.{ParseResult, ParseError}
use Combat.Char
use Combat.String
use Combat.Combinator

@Test
def testAnyCharOk(): Unit \ Assert =
    let r = Combat.runParser(Char.anyChar(), "abc");
    Assert.assertEq(expected = ParseResult.Success('a'), r)

@Test
def testAnyCharEmpty(): Unit \ Assert =
    let r = Combat.runParser(Char.anyChar(), "");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

@Test
def testCharMismatch(): Unit \ Assert =
    // 'b' != 'a': recoverable raise -> Failure with UnexpectedChar
    let r = Combat.runParser(Char.char('a'), "bc");
    match r {
        case ParseResult.Failure(s) =>
            Assert.assertTrue(Set.memberOf(ParseError.UnexpectedChar('a', 'b'), s))
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

@Test
def testStringOk(): Unit \ Assert =
    let r = Combat.runParser(String.string("hello"), "hello world");
    Assert.assertEq(expected = ParseResult.Success("hello"), r)

@Test
def testStringMismatch(): Unit \ Assert =
    let r = Combat.runParser(String.string("foo"), "barbaz");
    match r {
        case ParseResult.Failure(s) =>
            Assert.assertTrue(Set.memberOf(ParseError.UnexpectedString("foo", "bar"), s))
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

// ── Combat.Combinator.choice tests ───────────────────────────────────────

@Test
def testChoiceFirstSucceeds(): Unit \ Assert =
    // p1 matches, return p1's result
    let p = Combinator.choice(Char.char('a'), Char.char('b'));
    let r = Combat.runParser(p, "abc");
    Assert.assertEq(expected = ParseResult.Success('a'), r)

@Test
def testChoiceFallbackNonFatal(): Unit \ Assert =
    // p1 fails (non-fatal UnexpectedChar), falls back to p2 which succeeds
    let p = Combinator.choice(Char.char('a'), Char.char('b'));
    let r = Combat.runParser(p, "bc");
    Assert.assertEq(expected = ParseResult.Success('b'), r)

@Test
def testChoiceFallbackFatal(): Unit \ Assert =
    // p1 fails (fatal EmptyString), falls back to p2 which also fails (fatal) → Failure
    let p = Combinator.choice(Char.anyChar(), Char.char('a'));
    let r = Combat.runParser(p, "");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

@Test
def testChoiceBothRaise(): Unit \ Assert =
    // p1 raises (non-fatal), falls back to p2 which also raises (non-fatal)
    // p2's error propagates to runParser
    let p = Combinator.choice(Char.char('a'), Char.char('b'));
    let r = Combat.runParser(p, "cd");
    match r {
        case ParseResult.Failure(s) =>
            Assert.assertTrue(Set.memberOf(ParseError.UnexpectedChar('b', 'c'), s))
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

@Test
def testChoiceStringFallback(): Unit \ Assert =
    // p1 fails (non-fatal), falls back to p2 which succeeds
    let p = Combinator.choice(String.string("foo"), String.string("bar"));
    let r = Combat.runParser(p, "bar");
    Assert.assertEq(expected = ParseResult.Success("bar"), r)

// ── Combat.Combinator.andThen tests ────────────────────────────────────────

@Test
def testAndThenTwoChars(): Unit \ Assert =
    // both match => Success((a, b))
    let p = Char.char('a') |> Combinator.andThen(Char.char('b'));
    let r = Combat.runParser(p, "abc");
    Assert.assertEq(expected = ParseResult.Success(('a', 'b')), r)

@Test
def testAndThenStringAndChar(): Unit \ Assert =
    // string then char, both match
    let p = String.string("hi") |> Combinator.andThen(Char.char('!'));
    let r = Combat.runParser(p, "hi!");
    Assert.assertEq(expected = ParseResult.Success(("hi", '!')), r)

@Test
def testAndThenP1Fatal(): Unit \ Assert =
    // p1 fatals on empty input => Failure
    let p = Char.char('a') |> Combinator.andThen(Char.char('b'));
    let r = Combat.runParser(p, "");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

@Test
def testAndThenP2Fatal(): Unit \ Assert =
    // p1 succeeds, p2 fatals on empty rest => Failure
    let p = Char.anyChar() |> Combinator.andThen(Char.char('b'));
    let r = Combat.runParser(p, "x");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

@Test
def testAndThenBothNonFatal(): Unit \ Assert =
    // both raise non-fatally (wrong chars), both errors collected
    let p = Char.char('a') |> Combinator.andThen(Char.char('b'));
    let r = Combat.runParser(p, "cd");
    match r {
        case ParseResult.Failure(s) =>
            Assert.assertTrue(Set.memberOf(ParseError.UnexpectedChar('a', 'c'), s));
            Assert.assertTrue(Set.memberOf(ParseError.UnexpectedChar('b', 'd'), s))
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

// ── Combat.map tests ──────────────────────────────────────────────────────

@Test
def testMapTransform(): Unit \ Assert =
    let p = Combat.map(c -> c :: Nil, Char.char('a'));
    let r = Combat.runParser(p, "abc");
    Assert.assertEq(expected = ParseResult.Success('a' :: Nil), r)

@Test
def testMapFailurePropagates(): Unit \ Assert =
    let p = Combat.map(c -> c :: Nil, Char.char('z'));
    let r = Combat.runParser(p, "abc");
    match r {
        case ParseResult.Failure(s) =>
            Assert.assertTrue(Set.memberOf(ParseError.UnexpectedChar('z', 'a'), s))
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

// ── Combat.pure tests ─────────────────────────────────────────────────────

@Test
def testPureAlwaysSucceeds(): Unit \ Assert =
    let p = Combat.pure(42);
    let r = Combat.runParser(p, "anything");
    Assert.assertEq(expected = ParseResult.Success(42), r)

@Test
def testPureDoesNotConsume(): Unit \ Assert =
    let p = Combat.pure(1) |> Combinator.andThen(Char.char('a'));
    let r = Combat.runParser(p, "abc");
    Assert.assertEq(expected = ParseResult.Success((1, 'a')), r)

// ── Combat.Combinator.many0 tests ─────────────────────────────────────────

@Test
def testMany0ZeroMatches(): Unit \ Assert =
    // zero occurrences => pure(Nil), input untouched
    let p = Combinator.many0(Char.char('a'));
    let r = Combat.runParser(p, "bbb");
    Assert.assertEq(expected = ParseResult.Success(Nil), r)

@Test
def testMany0MultipleMatches(): Unit \ Assert =
    let p = Combinator.many0(Char.char('a'));
    let r = Combat.runParser(p, "aaabbb");
    Assert.assertEq(expected = ParseResult.Success('a' :: 'a' :: 'a' :: Nil), r)

@Test
def testMany0EmptyInput(): Unit \ Assert =
    // empty input => zero matches, pure(Nil), succeeds
    let p = Combinator.many0(Char.char('a'));
    let r = Combat.runParser(p, "");
    Assert.assertEq(expected = ParseResult.Success(Nil), r)

// ── Complex integration test ─────────────────────────────────────────────

@Test
def testComplexMany0StringChoice(): Unit \ Assert =
    // Parse "hi" or "hello" repeatedly, then a trailing '!'
    // Exercises: many0 + choice + String + andThen + Char
    let greeting = Combinator.choice(String.string("hi"), String.string("hello"));
    let greetings = Combinator.many0(greeting);
    let p = greetings |> Combinator.andThen(Char.char('!'));
    let r = Combat.runParser(p, "hihello!");
    Assert.assertEq(expected = ParseResult.Success(("hi" :: "hello" :: Nil, '!')), r)

@Test
def testAndThenP2NonFatal(): Unit \ Assert =
    // p1 succeeds, p2 raises non-fatally => Failure with p2's error
    let p = Char.char('a') |> Combinator.andThen(Char.char('b'));
    let r = Combat.runParser(p, "ac");
    match r {
        case ParseResult.Failure(s) =>
            Assert.assertTrue(Set.memberOf(ParseError.UnexpectedChar('b', 'c'), s))
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

// ── Combat.Combinator.discardThen tests ───────────────────────────────────

@Test
def testDiscardThenBothOk(): Unit \ Assert =
    // p1's result discarded, p2's result kept
    let p = Char.char('a') |> Combinator.discardThen(Char.char('b'));
    let r = Combat.runParser(p, "abc");
    Assert.assertEq(expected = ParseResult.Success('b'), r)

@Test
def testDiscardThenP1Fails(): Unit \ Assert =
    // p1 fails (non-fatal) => overall Failure
    let p = Char.char('a') |> Combinator.discardThen(Char.char('b'));
    let r = Combat.runParser(p, "xbc");
    match r {
        case ParseResult.Failure(s) =>
            Assert.assertTrue(Set.memberOf(ParseError.UnexpectedChar('a', 'x'), s))
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

@Test
def testDiscardThenP2Fails(): Unit \ Assert =
    // p1 succeeds, p2 fails (non-fatal) => overall Failure
    let p = Char.char('a') |> Combinator.discardThen(Char.char('b'));
    let r = Combat.runParser(p, "axc");
    match r {
        case ParseResult.Failure(s) =>
            Assert.assertTrue(Set.memberOf(ParseError.UnexpectedChar('b', 'x'), s))
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

@Test
def testDiscardThenP1Fatal(): Unit \ Assert =
    let p = Char.anyChar() |> Combinator.discardThen(Char.char('b'));
    let r = Combat.runParser(p, "");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

@Test
def testDiscardThenP2Fatal(): Unit \ Assert =
    let p = Char.char('a') |> Combinator.discardThen(Char.anyChar());
    let r = Combat.runParser(p, "a");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

@Test
def testDiscardThenInputConsumed(): Unit \ Assert =
    // both match, input consumed by both
    let p = String.string("hi") |> Combinator.discardThen(Char.char('!'));
    let r = Combat.runParser(p, "hi!rest");
    Assert.assertEq(expected = ParseResult.Success('!'), r)

// ── Combat.Combinator.many1 tests ─────────────────────────────────────────

@Test
def testMany1SingleMatch(): Unit \ Assert =
    let p = Combinator.many1(Char.char('a'));
    let r = Combat.runParser(p, "abbb");
    Assert.assertEq(expected = ParseResult.Success('a' :: Nil), r)

@Test
def testMany1MultipleMatches(): Unit \ Assert =
    let p = Combinator.many1(Char.char('a'));
    let r = Combat.runParser(p, "aaabbb");
    Assert.assertEq(expected = ParseResult.Success('a' :: 'a' :: 'a' :: Nil), r)

@Test
def testMany1ZeroMatches(): Unit \ Assert =
    // zero occurrences => p raises (non-fatal), propagates as Failure
    let p = Combinator.many1(Char.char('a'));
    let r = Combat.runParser(p, "bbb");
    match r {
        case ParseResult.Failure(s) =>
            Assert.assertTrue(Set.memberOf(ParseError.UnexpectedChar('a', 'b'), s))
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

@Test
def testMany1EmptyInput(): Unit \ Assert =
    // empty input => p fatals, propagates as Failure
    let p = Combinator.many1(Char.char('a'));
    let r = Combat.runParser(p, "");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }