use Combat.{ParseResult, ParseError}
use Combat.Char
use Combat.String
use Combat.Combinator
use Combat.Literal

def hasUnexpectedChar(errs: Set[ParseError], expected: Char, actual: Char): Bool =
    Set.exists(err -> match err {
        case ParseError.UnexpectedChar(_, e, a) => e == expected and a == actual
        case _ => false
    }, errs)

def hasUnexpectedString(errs: Set[ParseError], expected: String, actual: String): Bool =
    Set.exists(err -> match err {
        case ParseError.UnexpectedString(_, e, a) => e == expected and a == actual
        case _ => false
    }, errs)

def hasEmptyString(errs: Set[ParseError]): Bool =
    Set.exists(err -> match err {
        case ParseError.EmptyString(_) => true
        case _ => false
    }, errs)

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
            Assert.assertTrue(hasUnexpectedChar(s, 'a', 'b'))
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
            Assert.assertTrue(hasUnexpectedString(s, "foo", "bar"))
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
            Assert.assertTrue(hasUnexpectedChar(s, 'b', 'c'))
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
            Assert.assertTrue(hasUnexpectedChar(s, 'a', 'c'));
            Assert.assertTrue(hasUnexpectedChar(s, 'b', 'd'))
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
            Assert.assertTrue(hasUnexpectedChar(s, 'z', 'a'))
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
            Assert.assertTrue(hasUnexpectedChar(s, 'b', 'c'))
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
            Assert.assertTrue(hasUnexpectedChar(s, 'a', 'x'))
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

@Test
def testDiscardThenP2Fails(): Unit \ Assert =
    // p1 succeeds, p2 fails (non-fatal) => overall Failure
    let p = Char.char('a') |> Combinator.discardThen(Char.char('b'));
    let r = Combat.runParser(p, "axc");
    match r {
        case ParseResult.Failure(s) =>
            Assert.assertTrue(hasUnexpectedChar(s, 'b', 'x'))
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
            Assert.assertTrue(hasUnexpectedChar(s, 'a', 'b'))
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

// ── Combat.Combinator.anyOf tests ───────────────────────────────────────

@Test
def testAnyOfFirstSucceeds(): Unit \ Assert =
    let p = Combinator.anyOf(Char.char('a') :: Char.char('b') :: Nil);
    let r = Combat.runParser(p, "abc");
    Assert.assertEq(expected = ParseResult.Success('a'), r)

@Test
def testAnyOfSecondSucceeds(): Unit \ Assert =
    // first fails (non-fatal), second succeeds
    let p = Combinator.anyOf(Char.char('a') :: Char.char('b') :: Nil);
    let r = Combat.runParser(p, "bcd");
    Assert.assertEq(expected = ParseResult.Success('b'), r)

@Test
def testAnyOfAllFail(): Unit \ Assert =
    // all fail => last parser's error propagates
    let p = Combinator.anyOf(Char.char('a') :: Char.char('b') :: Nil);
    let r = Combat.runParser(p, "xyz");
    match r {
        case ParseResult.Failure(s) =>
            // last parser was char('b'), so error is UnexpectedChar('b', 'x')
            Assert.assertTrue(hasUnexpectedChar(s, 'b', 'x'))
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

@Test
def testAnyOfEmptyList(): Unit \ Assert =
    let p = Combinator.anyOf(Nil);
    let r = Combat.runParser(p, "abc");
    match r {
        case ParseResult.Failure(s) =>
            Assert.assertTrue(Set.memberOf(ParseError.EmptyChoice, s))
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

@Test
def testAnyOfFatalFallback(): Unit \ Assert =
    // first parser fatals on empty input, falls back to second which succeeds
    let p = Combinator.anyOf(Char.anyChar() :: Char.char('b') :: Nil);
    let r = Combat.runParser(p, "bcd");
    Assert.assertEq(expected = ParseResult.Success('b'), r)

@Test
def testAnyOfThreeParsers(): Unit \ Assert =
    // 3 parsers, third succeeds
    let p = Combinator.anyOf(Char.char('a') :: Char.char('b') :: Char.char('c') :: Nil);
    let r = Combat.runParser(p, "ccc");
    Assert.assertEq(expected = ParseResult.Success('c'), r)

@Test
def testAnyOfLastFatal(): Unit \ Assert =
    // all fail, last one fatals => Failure with the last error
    let p = Combinator.anyOf(Char.char('a') :: Char.char('b') :: Char.anyChar() :: Nil);
    let r = Combat.runParser(p, "");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_)  => Assert.assertTrue(false)
    }

// ── Complex multiple non-fatal error recovery tests ─────────────────────

@Test
def testComplexChainFourNonFatalErrors(): Unit \ Assert =
    // A chain of four char parsers, each mis-matching on a different input char.
    // All errors are non-fatal and should be collected in the Failure set.
    let p = Char.char('a')
        |> Combinator.andThen(Char.char('b'))
        |> Combinator.andThen(Char.char('c'))
        |> Combinator.andThen(Char.char('d'));
    let r = Combat.runParser(p, "wxyz");
    match r {
        case ParseResult.Failure(s) =>
            Assert.assertTrue(hasUnexpectedChar(s, 'a', 'w'));
            Assert.assertTrue(hasUnexpectedChar(s, 'b', 'x'));
            Assert.assertTrue(hasUnexpectedChar(s, 'c', 'y'));
            Assert.assertTrue(hasUnexpectedChar(s, 'd', 'z'))
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

@Test
def testComplexMixedErrorsCollected(): Unit \ Assert =
    // String mismatch + char mismatch + final char mismatch.
    // All non-fatal raises should accumulate across the andThen chain.
    let p = String.string("ab")
        |> Combinator.discardThen(Char.char('c'))
        |> Combinator.andThen(Char.char('d'));
    let r = Combat.runParser(p, "xbzw");
    match r {
        case ParseResult.Failure(s) =>
            Assert.assertTrue(hasUnexpectedString(s, "ab", "xb"));
            Assert.assertTrue(hasUnexpectedChar(s, 'c', 'z'));
            Assert.assertTrue(hasUnexpectedChar(s, 'd', 'w'))
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

// ── Combat.Char.whitespace tests ────────────────────────────────────────

@Test
def testWhitespaceSpace(): Unit \ Assert =
    let r = Combat.runParser(Char.whitespace(), " xyz");
    Assert.assertEq(expected = ParseResult.Success(' '), r)

@Test
def testWhitespaceTab(): Unit \ Assert =
    let r = Combat.runParser(Char.whitespace(), "\txyz");
    Assert.assertEq(expected = ParseResult.Success('\t'), r)

@Test
def testWhitespaceCarriageReturn(): Unit \ Assert =
    let r = Combat.runParser(Char.whitespace(), "\rxyz");
    Assert.assertEq(expected = ParseResult.Success('\r'), r)

@Test
def testWhitespaceNewline(): Unit \ Assert =
    let r = Combat.runParser(Char.whitespace(), "\nxyz");
    Assert.assertEq(expected = ParseResult.Success('\n'), r)

@Test
def testWhitespaceNonWhitespaceFails(): Unit \ Assert =
    // non-whitespace character => anyOf falls through all alternatives,
    // discarding intermediate errors, leaving only the last one's error.
    let r = Combat.runParser(Char.whitespace(), "abc");
    match r {
        case ParseResult.Failure(s) =>
            // last alternative is char('\n'); anyOf discards errors from ' ', '\t', '\r'
            Assert.assertTrue(hasUnexpectedChar(s, '\n', 'a'));
            Assert.assertEq(expected = 1, Set.size(s))
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

@Test
def testWhitespaceEmptyFatal(): Unit \ Assert =
    // empty input => fatal EmptyString from anyOf's alternatives
    let r = Combat.runParser(Char.whitespace(), "");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

// ── Combat.Combinator.thenDiscard tests ─────────────────────────────────

@Test
def testThenDiscardBothOk(): Unit \ Assert =
    // p1's result kept, p2 executed and consumed
    let p = Char.char('a') |> Combinator.thenDiscard(Char.char('b'));
    let r = Combat.runParser(p, "abc");
    Assert.assertEq(expected = ParseResult.Success('a'), r)

@Test
def testThenDiscardP1Fails(): Unit \ Assert =
    // p1 fails (non-fatal) => p2 still runs (consuming "bc") but overall Failure
    let p = Char.char('a') |> Combinator.thenDiscard(Char.char('b'));
    let r = Combat.runParser(p, "xbc");
    match r {
        case ParseResult.Failure(s) =>
            // only p1's error should be collected (p2 succeeds)
            Assert.assertTrue(hasUnexpectedChar(s, 'a', 'x'));
            Assert.assertEq(expected = 1, Set.size(s))
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

@Test
def testThenDiscardP2Fails(): Unit \ Assert =
    // p1 succeeds, p2 fails (non-fatal) => overall Failure
    let p = Char.char('a') |> Combinator.thenDiscard(Char.char('b'));
    let r = Combat.runParser(p, "axc");
    match r {
        case ParseResult.Failure(s) =>
            // only p2's error should be collected
            Assert.assertTrue(hasUnexpectedChar(s, 'b', 'x'));
            Assert.assertEq(expected = 1, Set.size(s))
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

@Test
def testThenDiscardP1Fatal(): Unit \ Assert =
    let p = Char.anyChar() |> Combinator.thenDiscard(Char.char('b'));
    let r = Combat.runParser(p, "");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

@Test
def testThenDiscardP2Fatal(): Unit \ Assert =
    let p = Char.char('a') |> Combinator.thenDiscard(Char.anyChar());
    let r = Combat.runParser(p, "a");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

@Test
def testThenDiscardInputConsumed(): Unit \ Assert =
    // both match, input consumed by both
    let p = String.string("hi") |> Combinator.thenDiscard(Char.char('!'));
    let r = Combat.runParser(p, "hi!rest");
    Assert.assertEq(expected = ParseResult.Success("hi"), r)

// ── Combat.String.whitespaces0 / whitespaces1 tests ─────────────────────

@Test
def testWhitespaces0Zero(): Unit \ Assert =
    // zero whitespace chars => success with empty string
    let r = Combat.runParser(String.whitespaces0(), "abc");
    Assert.assertEq(expected = ParseResult.Success(""), r)

@Test
def testWhitespaces0Multiple(): Unit \ Assert =
    // multiple whitespace chars consumed, then stops at 'a'
    let r = Combat.runParser(String.whitespaces0(), "  \t\na");
    Assert.assertEq(expected = ParseResult.Success("  \t\n"), r)

@Test
def testWhitespaces0Empty(): Unit \ Assert =
    let r = Combat.runParser(String.whitespaces0(), "");
    Assert.assertEq(expected = ParseResult.Success(""), r)

@Test
def testWhitespaces1Single(): Unit \ Assert =
    let r = Combat.runParser(String.whitespaces1(), " xyz");
    Assert.assertEq(expected = ParseResult.Success(" "), r)

@Test
def testWhitespaces1Multiple(): Unit \ Assert =
    let r = Combat.runParser(String.whitespaces1(), "\t\r\nabc");
    Assert.assertEq(expected = ParseResult.Success("\t\r\n"), r)

@Test
def testWhitespaces1ZeroFails(): Unit \ Assert =
    // zero matches => many1's first p fails; anyOf discards intermediate
    // errors, leaving only the last alternative's error.
    let r = Combat.runParser(String.whitespaces1(), "abc");
    match r {
        case ParseResult.Failure(s) =>
            // last alternative inside whitespace() is char('\n')
            Assert.assertTrue(hasUnexpectedChar(s, '\n', 'a'));
            Assert.assertEq(expected = 1, Set.size(s))
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

@Test
def testWhitespaces1EmptyFatal(): Unit \ Assert =
    // empty input => char(' ') inside whitespace() fatals on empty string
    let r = Combat.runParser(String.whitespaces1(), "");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

// ── Combat.Char.satisfy tests ─────────────────────────────────────────

@Test
def testSatisfyOk(): Unit \ Assert =
    let r = Combat.runParser(Char.satisfy(c -> c >= '0' and c <= '9'), "42");
    Assert.assertEq(expected = ParseResult.Success('4'), r)

@Test
def testSatisfyFail(): Unit \ Assert =
    let r = Combat.runParser(Char.satisfy(c -> c >= '0' and c <= '9'), "abc");
    match r {
        case ParseResult.Failure(s) =>
            Assert.assertTrue(hasUnexpectedChar(s, 'a', 'a'))
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

@Test
def testSatisfyEmpty(): Unit \ Assert =
    let r = Combat.runParser(Char.satisfy(c -> c == 'x'), "");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

// ── Combat.Combinator.lookAhead tests ─────────────────────────────────

@Test
def testLookAheadOk(): Unit \ Assert =
    // lookAhead does not consume input
    let p = Combinator.lookAhead(Char.char('a'));
    let r = Combat.runParser(p, "abc");
    Assert.assertEq(expected = ParseResult.Success('a'), r)

@Test
def testLookAheadChained(): Unit \ Assert =
    // after lookAhead, the same input is still available
    let p = Combinator.lookAhead(Char.char('a')) |> Combinator.thenDiscard(Char.char('a'));
    let r = Combat.runParser(p, "abc");
    Assert.assertEq(expected = ParseResult.Success('a'), r)

// ── Combat.Combinator.eof tests ───────────────────────────────────────

@Test
def testEofEmpty(): Unit \ Assert =
    let r = Combat.runParser(Combinator.eof(), "");
    Assert.assertEq(expected = ParseResult.Success(()), r)

@Test
def testEofNonEmpty(): Unit \ Assert =
    let r = Combat.runParser(Combinator.eof(), "abc");
    match r {
        case ParseResult.Failure(s) =>
            Assert.assertTrue(hasUnexpectedString(s, "EOF", "a"))
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

@Test
def testEofAfterParse(): Unit \ Assert =
    // succeeds when input is fully consumed
    let p = Char.char('a') |> Combinator.thenDiscard(Combinator.eof());
    let r = Combat.runParser(p, "a");
    Assert.assertEq(expected = ParseResult.Success('a'), r)

@Test
def testEofWithTrailing(): Unit \ Assert =
    // fails when there is trailing input
    let p = Char.char('a') |> Combinator.thenDiscard(Combinator.eof());
    let r = Combat.runParser(p, "ab");
    match r {
        case ParseResult.Failure(s) =>
            Assert.assertTrue(hasUnexpectedString(s, "EOF", "b"))
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

// ── Combat.Char.digit / letter / letterOrDigit tests ────────────────────

@Test
def testDigitOk(): Unit \ Assert =
    let r = Combat.runParser(Char.digit(), "42");
    Assert.assertEq(expected = ParseResult.Success('4'), r)

@Test
def testDigitFail(): Unit \ Assert =
    let r = Combat.runParser(Char.digit(), "abc");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

@Test
def testDigitEmpty(): Unit \ Assert =
    let r = Combat.runParser(Char.digit(), "");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

@Test
def testLetterLower(): Unit \ Assert =
    let r = Combat.runParser(Char.letter(), "abc");
    Assert.assertEq(expected = ParseResult.Success('a'), r)

@Test
def testLetterUpper(): Unit \ Assert =
    let r = Combat.runParser(Char.letter(), "ABC");
    Assert.assertEq(expected = ParseResult.Success('A'), r)

@Test
def testLetterFail(): Unit \ Assert =
    let r = Combat.runParser(Char.letter(), "123");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

@Test
def testLetterEmpty(): Unit \ Assert =
    let r = Combat.runParser(Char.letter(), "");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

@Test
def testLetterOrDigitLetter(): Unit \ Assert =
    let r = Combat.runParser(Char.letterOrDigit(), "abc");
    Assert.assertEq(expected = ParseResult.Success('a'), r)

@Test
def testLetterOrDigitDigit(): Unit \ Assert =
    let r = Combat.runParser(Char.letterOrDigit(), "42");
    Assert.assertEq(expected = ParseResult.Success('4'), r)

@Test
def testLetterOrDigitFail(): Unit \ Assert =
    let r = Combat.runParser(Char.letterOrDigit(), "!!!");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

@Test
def testLetterOrDigitEmpty(): Unit \ Assert =
    let r = Combat.runParser(Char.letterOrDigit(), "");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

// ── Combat.Combinator.between tests ─────────────────────────────────────

@Test
def testBetweenParens(): Unit \ Assert =
    // between '(', ')', char('a') => 'a'
    let p = Combinator.between(Char.char('('), Char.char(')'), Char.char('a'));
    let r = Combat.runParser(p, "(a)");
    Assert.assertEq(expected = ParseResult.Success('a'), r)

@Test
def testBetweenBraces(): Unit \ Assert =
    // between '{', '}', digit() => '4'
    let p = Combinator.between(Char.char('{'), Char.char('}'), Char.digit());
    let r = Combat.runParser(p, "{4}");
    Assert.assertEq(expected = ParseResult.Success('4'), r)

@Test
def testBetweenOpenFails(): Unit \ Assert =
    // open mismatch => failure
    let p = Combinator.between(Char.char('('), Char.char(')'), Char.char('a'));
    let r = Combat.runParser(p, "[a]");
    match r {
        case ParseResult.Failure(s) =>
            Assert.assertTrue(hasUnexpectedChar(s, '(', '['))
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

@Test
def testBetweenCloseFails(): Unit \ Assert =
    // close mismatch => failure
    let p = Combinator.between(Char.char('('), Char.char(')'), Char.char('a'));
    let r = Combat.runParser(p, "(a]");
    match r {
        case ParseResult.Failure(s) =>
            Assert.assertTrue(hasUnexpectedChar(s, ')', ']'))
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

@Test
def testBetweenInnerFails(): Unit \ Assert =
    // inner mismatch => failure
    let p = Combinator.between(Char.char('('), Char.char(')'), Char.char('a'));
    let r = Combat.runParser(p, "(b)");
    match r {
        case ParseResult.Failure(s) =>
            Assert.assertTrue(hasUnexpectedChar(s, 'a', 'b'))
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

// ── Combat.Combinator.optional tests ────────────────────────────────────

@Test
def testOptionalSome(): Unit \ Assert =
    let p = Combinator.optional(Char.char('a'));
    let r = Combat.runParser(p, "abc");
    Assert.assertEq(expected = ParseResult.Success(Some('a')), r)

@Test
def testOptionalNone(): Unit \ Assert =
    let p = Combinator.optional(Char.char('a'));
    let r = Combat.runParser(p, "xyz");
    Assert.assertEq(expected = ParseResult.Success(None), r)

@Test
def testOptionalEmpty(): Unit \ Assert =
    let p = Combinator.optional(Char.char('a'));
    let r = Combat.runParser(p, "");
    Assert.assertEq(expected = ParseResult.Success(None), r)

// ── Combat.Combinator.flatMap tests ─────────────────────────────────────

@Test
def testFlatMapSimple(): Unit \ Assert =
    // parse 'a', then choose next parser based on result
    let p = Char.char('a') |> Combinator.flatMap(c -> Char.char(c));
    let r = Combat.runParser(p, "aa");
    Assert.assertEq(expected = ParseResult.Success('a'), r)

@Test
def testFlatMapDifferent(): Unit \ Assert =
    // parse 'a', then expect 'b'
    let p = Char.char('a') |> Combinator.flatMap(_ -> Char.char('b'));
    let r = Combat.runParser(p, "ab");
    Assert.assertEq(expected = ParseResult.Success('b'), r)

@Test
def testFlatMapFail(): Unit \ Assert =
    let p = Char.char('a') |> Combinator.flatMap(_ -> Char.char('b'));
    let r = Combat.runParser(p, "ac");
    match r {
        case ParseResult.Failure(s) =>
            Assert.assertTrue(hasUnexpectedChar(s, 'b', 'c'))
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

// ── Combat.Combinator.sepBy / sepBy1 tests ──────────────────────────────

@Test
def testSepBy1Two(): Unit \ Assert =
    let p = Combinator.sepBy1(Char.digit(), Char.char(','));
    let r = Combat.runParser(p, "1,2");
    Assert.assertEq(expected = ParseResult.Success('1' :: '2' :: Nil), r)

@Test
def testSepBy1Three(): Unit \ Assert =
    let p = Combinator.sepBy1(Char.digit(), Char.char(','));
    let r = Combat.runParser(p, "1,2,3");
    Assert.assertEq(expected = ParseResult.Success('1' :: '2' :: '3' :: Nil), r)

@Test
def testSepBy1Single(): Unit \ Assert =
    let p = Combinator.sepBy1(Char.digit(), Char.char(','));
    let r = Combat.runParser(p, "5");
    Assert.assertEq(expected = ParseResult.Success('5' :: Nil), r)

@Test
def testSepBy1NoSep(): Unit \ Assert =
    // sep not followed by another digit => stop after first digit
    let p = Combinator.sepBy1(Char.digit(), Char.char(','));
    let r = Combat.runParser(p, "5x");
    Assert.assertEq(expected = ParseResult.Success('5' :: Nil), r)

@Test
def testSepBy1Empty(): Unit \ Assert =
    let p = Combinator.sepBy1(Char.digit(), Char.char(','));
    let r = Combat.runParser(p, "");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

@Test
def testSepByZero(): Unit \ Assert =
    let p = Combinator.sepBy(Char.digit(), Char.char(','));
    let r = Combat.runParser(p, "abc");
    Assert.assertEq(expected = ParseResult.Success(Nil), r)

@Test
def testSepByTwo(): Unit \ Assert =
    let p = Combinator.sepBy(Char.digit(), Char.char(','));
    let r = Combat.runParser(p, "1,2");
    Assert.assertEq(expected = ParseResult.Success('1' :: '2' :: Nil), r)

@Test
def testSepByEmpty(): Unit \ Assert =
    let p = Combinator.sepBy(Char.digit(), Char.char(','));
    let r = Combat.runParser(p, "");
    Assert.assertEq(expected = ParseResult.Success(Nil), r)

// ── Combat.Literal tests ────────────────────────────────────────────────

@Test
def testStringLiteralSimple(): Unit \ Assert =
    let r = Combat.runParser(Literal.stringLiteral(), "\"hello\"");
    Assert.assertEq(expected = ParseResult.Success("hello"), r)

@Test
def testStringLiteralEmpty(): Unit \ Assert =
    let r = Combat.runParser(Literal.stringLiteral(), "\"\"");
    Assert.assertEq(expected = ParseResult.Success(""), r)

@Test
def testStringLiteralUnclosed(): Unit \ Assert =
    let r = Combat.runParser(Literal.stringLiteral(), "\"hello");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

@Test
def testIntegerSingle(): Unit \ Assert =
    let r = Combat.runParser(Literal.integer(), "42");
    Assert.assertEq(expected = ParseResult.Success(42), r)

@Test
def testIntegerMultiple(): Unit \ Assert =
    let r = Combat.runParser(Literal.integer(), "12345");
    Assert.assertEq(expected = ParseResult.Success(12345), r)

@Test
def testIntegerFail(): Unit \ Assert =
    let r = Combat.runParser(Literal.integer(), "abc");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }

@Test
def testIntegerEmpty(): Unit \ Assert =
    let r = Combat.runParser(Literal.integer(), "");
    match r {
        case ParseResult.Failure(_) => Assert.assertTrue(true)
        case ParseResult.Success(_) => Assert.assertTrue(false)
    }
