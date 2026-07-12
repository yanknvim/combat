use Combat.{runParser, Parser, map, Input, ParseEff}
use Combat.Char.{char}
use Combat.Combinator.{choice, sepBy, thenDiscard, discardThen, andThen, between, anyOf}
use Combat.Literal.{integer, stringLiteral}
use Combat.String.{string, whitespaces0}
use Combat.ParseResult

pub enum Json with Eq, ToString {
    case JNull
    case JBool(Bool)
    case JInt(Int32)
    case JString(String)
    case JArray(List[Json])
    case JObject(List[(String, Json)])
}

// ── Parsers ────────────────────────────────────────────────────────────

def ws(): Parser[Unit] = input -> {
    let (_, rest) = whitespaces0()(input);
    ((), rest)
}

def nullParser(): Parser[Json] =
    string("null") |> map(_ -> Json.JNull)

def boolParser(): Parser[Json] =
    choice(
        string("true") |> map(_ -> Json.JBool(true)),
        string("false") |> map(_ -> Json.JBool(false))
    )

def pair(): Parser[(String, Json)] =
    stringLiteral()
    |> thenDiscard(ws())
    |> thenDiscard(char(':'))
    |> thenDiscard(ws())
    |> andThen(jsonValueImpl)
    |> thenDiscard(ws())
    |> map(match (k, v) -> (k, v))

def array(): Parser[Json] = {
    let elem = input -> {
        let (_, s1) = ws()(input);
        let (v, s2) = jsonValueImpl(s1);
        let (_, s3) = ws()(s2);
        (v, s3)
    };
    let sep =
        char(',') |> thenDiscard(whitespaces0());
    between(char('['), char(']'), sepBy(elem, sep))
    |> map(xs -> Json.JArray(xs))
}

def object(): Parser[Json] = {
    let sep =
        char(',') |> thenDiscard(whitespaces0());
    between(char('{'), char('}'), sepBy(pair(), sep))
    |> map(xs -> Json.JObject(xs))
}

def jsonValueImpl(input: Input): (Json, Input) \ ParseEff = {
    let p = anyOf(
        nullParser() ::
        boolParser() ::
        integer() |> map(n -> Json.JInt(n)) ::
        stringLiteral() |> map(s -> Json.JString(s)) ::
        array() ::
        object() ::
        Nil
    );
    p(input)
}

pub def parse(s: String): Combat.ParseResult[Json] =
    runParser(jsonValueImpl |> thenDiscard(Combat.Combinator.eof()), s)

pub def demo(): Unit \ IO = {
    println("=== Json ===");
    println(parse("null"));
    println(parse("true"));
    println(parse("false"));
    println(parse("42"));
    println(parse("\"hello\""));
    println(parse("[1, 2, 3]"));
    println(parse("{\"a\": 1, \"b\": 2}"))
}

// ── Tests ──────────────────────────────────────────────────────────────

@Test
def testJsonNull(): Unit \ Assert =
    let r = parse("null");
    Assert.assertEq(expected = ParseResult.Success(Json.JNull), r)

@Test
def testJsonTrue(): Unit \ Assert =
    let r = parse("true");
    Assert.assertEq(expected = ParseResult.Success(Json.JBool(true)), r)

@Test
def testJsonFalse(): Unit \ Assert =
    let r = parse("false");
    Assert.assertEq(expected = ParseResult.Success(Json.JBool(false)), r)

@Test
def testJsonInt(): Unit \ Assert =
    let r = parse("42");
    Assert.assertEq(expected = ParseResult.Success(Json.JInt(42)), r)

@Test
def testJsonString(): Unit \ Assert =
    let r = parse("\"hello\"");
    Assert.assertEq(expected = ParseResult.Success(Json.JString("hello")), r)

@Test
def testJsonEmptyArray(): Unit \ Assert =
    let r = parse("[]");
    Assert.assertEq(expected = ParseResult.Success(Json.JArray(Nil)), r)

@Test
def testJsonArray(): Unit \ Assert =
    let r = parse("[1, 2, 3]");
    Assert.assertEq(expected = ParseResult.Success(Json.JArray(Json.JInt(1) :: Json.JInt(2) :: Json.JInt(3) :: Nil)), r)

@Test
def testJsonNestedArray(): Unit \ Assert =
    let r = parse("[1, [2, 3]]");
    Assert.assertEq(expected = ParseResult.Success(Json.JArray(Json.JInt(1) :: Json.JArray(Json.JInt(2) :: Json.JInt(3) :: Nil) :: Nil)), r)

@Test
def testJsonEmptyObject(): Unit \ Assert =
    let r = parse("{}");
    Assert.assertEq(expected = ParseResult.Success(Json.JObject(Nil)), r)

@Test
def testJsonObject(): Unit \ Assert =
    let r = parse("{\"a\": 1, \"b\": 2}");
    Assert.assertEq(expected = ParseResult.Success(Json.JObject(("a", Json.JInt(1)) :: ("b", Json.JInt(2)) :: Nil)), r)

@Test
def testJsonNestedObject(): Unit \ Assert =
    let r = parse("{\"x\": {\"y\": 42}}");
    Assert.assertEq(expected = ParseResult.Success(Json.JObject(("x", Json.JObject(("y", Json.JInt(42)) :: Nil)) :: Nil)), r)

@Test
def testJsonMixed(): Unit \ Assert =
    let r = parse("{\"name\": \"foo\", \"value\": 123, \"flag\": true, \"items\": [1, 2]}");
    Assert.assertEq(expected = ParseResult.Success(Json.JObject(
        ("name", Json.JString("foo")) ::
        ("value", Json.JInt(123)) ::
        ("flag", Json.JBool(true)) ::
        ("items", Json.JArray(Json.JInt(1) :: Json.JInt(2) :: Nil)) ::
        Nil
    )), r)
