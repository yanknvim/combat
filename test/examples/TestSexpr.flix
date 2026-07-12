use Sexpr.{SExpr}
use Combat.ParseResult

@Test
def testSexprInt(): Unit \ Assert =
    let r = Sexpr.parse("42");
    Assert.assertEq(expected = ParseResult.Success(SExpr.SInt(42)), r)

@Test
def testSexprString(): Unit \ Assert =
    let r = Sexpr.parse("\"hello\"");
    Assert.assertEq(expected = ParseResult.Success(SExpr.SString("hello")), r)

@Test
def testSexprSym(): Unit \ Assert =
    let r = Sexpr.parse("(foo bar)");
    Assert.assertEq(expected = ParseResult.Success(SExpr.SList(SExpr.SSym("foo") :: SExpr.SSym("bar") :: Nil)), r)

@Test
def testSexprList(): Unit \ Assert =
    let r = Sexpr.parse("(1 2 3)");
    Assert.assertEq(expected = ParseResult.Success(SExpr.SList(SExpr.SInt(1) :: SExpr.SInt(2) :: SExpr.SInt(3) :: Nil)), r)

@Test
def testSexprMixed(): Unit \ Assert =
    let r = Sexpr.parse("(a \"b\" 42)");
    Assert.assertEq(expected = ParseResult.Success(SExpr.SList(SExpr.SSym("a") :: SExpr.SString("b") :: SExpr.SInt(42) :: Nil)), r)
