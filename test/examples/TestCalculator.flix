use Calculator.{parse, Expr}
use Combat.ParseResult

@Test
def testCalcAdd(): Unit \ Assert =
    let r = parse("1+2+3");
    Assert.assertEq(expected = ParseResult.Success(Expr.Add(Expr.Add(Expr.Num(1), Expr.Num(2)), Expr.Num(3))), r)

@Test
def testCalcSub(): Unit \ Assert =
    let r = parse("10-3");
    Assert.assertEq(expected = ParseResult.Success(Expr.Sub(Expr.Num(10), Expr.Num(3))), r)

@Test
def testCalcMulAdd(): Unit \ Assert =
    let r = parse("2*3+4");
    Assert.assertEq(expected = ParseResult.Success(Expr.Add(Expr.Mul(Expr.Num(2), Expr.Num(3)), Expr.Num(4))), r)

@Test
def testCalcParens(): Unit \ Assert =
    let r = parse("(1+2)*3");
    Assert.assertEq(expected = ParseResult.Success(Expr.Mul(Expr.Add(Expr.Num(1), Expr.Num(2)), Expr.Num(3))), r)

@Test
def testCalcDiv(): Unit \ Assert =
    let r = parse("100/10/2");
    Assert.assertEq(expected = ParseResult.Success(Expr.Div(Expr.Div(Expr.Num(100), Expr.Num(10)), Expr.Num(2))), r)
