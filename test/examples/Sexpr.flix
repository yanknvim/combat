mod Sexpr {
    use Combat.{runParser, Parser, map, Input, ParseEff}
    use Combat.Char.{char, letterOrDigit}
    use Combat.Combinator.{choice, between, many1, sepBy, thenDiscard}
    use Combat.Literal.{integer, stringLiteral}
    use Combat.String.whitespaces1

    pub enum SExpr with Eq, ToString {
        case SInt(Int32)
        case SString(String)
        case SSym(String)
        case SList(List[SExpr])
    }

    def symImpl(input: Input): (String, Input) \ ParseEff = {
        let (chars, rest) = many1(letterOrDigit())(input);
        let s = chars |> List.map(Char.toString) |> List.join("");
        (s, rest)
    }

    def atomImpl(input: Input): (SExpr, Input) \ ParseEff = {
        let p = Combat.Combinator.anyOf(
            integer() |> map(n -> SExpr.SInt(n)) ::
            stringLiteral() |> map(s -> SExpr.SString(s)) ::
            (input2 -> symImpl(input2)) |> map(s -> SExpr.SSym(s)) ::
            Nil
        );
        p(input)
    }

    def sexprImpl(input: Input): (SExpr, Input) \ ParseEff = {
        let p = choice(
            input2 -> atomImpl(input2),
            between(
                char('('),
                char(')'),
                sepBy(input2 -> sexprImpl(input2), whitespaces1()) |> map(xs -> SExpr.SList(xs))
            )
        );
        p(input)
    }

    pub def sexpr(): Parser[SExpr] = input -> sexprImpl(input)

    pub def parse(s: String): Combat.ParseResult[SExpr] =
        runParser(sexpr() |> thenDiscard(Combat.Combinator.eof()), s)

    pub def demo(): Unit \ IO = {
        println("=== Sexpr ===");
        println(parse("42"));
        println(parse("\"hello\""));
        println(parse("(foo bar)"));
        println(parse("(1 2 3)"));
        println(parse("(a \"b\" 42)"))
    }
}
