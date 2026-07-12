mod Combat.Literal {
    use Combat.{Parser, ParseError, ParseEff}
    use Combat.Char.{digit, satisfy}
    use Combat.Combinator.{between, many0, many1}

    pub def stringLiteral(): Parser[String] = input -> {
        let inner = many0(satisfy(c -> c != '"'));
        let p = between(
            satisfy(c -> c == '"'),
            satisfy(c -> c == '"'),
            inner
        );
        let (chars, rest) = p(input);
        let s = chars |> List.map(Char.toString) |> List.join("");
        (s, rest)
    }

    pub def integer(): Parser[Int32] = input -> {
        let digits = many1(digit());
        let (chars, rest) = digits(input);
        let s = chars |> List.map(Char.toString) |> List.join("");
        match Int32.parse(10, s) {
            case Ok(n)    => (n, rest)
            case Err(msg) => ParseEff.raiseFatal(ParseError.UnexpectedString(input#offset, "integer", msg))
        }
    }
}
