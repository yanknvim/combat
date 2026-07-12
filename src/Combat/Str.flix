mod Combat.Str {
    use Combat.{map, Parser, ParseError, ParseEff}
    use Combat.Char.whitespace
    use Combat.Combinator.{many0, many1}

    def guardEmpty(input: Combat.Input): Unit \ ParseEff = {
        if (String.isEmpty(input#text)) {
            ParseEff.raiseFatal(ParseError.EmptyString(input#offset))
        } else ()
    }

    def advanceN(n: Int32, input: Combat.Input): Combat.Input = {
        { text = String.dropLeft(n, input#text), offset = input#offset + n }
    }

    pub def string(s: String): Parser[String] = input -> {
        guardEmpty(input);
        let len = String.length(s);
        let rest = advanceN(len, input);
        if (String.startsWith({ prefix = s }, input#text)) {
            (s, rest)
        } else {
            let actual = String.sliceLeft({ end = len }, input#text);
            ParseEff.raise(ParseError.UnexpectedString(input#offset, s, actual));
            (s, rest)
        }
    }

    pub def whitespaces0(): Parser[String] =
        many0(whitespace())
        |> map(chars -> List.map(Char.toString, chars) |> List.join(""))

    pub def whitespaces1(): Parser[String] =
        many1(whitespace())
        |> map(chars -> List.map(Char.toString, chars) |> List.join(""))
}
