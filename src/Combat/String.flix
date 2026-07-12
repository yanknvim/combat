mod Combat.String {
    use Combat.{map, Parser, ParseError, ParseEff}
    use Combat.Char.whitespace
    use Combat.Combinator.{many0, many1}

    pub def string(s: String): Parser[String] = input -> {
        if (String.isEmpty(input#text)) {
            ParseEff.raiseFatal(ParseError.EmptyString(input#offset))
        } else if (String.startsWith({ prefix = s }, input#text)) {
            let len = String.length(s);
            (s, { text = String.dropLeft(len, input#text), offset = input#offset + len })
        } else {
            let len = String.length(s);
            let actual = String.sliceLeft({ end = len }, input#text);
            ParseEff.raise(ParseError.UnexpectedString(input#offset, s, actual));
            (s, { text = String.dropLeft(len, input#text), offset = input#offset + len })
        }
    }

    pub def whitespaces0(): Parser[String] =
        many0(whitespace())
        |> map(chars -> List.map(Char.toString, chars) |> List.join(""))

    pub def whitespaces1(): Parser[String] =
        many1(whitespace())
        |> map(chars -> List.map(Char.toString, chars) |> List.join(""))
}
