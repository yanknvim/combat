mod Combat.Char {
    use Combat.Combinator.anyOf
    use Combat.{Parser, ParseError, ParseEff}

    def consume(input: Combat.Input): (Char, Combat.Input) \ ParseEff = {
        if (String.isEmpty(input#text)) {
            ParseEff.raiseFatal(ParseError.EmptyString(input#offset))
        } else {
            let c = String.charAt(0, input#text);
            let rest = { text = String.dropLeft(1, input#text), offset = input#offset + 1 };
            (c, rest)
        }
    }

    pub def satisfy(pred: Char -> Bool \ {}): Parser[Char] = input -> {
        let (c, rest) = consume(input);
        if (pred(c)) {
            (c, rest)
        } else {
            ParseEff.raiseFatal(ParseError.UnexpectedChar(input#offset, c, c))
        }
    }

    pub def anyChar(): Parser[Char] = input -> {
        let (c, rest) = consume(input);
        (c, rest)
    }

    pub def char(c: Char): Parser[Char] = input -> {
        let (first, rest) = consume(input);
        if (first == c) {
            (first, rest)
        } else {
            ParseEff.raise(ParseError.UnexpectedChar(input#offset, c, first));
            (first, rest)
        }
    }

    pub def whitespace(): Parser[Char] =
        anyOf(List#{char(' '), char('\t'), char('\r'), char('\n')})
}
