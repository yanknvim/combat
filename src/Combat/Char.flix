mod Combat.Char {
    use Combat.Combinator.anyOf
    use Combat.{Parser, ParseError, ParseEff}

    pub def anyChar(): Parser[Char] = input -> {
        if (String.isEmpty(input#text)) {
            ParseEff.raiseFatal(ParseError.EmptyString(input#offset))
        } else {
            let first = String.charAt(0, input#text);
            (first, { text = String.dropLeft(1, input#text), offset = input#offset + 1 })
        }
    }

    pub def char(c: Char): Parser[Char] = input -> {
        if (String.isEmpty(input#text)) {
            ParseEff.raiseFatal(ParseError.EmptyString(input#offset))
        } else {
            let first = String.charAt(0, input#text);
            if (first == c) {
                (first, { text = String.dropLeft(1, input#text), offset = input#offset + 1 })
            } else {
                ParseEff.raise(ParseError.UnexpectedChar(input#offset, c, first));
                (first, { text = String.dropLeft(1, input#text), offset = input#offset + 1 })
            }
        }
    }

    pub def whitespace(): Parser[Char] =
        anyOf(List#{char(' '), char('\t'), char('\r'), char('\n')})
}
