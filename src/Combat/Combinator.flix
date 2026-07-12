mod Combat.Combinator {
    use Combat.{Parser, ParseError, ParseEff}

    pub def lookAhead(p: Parser[a]): Parser[a] = input -> {
        let (result, _) = p(input);
        (result, input)
    }

    pub def eof(): Parser[Unit] = input -> {
        if (String.isEmpty(input#text)) {
            ((), input)
        } else {
            let actual = String.takeLeft(1, input#text);
            ParseEff.raiseFatal(ParseError.UnexpectedString(input#offset, "EOF", actual))
        }
    }

    pub def choice(p1: Parser[a], p2: Parser[a]): Parser[a] = input -> {
        run {
            p1(input)
        } with handler ParseEff {
            def raise(_err, _resume) = p2(input)
            def raiseFatal(_err, _resume) = p2(input)
        }
    }

    pub def andThen(p2: Parser[b], p1: Parser[a]): Parser[(a, b)] = input -> {
        let (r1, s1) = p1(input);
        let (r2, s2) = p2(s1);
        ((r1, r2), s2)
    }

    pub def discardThen(p2: Parser[b], p1: Parser[a]): Parser[b] = input -> {
        let (_, s1) = p1(input);
        let (r2, s2) = p2(s1);
        (r2, s2)
    }

    pub def thenDiscard(p2: Parser[b], p1: Parser[a]): Parser[a] = input -> {
        let (r1, s1) = p1(input);
        let (_, s2) = p2(s1);
        (r1, s2)
    }

    pub def anyOf(ps: List[Parser[a]]): Parser[a] = input -> match ps {
        case Nil => ParseEff.raiseFatal(ParseError.EmptyChoice)
        case p :: Nil => p(input)
        case p :: rest => run {
            p(input)
        } with handler ParseEff {
            def raise(_err, _resume) = anyOf(rest)(input)
            def raiseFatal(_err, _resume) = anyOf(rest)(input)
        }
    }

    pub def many0(p: Parser[a]): Parser[List[a]] = input -> eidentity(
        run {
            let (x, rest) = p(input);
            let (xs, rest2) = many0(p)(rest);
            (x :: xs, rest2)
        } with handler ParseEff {
            def raise(_err, _resume) = (Nil, input)
            def raiseFatal(_err, _resume) = (Nil, input)
        }
    )

    pub def many1(p: Parser[a]): Parser[List[a]] = input -> {
        let (x, rest) = p(input);
        let (xs, rest2) = many0(p)(rest);
        (x ::xs, rest2)
    }
}
