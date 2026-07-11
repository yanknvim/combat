mod Combat {
    pub type alias Parser[a] = String -> (a, String) \ ParseEff

    pub enum ParseError with Eq, Order, ToString {
        case EmptyString
        case EmptyChoice
        case UnexpectedChar(Char, Char) // expected, actual
        case UnexpectedString(String, String)
    }

    pub eff ParseEff {
        def raise(p: ParseError): Unit
        def raiseFatal(p: ParseError): Void
    }

    pub enum ParseResult[a] with Eq, ToString {
        case Success(a)
        case Failure(Set[ParseError])
    }

    pub def map(f: a -> b \ {}, p: Parser[a]): Parser[b] = input -> {
        let (r, rest) = p(input);
        (f(r), rest)
    }

    pub def pure(v: a): Parser[a] = input -> eidentity((v, input))

    pub def runParser(p: Parser[a], input: String): ParseResult[a] =
        region rc {
            let errors = Ref.fresh(rc, Set.empty());
            run {
                let (a, _rest) = p(input);
                let collected = Ref.get(errors);
                if (Set.isEmpty(collected)) ParseResult.Success(a)
                else ParseResult.Failure(collected)
            } with handler ParseEff {
                def raise(err, resume) = {
                    Ref.put(Set.insert(err, Ref.get(errors)), errors);
                    resume(())
                }
                def raiseFatal(err, _resume) = ParseResult.Failure(Set.insert(err, Ref.get(errors)))
            }
        }

    mod Char {
        use Combat.Combinator.anyOf
        use Combat.{Parser, ParseError, ParseEff}

        pub def anyChar(): Parser[Char] = input -> {
            if (String.isEmpty(input)) {
                ParseEff.raiseFatal(ParseError.EmptyString)
            } else {
                let first = String.charAt(0, input);
                (first, String.dropLeft(1, input))
            }
        }

        pub def char(c: Char): Parser[Char] = input -> {
            if (String.isEmpty(input)) {
                ParseEff.raiseFatal(ParseError.EmptyString)
            } else {
                let first = String.charAt(0, input);
                if (first == c) {
                    (first, String.dropLeft(1, input))
                } else {
                    ParseEff.raise(ParseError.UnexpectedChar(c, first));
                    (first, String.dropLeft(1, input))
                }
            }
        }

        pub def whitespace(): Parser[Char] =
            anyOf(List#{char(' '), char('\t'), char('\r'), char('\n')})
    }

    mod String {
        use Combat.{map, Parser, ParseError, ParseEff}
        use Combat.Char.whitespace
        use Combat.Combinator.{many0, many1};

        pub def string(s: String): Parser[String] = input -> {
            if (String.isEmpty(input)) {
                ParseEff.raiseFatal(ParseError.EmptyString)
            } else if (String.startsWith({ prefix = s }, input)) {
                (s, String.dropLeft(String.length(s), input))
            } else {
                ParseEff.raise(ParseError.UnexpectedString(s, String.sliceLeft({ end = String.length(s) }, input)));
                (s, String.dropLeft(String.length(s), input))
            }
        }

        pub def whitespaces0(): Parser[String] =
            many0(whitespace())
            |> map(chars -> List.map(Char.toString, chars) |> List.join(""))

        pub def whitespaces1(): Parser[String] =
            many1(whitespace())
            |> map(chars -> List.map(Char.toString, chars) |> List.join(""))
    }

    mod Combinator {
        use Combat.{Parser, ParseError, ParseEff}

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
}
