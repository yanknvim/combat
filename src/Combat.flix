mod Combat {
    pub type alias Input = { text = String, offset = Int32 }

    pub type alias Parser[a] = Input -> (a, Input) \ ParseEff

    pub enum ParseError with Eq, Order, ToString {
        case EmptyString(Int32)
        case EmptyChoice
        case UnexpectedChar(Int32, Char, Char) // offset, expected, actual
        case UnexpectedString(Int32, String, String) // offset, expected, actual
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
                let (a, _rest) = p({ text = input, offset = 0 });
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

    mod String {
        use Combat.{map, Parser, ParseError, ParseEff}
        use Combat.Char.whitespace
        use Combat.Combinator.{many0, many1};

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
