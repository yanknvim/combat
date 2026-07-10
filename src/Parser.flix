mod Parser {
    pub type alias Parser[a] = String -> (a, String) \ ParseEff

    pub enum ParseError with Eq, Order, ToString {
        case EmptyString
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
        use Parser.{Parser, ParseError, ParseEff}

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
    }

    mod String {
        use Parser.{Parser, ParseError, ParseEff}

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
    }

    pub def map(f: a -> b \ {}, p: Parser[a]): Parser[b] = input -> {
        let (r, rest) = p(input);
        (f(r), rest)
    }

    pub def pure(v: a): Parser[a] = input -> eidentity((v, input))

    mod Combinator {
        use Parser.{Parser, ParseError, ParseEff}

        pub def choice(p1: Parser[a], p2: Parser[a]): Parser[a] = input -> {
            run {
                p1(input)
            } with handler ParseEff {
                def raise(_err, _resume) = p2(input)

                def raiseFatal(_err, _resume) = p2(input)
            }
        }

        pub def andThen(p1: Parser[a], p2: Parser[b]): Parser[(a, b)] = input -> {
            let (r1, s1) = p1(input);
            let (r2, s2) = p2(s1);
            ((r1, r2), s2)
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
    }
}
