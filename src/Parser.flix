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
            let charList = String.toList(input);
            match charList {
                case Nil => ParseEff.raiseFatal(ParseError.EmptyString)
                case head :: tail => (head, List.toString(tail))
            }
        }

        pub def char(c: Char): Parser[Char] = input -> {
            let charList = String.toList(input);
            match charList {
                case Nil => ParseEff.raiseFatal(ParseError.EmptyString)
                case head :: tail if head == c => (head, List.toString(tail))
                case head :: tail => {
                    ParseEff.raise(ParseError.UnexpectedChar(c, head));
                    (head, List.toString(tail))
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
    }
}
