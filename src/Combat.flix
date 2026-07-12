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
}
