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

    /// Compute 1-based (line, column) from an offset into the source string.
    def offsetToLineCol(source: String, offset: Int32): (Int32, Int32) = {
        offsetToLineColHelper(source, offset, 0, 1, 1)
    }

    def offsetToLineColHelper(source: String, offset: Int32, idx: Int32, line: Int32, col: Int32): (Int32, Int32) = {
        if (idx >= offset) (line, col)
        else if (idx >= String.length(source)) (line, col)
        else {
            let c = String.charAt(idx, source);
            if (c == '\n') offsetToLineColHelper(source, offset, idx + 1, line + 1, 1)
            else offsetToLineColHelper(source, offset, idx + 1, line, col + 1)
        }
    }

    /// Join a list of strings with a separator.
    def joinStrings(sep: String, lines: List[String]): String = match lines {
        case Nil => ""
        case x :: xs => List.foldLeft((acc, s) -> acc + sep + s, x, xs)
    }

    /// Format a single parse error using the original source string to resolve offsets to line/column.
    pub def formatError(source: String, err: ParseError): String = match err {
        case ParseError.EmptyString(offset) => {
            let (line, col) = offsetToLineCol(source, offset);
            "Empty input at ${line}:${col}"
        }
        case ParseError.EmptyChoice => {
            "No matching choice"
        }
        case ParseError.UnexpectedChar(offset, expected, actual) => {
            let (line, col) = offsetToLineCol(source, offset);
            "Unexpected character at ${line}:${col}: expected '${expected}', got '${actual}'"
        }
        case ParseError.UnexpectedString(offset, expected, actual) => {
            let (line, col) = offsetToLineCol(source, offset);
            "Unexpected string at ${line}:${col}: expected '${expected}', got '${actual}'"
        }
    }

    /// Format a parse result with line/column information for any errors.
    pub def formatResult(source: String, result: ParseResult[a]): String = match result {
        case ParseResult.Success(_) => "Parse succeeded"
        case ParseResult.Failure(errs) => {
            let msg = Set.toList(errs)
                |> List.map(e -> "  " + formatError(source, e))
                |> joinStrings("\n");
            "Parse failure:\n" + msg
        }
    }
}
