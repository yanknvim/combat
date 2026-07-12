mod Calculator {
    use Combat.{runParser, Parser, map, Input, ParseEff}
    use Combat.Char.{digit, char}
    use Combat.Combinator.{choice, between, many0, thenDiscard}
    use Combat.Literal.integer

    pub enum Expr with Eq, ToString {
        case Num(Int32)
        case Add(Expr, Expr)
        case Sub(Expr, Expr)
        case Mul(Expr, Expr)
        case Div(Expr, Expr)
    }

    def factorImpl(input: Input): (Expr, Input) \ ParseEff = {
        let p = choice(
            between(char('('), char(')'), input2 -> exprImpl(input2)),
            integer() |> map(n -> Expr.Num(n))
        );
        p(input)
    }

    def termOp(input: Input): ((Char, Expr), Input) \ ParseEff = {
        let (op, rest1) = choice(char('*'), char('/'))(input);
        let (f, rest2) = factorImpl(rest1);
        ((op, f), rest2)
    }

    def termImpl(input: Input): (Expr, Input) \ ParseEff = {
        let (first, rest) = factorImpl(input);
        let (ops, rest2) = many0(termOp)(rest);
        let result = List.foldLeft((acc, pair) -> {
            let (op, f) = pair;
            match op {
                case '*' => Expr.Mul(acc, f)
                case '/' => Expr.Div(acc, f)
                case _ => acc
            }
        }, first, ops);
        (result, rest2)
    }

    def exprOp(input: Input): ((Char, Expr), Input) \ ParseEff = {
        let (op, rest1) = choice(char('+'), char('-'))(input);
        let (t, rest2) = termImpl(rest1);
        ((op, t), rest2)
    }

    def exprImpl(input: Input): (Expr, Input) \ ParseEff = {
        let (first, rest) = termImpl(input);
        let (ops, rest2) = many0(exprOp)(rest);
        let result = List.foldLeft((acc, pair) -> {
            let (op, t) = pair;
            match op {
                case '+' => Expr.Add(acc, t)
                case '-' => Expr.Sub(acc, t)
                case _ => acc
            }
        }, first, ops);
        (result, rest2)
    }

    pub def expr(): Parser[Expr] = input -> exprImpl(input)

    pub def parse(s: String): Combat.ParseResult[Expr] =
        runParser(expr() |> thenDiscard(Combat.Combinator.eof()), s)

    pub def demo(): Unit \ IO = {
        println("=== Calculator ===");
        println(parse("1+2+3"));
        println(parse("10-3"));
        println(parse("2*3+4"));
        println(parse("(1+2)*3"));
        println(parse("100/10/2"))
    }
}
