use Combat.{runParser, formatResult}
use Combat.Char.{char}
use Combat.Str.{string}
use Combat.Combinator.{andThen, choice}

pub def main(): Unit \ IO = {
    println("=== Demo 1: four non-fatal errors in a chain ===");
    let src1 = "wxyz";
    let p1 = char('a')
        |> andThen(char('b'))
        |> andThen(char('c'))
        |> andThen(char('d'));
    let r1 = runParser(p1, src1);
    println("Raw:    " + ToString.toString(r1));
    println("Fancy:  " + formatResult(src1, r1));
    println("");

    println("=== Demo 2: multi-line source, string + char mismatch ===");
    let src2 = "hello\nworld";
    let p2 = string("hallo") |> andThen(char('!'));
    let r2 = runParser(p2, src2);
    println("Raw:    " + ToString.toString(r2));
    println("Fancy:  " + formatResult(src2, r2));
    println("");

    println("=== Demo 3: choice where every alternative fails ===");
    let src3 = "xyz";
    let p3 = choice(char('a'), char('b'));
    let r3 = runParser(p3, src3);
    println("Raw:    " + ToString.toString(r3));
    println("Fancy:  " + formatResult(src3, r3));
    println("");

    println("=== Demo 4: mixed errors on multiple lines ===");
    let src4 = "foo\nbar\nbaz";
    let p4 = string("foo")
        |> andThen(char('\n'))
        |> andThen(string("baz"));
    let r4 = runParser(p4, src4);
    println("Raw:    " + ToString.toString(r4));
    println("Fancy:  " + formatResult(src4, r4))
}
