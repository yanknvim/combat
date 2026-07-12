use Combat.{runParser, formatResult}
use Combat.Char.{char}
use Combat.Combinator.{andThen}

def main(): Unit \ IO = {
    let src = "a\ncd";
    let p = char('a') |> andThen(char('b'));
    let r = runParser(p, src);
    println("=== raw result ===");
    println(r);
    println("=== formatted ===");
    println(formatResult(src, r))
}
