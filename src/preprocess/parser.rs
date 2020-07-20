use super::ast::*;
use peg;

peg::parser! {
    pub grammar parse() for str {
        rule block() -> Block = precedence! {
            "(" xs:block_list() ")" { Block::Block(xs) }
            --
            xs:$(([_]!")")*) x:$([_]) { Block::Text(xs.to_string() + x) }
        }

        pub rule block_list() -> Vec<Block> =
            xs:block()* { xs }
    }
}
