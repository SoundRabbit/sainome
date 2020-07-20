use super::ast::*;
use peg;

pub mod parse {
    use super::*;
    use std::collections::VecDeque;
    pub fn block(chars: &mut VecDeque<char>) -> Block {
        let mut is_in_str = false;
        let mut text = String::new();
        let mut blocks = vec![];

        while let Some(c) = chars.pop_front() {
            if c == '"' {
                text.push(c);
                is_in_str = !is_in_str;
            } else {
                if is_in_str {
                    text.push(c);
                } else {
                    if c == '(' {
                        if text != "" {
                            blocks.push(Block::Text(text.drain(..).collect()));
                        }
                        blocks.push(block(chars));
                    } else if c == ')' {
                        if text != "" {
                            blocks.push(Block::Text(text.drain(..).collect()));
                        }
                        break;
                    } else {
                        text.push(c);
                    }
                }
            }
        }

        if text != "" {
            blocks.push(Block::Text(text.drain(..).collect()));
        }

        Block::Block(blocks)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_text() {
        let src = r#"abcd"#;
        let expect = Block::Block(vec![Block::Text(String::from("abcd"))]);
        assert_eq!(parse::block(&mut src.chars().collect()), expect);
    }

    #[test]
    fn a_block_in_a_block() {
        let src = r#"(abcd("abcd"))"#;
        let expect = Block::Block(vec![Block::Block(vec![
            Block::Text(String::from("abcd")),
            Block::Block(vec![Block::Text(String::from(r#""abcd""#))]),
        ])]);
        assert_eq!(parse::block(&mut src.chars().collect()), expect);
    }

    #[test]
    fn a_str_and_a_block() {
        let src = r#""abcd"(abcd)"#;
        let expect = Block::Block(vec![
            Block::Text(String::from(r#""abcd""#)),
            Block::Block(vec![Block::Text(String::from("abcd"))]),
        ]);
        assert_eq!(parse::block(&mut src.chars().collect()), expect);
    }
}
