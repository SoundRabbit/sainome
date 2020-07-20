#[derive(Debug, PartialEq, Clone)]
pub enum Block {
    Block(Vec<Block>),
    Text(String),
}
