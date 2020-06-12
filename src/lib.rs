use nom::branch::*;
use nom::character::complete::*;
use nom::IResult;
use nom_locate::LocatedSpan;
use nom_recursive::{recursive_parser, RecursiveInfo};

// Input type must implement trait HasRecursiveInfo
// nom_locate::LocatedSpan<T, RecursiveInfo> implements it.
type Span<'a> = LocatedSpan<&'a str, RecursiveInfo>;

pub fn expr(s: Span) -> IResult<Span, String> {
    alt((expr_binary, term))(s)
}

// Apply recursive_parser by custom attribute
#[recursive_parser]
pub fn expr_binary(s: Span) -> IResult<Span, String> {
    let (s, x) = expr(s)?;
    let (s, y) = char('+')(s)?;
    let (s, z) = expr(s)?;
    let ret = format!("{}{}{}", x, y, z);
    Ok((s, ret))
}

pub fn term(s: Span) -> IResult<Span, String> {
    let (s, x) = char('1')(s)?;
    Ok((s, x.to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let ret = expr(LocatedSpan::new_extra("1+1", RecursiveInfo::new()));
        assert_eq!(ret.unwrap().1, String::from(""));
    }
}
