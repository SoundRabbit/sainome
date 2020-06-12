extern crate nom;
extern crate nom_recursive;

use nom::{
    branch::{alt, permutation},
    bytes::complete::escaped_transform,
    character::complete::{alpha1, anychar, char, multispace0, none_of},
    combinator::{map, value},
    multi::{many0, separated_list},
    number::complete::double,
    sequence::delimited,
    IResult,
};
use nom_locate::LocatedSpan;
use nom_recursive::{recursive_parser, RecursiveInfo};

type Span<'a> = LocatedSpan<&'a str, RecursiveInfo>;

#[derive(Debug, PartialEq)]
enum Literal {
    Number(f64),
    String(String),
    Ident(String),
    Reference(Vec<String>),
}

fn number(s: Span) -> IResult<Span, Literal> {
    map(double, |s| Literal::Number(s))(s)
}

fn string(s: Span) -> IResult<Span, Literal> {
    map(
        delimited(
            char('\"'),
            escaped_transform(
                none_of("\"\\"),
                '\\',
                alt((
                    value('\\', char('\\')),
                    value('\"', char('\"')),
                    value('\'', char('\'')),
                    value('\r', char('r')),
                    value('\n', char('n')),
                    value('\t', char('t')),
                )),
            ),
            char('\"'),
        ),
        |s| Literal::String(s),
    )(s)
}

fn reference(s: Span) -> IResult<Span, Literal> {
    map(
        delimited(
            char('{'),
            separated_list(char('.'), many0(anychar)),
            char('}'),
        ),
        |r| Literal::Reference(r.into_iter().map(|x| x.into_iter().collect()).collect()),
    )(s)
}

fn literal(s: Span) -> IResult<Span, Literal> {
    alt((number, string, reference))(s)
}

#[derive(Debug, PartialEq)]
enum MultiplicativeOperator {
    Multi,
    Div,
    Mod,
}

fn operator_multi(s: Span) -> IResult<Span, MultiplicativeOperator> {
    map(char('*'), |_| MultiplicativeOperator::Multi)(s)
}

fn operator_div(s: Span) -> IResult<Span, MultiplicativeOperator> {
    map(char('/'), |_| MultiplicativeOperator::Div)(s)
}

fn operator_mod(s: Span) -> IResult<Span, MultiplicativeOperator> {
    map(char('%'), |_| MultiplicativeOperator::Mod)(s)
}

fn multiplicative_operator(s: Span) -> IResult<Span, MultiplicativeOperator> {
    alt((operator_multi, operator_div, operator_mod))(s)
}

#[derive(Debug, PartialEq)]
enum AdditiveOperator {
    Add,
    Minus,
}

fn operator_add(s: Span) -> IResult<Span, AdditiveOperator> {
    map(char('+'), |_| AdditiveOperator::Add)(s)
}

fn operator_minus(s: Span) -> IResult<Span, AdditiveOperator> {
    map(char('-'), |_| AdditiveOperator::Minus)(s)
}

fn additive_operator(s: Span) -> IResult<Span, AdditiveOperator> {
    alt((operator_add, operator_minus))(s)
}

#[derive(Debug, PartialEq)]
enum RelationalOperator {
    Equals,
    GreaterThan,
    LessThan,
    EqGreaterThan,
    EqLessThan,
    NotEquals,
}

fn operator_eq(s: Span) -> IResult<Span, RelationalOperator> {
    map(permutation((char('='), char('='))), |_| {
        RelationalOperator::Equals
    })(s)
}

fn operator_gt(s: Span) -> IResult<Span, RelationalOperator> {
    map(char('>'), |_| RelationalOperator::GreaterThan)(s)
}

fn operator_lt(s: Span) -> IResult<Span, RelationalOperator> {
    map(char('>'), |_| RelationalOperator::LessThan)(s)
}

fn operator_eq_gt(s: Span) -> IResult<Span, RelationalOperator> {
    map(permutation((char('>'), char('='))), |_| {
        RelationalOperator::EqGreaterThan
    })(s)
}

fn operator_eq_lt(s: Span) -> IResult<Span, RelationalOperator> {
    map(permutation((char('<'), char('='))), |_| {
        RelationalOperator::EqLessThan
    })(s)
}

fn operator_not_eq(s: Span) -> IResult<Span, RelationalOperator> {
    map(permutation((char('!'), char('='))), |_| {
        RelationalOperator::NotEquals
    })(s)
}

fn relational_operator(s: Span) -> IResult<Span, RelationalOperator> {
    alt((
        operator_eq,
        operator_eq_gt,
        operator_eq_lt,
        operator_gt,
        operator_lt,
        operator_not_eq,
    ))(s)
}

#[derive(Debug, PartialEq)]
enum PrimaryExpression {
    Tuple(Vec<Expression>),
    Literal(Literal),
}

#[recursive_parser]
fn tuple(s: Span) -> IResult<Span, Vec<Expression>> {
    delimited(char('('), separated_list(char(','), expression), char(')'))(s)
}

fn primary_expression(s: Span) -> IResult<Span, PrimaryExpression> {
    alt((
        map(literal, |x| PrimaryExpression::Literal(x)),
        map(tuple, |x| PrimaryExpression::Tuple(x)),
    ))(s)
}

#[derive(Debug, PartialEq)]
enum Expression {
    PrimaryExpression(PrimaryExpression),
}

fn expression(s: Span) -> IResult<Span, Expression> {
    map(primary_expression, |x| Expression::PrimaryExpression(x))(s)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = expression(LocatedSpan::new_extra("(0,120.0,5)", RecursiveInfo::new()));
        assert_eq!(
            result.map(|(_, r)| r).ok(),
            Some(Expression::PrimaryExpression(PrimaryExpression::Tuple(
                vec![]
            )))
        );
    }
}
