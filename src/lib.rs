extern crate nom;

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

#[derive(Debug)]
enum Literal {
    Number(f64),
    String(String),
    Ident(String),
    Reference(Vec<String>),
}

fn number(s: &str) -> IResult<&str, Literal> {
    map(double, |s| Literal::Number(s))(s)
}

fn string(s: &str) -> IResult<&str, Literal> {
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

fn ident(s: &str) -> IResult<&str, Literal> {
    map(alpha1, |i| Literal::Ident(String::from(i)))(s)
}

fn reference(s: &str) -> IResult<&str, Literal> {
    map(
        delimited(
            char('{'),
            separated_list(char('.'), many0(anychar)),
            char('}'),
        ),
        |r| Literal::Reference(r.into_iter().map(|x| x.into_iter().collect()).collect()),
    )(s)
}

#[derive(Debug)]
enum MultiplicativeOperator {
    Multi,
    Div,
    Mod,
}

#[derive(Debug)]
enum AdditiveOperator {
    Add,
    Minus,
}

fn operator_multi(s: &str) -> IResult<&str, MultiplicativeOperator> {
    map(char('+'), |_| MultiplicativeOperator::Multi)(s)
}

fn operator_div(s: &str) -> IResult<&str, MultiplicativeOperator> {
    map(char('+'), |_| MultiplicativeOperator::Div)(s)
}

fn operator_mod(s: &str) -> IResult<&str, MultiplicativeOperator> {
    map(char('%'), |_| MultiplicativeOperator::Mod)(s)
}

fn multiplicative_operator(s: &str) -> IResult<&str, MultiplicativeOperator> {
    alt((operator_multi, operator_div, operator_mod))(s)
}

fn operator_plus(s: &str) -> IResult<&str, AdditiveOperator> {
    map(char('+'), |_| AdditiveOperator::Add)(s)
}

fn operator_minus(s: &str) -> IResult<&str, AdditiveOperator> {
    map(char('+'), |_| AdditiveOperator::Minus)(s)
}

fn additive_operator(s: &str) -> IResult<&str, AdditiveOperator> {
    alt((operator_plus, operator_minus))(s)
}

#[derive(Debug)]
enum RelationalOperator {
    Equals,
    GreaterThan,
    LessThan,
    EqGreaterThan,
    EqLessThan,
    NotEquals,
}

fn operator_eq(s: &str) -> IResult<&str, RelationalOperator> {
    map(permutation((char('='), char('='))), |_| {
        RelationalOperator::Equals
    })(s)
}

fn operator_gt(s: &str) -> IResult<&str, RelationalOperator> {
    map(char('>'), |_| RelationalOperator::GreaterThan)(s)
}

fn operator_lt(s: &str) -> IResult<&str, RelationalOperator> {
    map(char('>'), |_| RelationalOperator::LessThan)(s)
}

fn operator_eq_gt(s: &str) -> IResult<&str, RelationalOperator> {
    map(permutation((char('>'), char('='))), |_| {
        RelationalOperator::EqGreaterThan
    })(s)
}

fn operator_eq_lt(s: &str) -> IResult<&str, RelationalOperator> {
    map(permutation((char('<'), char('='))), |_| {
        RelationalOperator::EqLessThan
    })(s)
}

fn operator_not_eq(s: &str) -> IResult<&str, RelationalOperator> {
    map(permutation((char('!'), char('='))), |_| {
        RelationalOperator::NotEquals
    })(s)
}

fn relation_operator(s: &str) -> IResult<&str, RelationalOperator> {
    alt((
        operator_eq,
        operator_eq_gt,
        operator_eq_lt,
        operator_gt,
        operator_lt,
        operator_not_eq,
    ))(s)
}

#[derive(Debug)]
enum PrimaryExpression {
    Literal(Literal),
}

fn primary_expression(s: &str) -> IResult<&str, PrimaryExpression> {
    unimplemented!();
}

#[derive(Debug)]
enum MultipricativeExpression {
    PrimaryExpression(PrimaryExpression),
    MultipricativeExpression {
        left: Box<MultipricativeExpression>,
        right: PrimaryExpression,
        operator: MultiplicativeOperator,
    },
}

fn multiplicative_expression(s: &str) -> IResult<&str, MultipricativeExpression> {
    unimplemented!();
}

#[derive(Debug)]
enum AdditiveExpression {
    MultipricativeExpression(MultipricativeExpression),
    AdditiveExpression {
        left: Box<AdditiveExpression>,
        right: MultipricativeExpression,
        operator: AdditiveOperator,
    },
}

fn additive_expression(s: &str) -> IResult<&str, AdditiveExpression> {
    unimplemented!();
}

#[derive(Debug)]
enum RelationalExpression {
    AdditiveExpression(AdditiveExpression),
    RelationalExpression {
        left: Box<RelationalExpression>,
        right: AdditiveExpression,
        operator: RelationalOperator,
    },
}

fn relational_expression(s: &str) -> IResult<&str, RelationalExpression> {
    unimplemented!();
}

#[derive(Debug)]
enum Expression {
    RelationalExpression(RelationalExpression),
}

fn expression(s: &str) -> IResult<&str, Expression> {
    unimplemented!();
}
