use std::{
    collections::BTreeMap,
    convert::{TryFrom, TryInto},
    mem,
    str::FromStr,
};

use pest_consume::{match_nodes, Error};
type ParseResult<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;
// type ParseResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

use crate::parse::{HclParser, Rule};

#[pest_consume::parser]
impl HclParser {
    fn attribute(input: Node) -> ParseResult<Structure> {
        Ok(match_nodes!(input.into_children();
            [ident(ident), expr(expr)] => Structure::Attribute{ident, expr},
            [ident(ident), expr(expr), EOI(_)] => Structure::Attribute{ident, expr},
        ))
    }

    fn block(input: Node) -> ParseResult<Structure> {
        Ok(match_nodes!(input.into_children();
        [ident(ident), block_extra_id(extra_ids).., body(body)] => Structure::Block{ident, extra_ids:extra_ids.collect(), body},
        [ident(ident), block_extra_id(extra_ids).., body(body), EOI(_)] => Structure::Block{ident, extra_ids:extra_ids.collect(), body},))
    }

    fn block_extra_id(input: Node) -> ParseResult<String> {
        Ok(match_nodes!(input.into_children();
                [ident(ident)] => ident,
                [string_lit(s)] => s
        ))
    }

    fn body(input: Node) -> ParseResult<Vec<Structure>> {
        Ok(match_nodes!(input.into_children();
        [body_element(bes)..] => bes.collect()))
    }

    fn body_element(input: Node) -> ParseResult<Structure> {
        Ok(match_nodes!(input.into_children();
            [attribute(a)] => a,
            [block(b)] => b,
            [one_line_block(b)] => b,
        ))
    }

    pub fn config_file(input: Node) -> ParseResult<ConfigFile> {
        Ok(match_nodes!(input.into_children();
        [body(body), EOI(_)] => ConfigFile{body}))
    }

    pub(crate) fn collection_value(input: Node) -> ParseResult<Expression> {
        // CollectionValue = tuple | object;
        Ok(match_nodes!(input.into_children();
            [tuple(t)] => Expression::Tuple(t),
            [object(o)] => o,
            [x] => unreachable!("{:?}", x)
        ))
    }

    fn COMMENT(input: Node) -> ParseResult<String> {
        Ok(input.as_str().into())
    }

    fn EOI(input: Node) -> ParseResult<()> {
        Ok(())
    }

    pub(crate) fn expr(input: Node) -> ParseResult<Expression> {
        // Expression = (
        //     ExprTerm |
        //     Operation |
        //     Conditional
        // );
        Ok(match_nodes!(input.into_children();
            [literal_value(v)]=>Expression::LiteralValue(v),
            [literal_value(v), index(i)]=>Expression::IndexedExpression(vec![Expression::LiteralValue(v),i]),
            [collection_value(c)] => c,
            [collection_value(c), index(i)] => Expression::IndexedExpression(vec![c,i]),
            [template_expr(t)] => Expression::TemplateExpr(t),
            [template_expr(t), index(i)] => Expression::IndexedExpression(vec![Expression::TemplateExpr(t),i]),
            [function_call(c)] => Expression::Call(c),
            [function_call(c), index(i)] => Expression::IndexedExpression(vec![Expression::Call(c),i]),
            [for_expr(f)] => Expression::ForExpr(vec![f]),
            [for_expr(f), index(i)] => Expression::IndexedExpression(vec![Expression::ForExpr(vec![f]),i]),
            [variable_expr(v)] => Expression::VariableExpr(v),
            [variable_expr(v), index(i)] => Expression::IndexedExpression(vec![Expression::VariableExpr(v),i]),
            [expr_newline(e)] => Expression::BracketExpression(vec![e]),
            [expr_newline(e), index(i)] => Expression::IndexedExpression(vec![Expression::BracketExpression(vec![e]),i]),
            [x] => unreachable!("{:?}", x)
        ))
    }

    pub(crate) fn expr_newline(input: Node) -> ParseResult<Expression> {
        // Expression = (
        //     ExprTerm |
        //     Operation |
        //     Conditional
        // );
        Ok(match_nodes!(input.into_children();
        [literal_value(v)]=>Expression::LiteralValue(v),
        [literal_value(v), index_newline(i)]=>Expression::IndexedExpression(vec![Expression::LiteralValue(v),i]),
        [collection_value(c)] => c,
        [collection_value(c), index_newline(i)] => Expression::IndexedExpression(vec![c,i]),
        [template_expr(t)] => Expression::TemplateExpr(t),
        [template_expr(t), index_newline(i)] => Expression::IndexedExpression(vec![Expression::TemplateExpr(t),i]),
        [function_call_newline(c)] => Expression::Call(c),
        [function_call_newline(c), index_newline(i)] => Expression::IndexedExpression(vec![Expression::Call(c),i]),
        [for_expr(f)] => Expression::ForExpr(vec![f]),
        [for_expr(f), index_newline(i)] => Expression::IndexedExpression(vec![Expression::ForExpr(vec![f]),i]),
        [variable_expr(v)] => Expression::VariableExpr(v),
        [variable_expr(v), index_newline(i)] => Expression::IndexedExpression(vec![Expression::VariableExpr(v),i]),
        [expr_newline(e)] => Expression::BracketExpression(vec![e]),
        [expr_newline(e), index_newline(i)] => Expression::IndexedExpression(vec![Expression::BracketExpression(vec![e]),i]),

        [x] => unreachable!("{:?}", x)
        ))
    }

    pub(crate) fn ellipsis(input: Node) -> ParseResult<()> {
        Ok(())
    }

    pub(crate) fn for_expr(input: Node) -> ParseResult<ForExpr> {
        Ok(match_nodes!(input.into_children();
                    [for_tuple_expr(tuple)]=>tuple,
                    [for_object_expr(object)]=>object))
    }

    pub(crate) fn for_tuple_expr(input: Node) -> ParseResult<ForExpr> {
        Ok(match_nodes!(input.into_children();
        [for_intro((binding, iterable)), expr(assignment)] => ForExpr{
            binding, iterable, assignment: ForAssignment::Tuple(assignment), condition: None},
            [for_intro((binding, iterable)), expr(assignment), for_cond(condition)] => ForExpr{
                binding, iterable, assignment: ForAssignment::Tuple(assignment), condition:Some(condition)},
            ))
    }

    pub(crate) fn for_object_expr(input: Node) -> ParseResult<ForExpr> {
        Ok(match_nodes!(input.into_children();
        [for_intro((binding, iterable)), expr(key), for_value((value, ellipsis))] => ForExpr{
            binding, iterable, assignment:ForAssignment::Object(key, value, ellipsis), condition: None},
        [for_intro((binding, iterable)), expr(key), for_value((value, ellipsis)), for_cond(condition)] => ForExpr{
            binding, iterable, assignment: ForAssignment::Object(key, value, ellipsis), condition:Some(condition)},
        ))
    }

    pub(crate) fn for_cond(input: Node) -> ParseResult<Expression> {
        Ok(match_nodes!(input.into_children();
    [expr(expression)]=> expression))
    }

    fn for_intro(input: Node) -> ParseResult<(ForBinding, Expression)> {
        Ok(match_nodes!(input.into_children();
            [ident(key), ident(value), expr (e) ]=>(ForBinding::KeyValue(key, value), e),
            [ident(value), expr (e) ]=>(ForBinding::Value(value), e),

        ))
    }

    fn for_value(input: Node) -> ParseResult<(Expression, bool)> {
        Ok(match_nodes!(input.into_children();
            [expr(value), ellipsis(_) ]=>(value, true),
            [expr(value) ]=>(value, false),

        ))
    }

    fn function_call(input: Node) -> ParseResult<FunctionCall> {
        Ok(match_nodes!(input.into_children();
        [ident(name), expr_newline(es).., ellipsis(_)]=>FunctionCall{name, arguments:es.collect(), ellipsis:true},
        [ident(name), expr_newline(es)..]=>FunctionCall{name, arguments:es.collect(), ellipsis:false},
        ))
    }

    fn function_call_newline(input: Node) -> ParseResult<FunctionCall> {
        Ok(match_nodes!(input.into_children();
        [ident(name), expr_newline(es).., ellipsis(_)]=>FunctionCall{name, arguments:es.collect(), ellipsis:true},
        [ident(name), expr_newline(es)..]=>FunctionCall{name, arguments:es.collect(), ellipsis:false},
        ))
    }

    fn heredoc(input: Node) -> ParseResult<String> {
        Ok(match_nodes!(input.into_children();
        [heredoc_body(body)] => body
        ))
    }

    fn heredoc_body(input: Node) -> ParseResult<String> {
        Ok(input.as_str().into())
    }

    fn hcl_index(input: Node) -> ParseResult<Expression> {
        Ok(match_nodes!(input.into_children();
        [expr(e)] => e,
        ))
    }

    fn ident(input: Node) -> ParseResult<String> {
        Ok(input.as_str().into())
    }

    fn index(input: Node) -> ParseResult<Expression> {
        Ok(match_nodes!(input.into_children();
        [hcl_index(e)] => e,
        [legacy_index(i)] => i,
        ))
    }

    fn index_newline(input: Node) -> ParseResult<Expression> {
        Ok(match_nodes!(input.into_children();
        [hcl_index(e)] => e,
        [legacy_index_newline(i)] => i,
        ))
    }

    fn legacy_index(input: Node) -> ParseResult<Expression> {
        Ok(Expression::TemplateExpr(TemplateExpr::Quoted(
            input.as_str().into(),
        )))
    }

    fn legacy_index_newline(input: Node) -> ParseResult<Expression> {
        Ok(Expression::TemplateExpr(TemplateExpr::Quoted(
            input.as_str().into(),
        )))
    }

    pub(crate) fn literal_value(input: Node) -> ParseResult<LiteralValue> {
        Ok(match input.as_str() {
            "true" => LiteralValue::True,
            "false" => LiteralValue::False,
            "null" => LiteralValue::Null,
            as_str => LiteralValue::NumericLiteral(as_str.parse().map_err(|e| input.error(e))?),
        })
    }

    pub(crate) fn object(input: Node) -> ParseResult<Expression> {
        // object = "{" (
        //     (objectelem ("," objectelem)* ","?)?
        // ) "}";
        Ok(match_nodes!(input.into_children();
            [object_elem(elems)..] => Expression::Object(elems.collect::<BTreeMap<ObjectKey,Expression>>())
        ))
    }

    pub(crate) fn object_elem(input: Node) -> ParseResult<(ObjectKey, Expression)> {
        // objectelem = (Identifier | Expression) ("=" | ":") Expression;
        Ok(match_nodes!(input.into_children();
            [ident(key), expr(value)] => (ObjectKey::Ident(key), value),
            [expr(key), expr(value)] => (ObjectKey::Expression(key), value),
        ))
    }

    fn one_line_block(input: Node) -> ParseResult<Structure> {
        Ok(match_nodes!(input.into_children();
            [ident(ident), block_extra_id(extra_ids).., ident(attr), expr(expr)] => Structure::Block{ident, extra_ids:extra_ids.collect(), body:vec![Structure::Attribute{ident:attr, expr}]},
            [ident(ident), block_extra_id(extra_ids)..] => Structure::OneLineBlock{ident, extra_ids:extra_ids.collect(), body:vec![]},
            [ident(ident), block_extra_id(extra_ids).., ident(attr), expr(expr), EOI(_)] => Structure::Block{ident, extra_ids:extra_ids.collect(), body:vec![Structure::Attribute{ident:attr, expr}]},
            [ident(ident), block_extra_id(extra_ids).., EOI(_)] => Structure::OneLineBlock{ident, extra_ids:extra_ids.collect(), body:vec![]},
        ))
    }
    fn string_lit(input: Node) -> ParseResult<String> {
        let as_str = input.as_str();
        Ok(as_str[1..as_str.len() - 1].to_owned())
    }

    fn template_expr(input: Node) -> ParseResult<TemplateExpr> {
        Ok(match_nodes!(input.into_children();
        [string_lit(s)] => TemplateExpr::Quoted(s),
        [heredoc(d)] => TemplateExpr::HereDoc(d),
        ))
    }

    fn tuple(input: Node) -> ParseResult<Vec<Expression>> {
        // tuple = "[" (
        //     (Expression ("," Expression)* ","?)?
        // ) "]";
        Ok(match_nodes!(input.into_children();
            [expr(es)..] => es.collect()
        ))
    }

    fn variable_expr(input: Node) -> ParseResult<VariableExpr> {
        Ok(VariableExpr(input.as_str().to_owned()))
    }
}

#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ConfigFile {
    pub body: Vec<Structure>,
}

#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Structure {
    Attribute {
        ident: String,
        expr: Expression,
    },
    Block {
        ident: String,
        extra_ids: Vec<String>,
        body: Vec<Structure>,
    },
    OneLineBlock {
        ident: String,
        extra_ids: Vec<String>,
        body: Vec<Structure>,
    },
}

/// An [ObjectItem] key, which be either a bare identifier or a string.
#[derive(Debug)]
pub enum Key {
    /// A bare identifier.
    Ident(String),
    /// A double-quoted string.
    String(String),
}

#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
/// Number matching the HCL needs: basically float but no NaN or infinity.
pub struct Number {
    mantissa: u64,
    exponent: i16,
    sign: i8,
}

impl Number {
    // derived from https://github.com/rust-lang/rust/blob/5c674a11471ec0569f616854d715941757a48a0a/src/libcore/num/f64.rs#L203-L216
    fn integer_decode(source: f64) -> (u64, i16, i8) {
        let bits: u64 = unsafe { mem::transmute(source) };
        let sign: i8 = if bits >> 63 == 0 { 1 } else { -1 };
        let mut exponent: i16 = ((bits >> 52) & 0x7ff) as i16;
        let mantissa = if exponent == 0 {
            (bits & 0xfffffffffffff) << 1
        } else {
            (bits & 0xfffffffffffff) | 0x10000000000000
        };
        // Exponent bias + mantissa shift
        exponent -= 1023 + 52;
        (mantissa, exponent, sign)
    }
}

impl FromStr for Number {
    type Err = Box<dyn std::error::Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(s.parse::<f64>()?.try_into()?)
    }
}

impl TryFrom<f64> for Number {
    type Error = &'static str;

    fn try_from(source: f64) -> Result<Self, Self::Error> {
        let (mantissa, exponent, sign) = Number::integer_decode(source);
        // NaN and Infinity
        if exponent == 0x7ff {
            match mantissa {
                0 => return Err("Infinity is not a valid HCL number"),
                _ => return Err("NaN is not a valid HCL number"),
            }
        }
        Ok(Self {
            mantissa,
            exponent,
            sign,
        })
    }
}

#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum LiteralValue {
    NumericLiteral(Number),
    True,
    False,
    Null,
}

#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum TemplateExpr {
    Quoted(String),
    HereDoc(String),
}

#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct VariableExpr(String);

#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct FunctionCall {
    name: String,
    arguments: Vec<Expression>,
    ellipsis: bool,
}

#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ObjectKey {
    Ident(String),
    Expression(Expression),
}

#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Expression {
    BracketExpression(Vec<Expression>),
    Call(FunctionCall),
    ForExpr(Vec<ForExpr>),
    IndexedExpression(Vec<Expression>),
    LiteralValue(LiteralValue),
    Object(BTreeMap<ObjectKey, Expression>),
    TemplateExpr(TemplateExpr),
    Tuple(Vec<Expression>),
    VariableExpr(VariableExpr),
}

#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ForBinding {
    Value(String),
    KeyValue(String, String),
}

#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ForAssignment {
    Tuple(Expression),
    Object(Expression, Expression, bool),
}

#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ForExpr {
    binding: ForBinding,
    iterable: Expression,
    assignment: ForAssignment,
    condition: Option<Expression>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::{HclParser, Rule};

    #[test]
    fn parse_literal_value() -> Result<(), Box<dyn std::error::Error>> {
        fn assert_input(
            input: &str,
            expected: LiteralValue,
        ) -> Result<(), Box<dyn std::error::Error>> {
            assert_eq!(
                expected,
                HclParser::literal_value(
                    <HclParser as pest_consume::Parser>::parse(Rule::literal_value, input)?
                        .next()
                        .unwrap()
                )?
            );
            Ok(())
        }
        assert_input("true", LiteralValue::True)?;
        assert_input("false", LiteralValue::False)?;
        assert_input("null", LiteralValue::Null)?;
        assert_input("1", LiteralValue::NumericLiteral((1.).try_into()?))?;
        assert_input("1.0e10", LiteralValue::NumericLiteral(1.0e10.try_into()?))
    }

    #[test]
    fn parse_template_expr() -> Result<(), Box<dyn std::error::Error>> {
        #[track_caller]
        fn assert_input(
            input: &str,
            expected: TemplateExpr,
        ) -> Result<(), Box<dyn std::error::Error>> {
            assert_eq!(
                expected,
                HclParser::template_expr(
                    <HclParser as pest_consume::Parser>::parse(Rule::template_expr, input)
                        .unwrap()
                        .next()
                        .unwrap()
                )?
            );
            Ok(())
        }
        assert_input(
            r#"<<-Foo
bar
baz
Foo
"#,
            TemplateExpr::HereDoc(
                r#"bar
baz
"#
                .into(),
            ),
        )?;
        assert_input("\"true\"", TemplateExpr::Quoted("true".into()))
    }

    #[test]
    fn parse_variable_expr() -> Result<(), Box<dyn std::error::Error>> {
        fn assert_input(
            input: &str,
            expected: VariableExpr,
        ) -> Result<(), Box<dyn std::error::Error>> {
            assert_eq!(
                expected,
                HclParser::variable_expr(
                    <HclParser as pest_consume::Parser>::parse(Rule::variable_expr, input)
                        .unwrap()
                        .next()
                        .unwrap()
                )?
            );
            Ok(())
        }
        assert_input("foo", VariableExpr("foo".into()))
    }

    #[test]
    fn parse_function_call() -> Result<(), Box<dyn std::error::Error>> {
        fn assert_input(
            input: &str,
            expected: FunctionCall,
        ) -> Result<(), Box<dyn std::error::Error>> {
            assert_eq!(
                expected,
                HclParser::function_call(
                    <HclParser as pest_consume::Parser>::parse(Rule::function_call, input)
                        .unwrap()
                        .next()
                        .unwrap()
                )?
            );
            Ok(())
        }
        assert_input(
            "foo()",
            FunctionCall {
                name: "foo".into(),
                arguments: vec![],
                ellipsis: false,
            },
        )?;
        assert_input(
            "foo(1)",
            FunctionCall {
                name: "foo".into(),
                arguments: vec![Expression::LiteralValue(LiteralValue::NumericLiteral(
                    (1.).try_into()?,
                ))],
                ellipsis: false,
            },
        )?;
        assert_input(
            "foo(1,2)",
            FunctionCall {
                name: "foo".into(),
                arguments: vec![
                    Expression::LiteralValue(LiteralValue::NumericLiteral((1.).try_into()?)),
                    Expression::LiteralValue(LiteralValue::NumericLiteral((2.).try_into()?)),
                ],
                ellipsis: false,
            },
        )?;
        assert_input(
            "foo(1,2,)",
            FunctionCall {
                name: "foo".into(),
                arguments: vec![
                    Expression::LiteralValue(LiteralValue::NumericLiteral((1.).try_into()?)),
                    Expression::LiteralValue(LiteralValue::NumericLiteral((2.).try_into()?)),
                ],
                ellipsis: false,
            },
        )?;
        assert_input(
            "foo(1,[1,2]...)",
            FunctionCall {
                name: "foo".into(),
                arguments: vec![
                    Expression::LiteralValue(LiteralValue::NumericLiteral((1.).try_into()?)),
                    Expression::Tuple(vec![
                        Expression::LiteralValue(LiteralValue::NumericLiteral((1.).try_into()?)),
                        Expression::LiteralValue(LiteralValue::NumericLiteral((2.).try_into()?)),
                    ]),
                ],
                ellipsis: true,
            },
        )
    }

    #[test]
    fn parse_for_expr() -> Result<(), Box<dyn std::error::Error>> {
        #[track_caller]
        fn assert_input(input: &str, expected: ForExpr) -> Result<(), Box<dyn std::error::Error>> {
            assert_eq!(
                expected,
                HclParser::for_expr(
                    <HclParser as pest_consume::Parser>::parse(Rule::for_expr, input)
                        .unwrap()
                        .next()
                        .unwrap()
                )?,
                "failed on {:?}",
                input
            );
            Ok(())
        }
        assert_input(
            "[ for value in 1: 1 ]",
            ForExpr {
                binding: ForBinding::Value("value".into()),
                iterable: Expression::LiteralValue(LiteralValue::NumericLiteral((1.).try_into()?)),
                assignment: ForAssignment::Tuple(Expression::LiteralValue(
                    LiteralValue::NumericLiteral((1.).try_into()?),
                )),
                condition: None,
            },
        )?;
        assert_input(
            "[ for key, value in 1: 1 ]",
            ForExpr {
                binding: ForBinding::KeyValue("key".into(), "value".into()),
                iterable: Expression::LiteralValue(LiteralValue::NumericLiteral((1.).try_into()?)),
                assignment: ForAssignment::Tuple(Expression::LiteralValue(
                    LiteralValue::NumericLiteral((1.).try_into()?),
                )),
                condition: None,
            },
        )?;
        assert_input(
            "[ for key, value in [1]: 1 ]",
            ForExpr {
                binding: ForBinding::KeyValue("key".into(), "value".into()),
                iterable: Expression::Tuple(vec![Expression::LiteralValue(
                    LiteralValue::NumericLiteral((1.).try_into()?),
                )]),
                assignment: ForAssignment::Tuple(Expression::LiteralValue(
                    LiteralValue::NumericLiteral((1.).try_into()?),
                )),
                condition: None,
            },
        )?;
        assert_input(
            "[ for key, value in []: 1 if 1 ]",
            ForExpr {
                binding: ForBinding::KeyValue("key".into(), "value".into()),
                iterable: Expression::Tuple(vec![]),
                assignment: ForAssignment::Tuple(Expression::LiteralValue(
                    LiteralValue::NumericLiteral((1.).try_into()?),
                )),
                condition: Some(Expression::LiteralValue(LiteralValue::NumericLiteral(
                    (1.).try_into()?,
                ))),
            },
        )?;
        assert_input(
            "{ for value in []: 1 => 1}",
            ForExpr {
                binding: ForBinding::Value("value".into()),
                iterable: Expression::Tuple(vec![]),
                assignment: ForAssignment::Object(
                    Expression::LiteralValue(LiteralValue::NumericLiteral((1.).try_into()?)),
                    Expression::LiteralValue(LiteralValue::NumericLiteral((1.).try_into()?)),
                    false,
                ),
                condition: None,
            },
        )?;
        assert_input(
            "{ for key, value in []: 1 => 1 }",
            ForExpr {
                binding: ForBinding::KeyValue("key".into(), "value".into()),
                iterable: Expression::Tuple(vec![]),
                assignment: ForAssignment::Object(
                    Expression::LiteralValue(LiteralValue::NumericLiteral((1.).try_into()?)),
                    Expression::LiteralValue(LiteralValue::NumericLiteral((1.).try_into()?)),
                    false,
                ),
                condition: None,
            },
        )?;
        assert_input(
            "{ for key, value in [1]: 1 => 1 }",
            ForExpr {
                binding: ForBinding::KeyValue("key".into(), "value".into()),
                iterable: Expression::Tuple(vec![Expression::LiteralValue(
                    LiteralValue::NumericLiteral((1.).try_into()?),
                )]),
                assignment: ForAssignment::Object(
                    Expression::LiteralValue(LiteralValue::NumericLiteral((1.).try_into()?)),
                    Expression::LiteralValue(LiteralValue::NumericLiteral((1.).try_into()?)),
                    false,
                ),
                condition: None,
            },
        )?;
        assert_input(
            "{ for key, value in []: 1 => 1 if 1 }",
            ForExpr {
                binding: ForBinding::KeyValue("key".into(), "value".into()),
                iterable: Expression::Tuple(vec![]),
                assignment: ForAssignment::Object(
                    Expression::LiteralValue(LiteralValue::NumericLiteral((1.).try_into()?)),
                    Expression::LiteralValue(LiteralValue::NumericLiteral((1.).try_into()?)),
                    false,
                ),
                condition: Some(Expression::LiteralValue(LiteralValue::NumericLiteral(
                    (1.).try_into()?,
                ))),
            },
        )?;
        assert_input(
            "{ for key, value in []: 1 => 1...  }",
            ForExpr {
                binding: ForBinding::KeyValue("key".into(), "value".into()),
                iterable: Expression::Tuple(vec![]),
                assignment: ForAssignment::Object(
                    Expression::LiteralValue(LiteralValue::NumericLiteral((1.).try_into()?)),
                    Expression::LiteralValue(LiteralValue::NumericLiteral((1.).try_into()?)),
                    true,
                ),
                condition: None,
            },
        )?;
        assert_input(
            "{ for key, value in []: 1 => 1... if 1 }",
            ForExpr {
                binding: ForBinding::KeyValue("key".into(), "value".into()),
                iterable: Expression::Tuple(vec![]),
                assignment: ForAssignment::Object(
                    Expression::LiteralValue(LiteralValue::NumericLiteral((1.).try_into()?)),
                    Expression::LiteralValue(LiteralValue::NumericLiteral((1.).try_into()?)),
                    true,
                ),
                condition: Some(Expression::LiteralValue(LiteralValue::NumericLiteral(
                    (1.).try_into()?,
                ))),
            },
        )?;
        #[track_caller]
        fn assert_syntax_error<'a>(input: &'a str) -> Result<(), Box<dyn std::error::Error>> {
            // Should fail to parse as either a collection or a for-expression

            for (rule, hrule) in [
                (
                    Rule::for_expr,
                    Box::new(move |i| HclParser::for_expr(i).map(|_| ()))
                        as Box<dyn Fn(pest_consume::Node<'a, Rule, ()>) -> ParseResult<()> + 'a>,
                ),
                (
                    Rule::collection_value,
                    Box::new(move |i| HclParser::collection_value(i).map(|_| ()))
                        as Box<dyn Fn(pest_consume::Node<'a, Rule, ()>) -> ParseResult<()> + 'a>,
                ),
            ] {
                let nodes = <HclParser as pest_consume::Parser>::parse(rule, input);
                if nodes.is_err() {
                    continue;
                } else {
                    let parse_result = hrule(nodes.unwrap().single()?);
                    assert_eq!(
                        true,
                        parse_result.is_err(),
                        "succeed parsing! {:?} -> {:?}",
                        input,
                        parse_result
                    );
                }
            }
            Ok(())
        }
        // from hcl.md:
        assert_syntax_error("[for, foo, baz]")?;
        // [(for), foo, baz] is a tuple whose first element is the value of variable for.
        assert_syntax_error("{for = 1, baz = 2}")
        // {"for" = 1, baz = 2} is an object with an attribute literally named for.
        // {baz = 2, for = 1} is equivalent to the previous example, and resolves the ambiguity by reordering.
        // {(for) = 1, baz = 2} is an object with a key with the same value as the variable for.
    }
}
