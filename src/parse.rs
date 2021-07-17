use pest;
// use pest::Parser;
use pest_consume::{Nodes, Parser};

/// An error encountered while decoding HCL data.
type Error = pest::error::Error<Rule>;

/// A parser for HCL data.
#[derive(Debug, Parser)]
#[grammar = "hcl.pest"]
pub(crate) struct HclParser;

/// Parse HCL data contained in a string_lit slice.
pub fn parse(input: &str) -> Result<Nodes<Rule, ()>, Error> {
    HclParser::parse(Rule::config_file, input)
}

#[cfg(test)]

mod tests {
    #![allow(non_fmt_panic)]
    use indoc::indoc;
    use pest::Parser;

    use super::*;

    #[derive(Debug, Parser)]
    #[grammar = "hcl.pest"]
    pub(crate) struct PestParser;

    #[test]
    fn parse_true_lit() {
        parses_to! {
            parser: PestParser,
            input: "true",
            rule: Rule::literal_value,
            tokens: [
                literal_value(0, 4, [])
            ]
        }
    }

    #[test]
    fn parse_false_lit() {
        parses_to! {
            parser: PestParser,
            input: "false",
            rule: Rule::literal_value,
            tokens: [
                literal_value(0, 5, [])
            ]
        }
    }

    #[test]
    fn parse_number() {
        parses_to! {
            parser: PestParser,
            input: "0",
            rule: Rule::number,
            tokens: [
                number(0, 1)
            ]
        }
    }

    #[test]
    fn parse_negative_number() {
        parses_to! {
            parser: PestParser,
            input: "-1",
            rule: Rule::number,
            tokens: [
                number(0, 2, [])
            ]
        }
    }

    #[test]
    fn parse_number_zero_point() {
        parses_to! {
            parser: PestParser,
            input: "0.",
            rule: Rule::number,
            tokens: [
                number(0, 2, [])
            ]
        }
    }

    #[test]
    fn parse_number_one_exp() {
        parses_to! {
            parser: PestParser,
            input: "1e10",
            rule: Rule::number,
            tokens: [
                number(0, 4, [])
            ]
        }
    }

    #[test]
    fn parse_string() {
        parses_to! {
            parser: PestParser,
            input: "\"foo\"",
            rule: Rule::string_lit,
            tokens: [
                string_lit(0, 5)
            ]
        }
    }

    #[test]
    fn parse_tuple() {
        macro_rules! assert_fn {
            ($string_lit:expr, $tokens: tt ) => {
                println!("{}", PestParser::parse(Rule::tuple, $string_lit).unwrap());
                parses_to! {
                    parser: PestParser,
                    input: $string_lit,
                    rule: Rule::tuple,
                    tokens: $tokens
                }
            };
        }
        assert_fn!("[]", [tuple(0, 2)]);
        assert_fn!("[true]", [tuple(0, 6, [expr(1, 5, [literal_value(1, 5)])])]);
        assert_fn!(
            indoc! {
            "[
             true
             ]"},
            [tuple(0, 8, [expr(2, 6, [literal_value(2, 6)])])]
        );
        assert_fn!(
            indoc! {
            "[
             true,
             false
             ]"},
            [tuple(
                0,
                15,
                [
                    expr(2, 6, [literal_value(2, 6)]),
                    expr(8, 13, [literal_value(8, 13)])
                ]
            )]
        );
        assert_fn!(
            indoc! {
            "[
             true
             ,false
             ]"},
            [tuple(
                0,
                15,
                [
                    expr(2, 6, [literal_value(2, 6)]),
                    expr(8, 13, [literal_value(8, 13)])
                ]
            )]
        );
        assert_fn!(
            "[1234]",
            [tuple(
                0,
                6,
                [expr(1, 5, [literal_value(1, 5, [number(1, 5)])])]
            )]
        );
        assert_fn!(
            "[\"foo\"]",
            [tuple(
                0,
                7,
                [expr(1, 6, [template_expr(1, 6, [string_lit(1, 6)])])]
            )]
        );
        assert_fn!(
            "[true,]",
            [tuple(0, 7, [expr(1, 5, [literal_value(1, 5, [])])])]
        );
        assert_fn!(
            "[true, true, ]",
            [tuple(
                0,
                14,
                [
                    expr(1, 5, [literal_value(1, 5)]),
                    expr(7, 11, [literal_value(7, 11)])
                ]
            )]
        );
        assert_fn!(
            "[true, 1]",
            [tuple(
                0,
                9,
                [
                    expr(1, 5, [literal_value(1, 5)]),
                    expr(7, 8, [literal_value(7, 8, [number(7, 8)])])
                ]
            )]
        );
        assert_fn!(
            "[true,true]",
            [tuple(
                0,
                11,
                [
                    expr(1, 5, [literal_value(1, 5)]),
                    expr(6, 10, [literal_value(6, 10)])
                ]
            )]
        );
        assert_fn!(
            "[true, true]",
            [tuple(
                0,
                12,
                [
                    expr(1, 5, [literal_value(1, 5)]),
                    expr(7, 11, [literal_value(7, 11)])
                ]
            )]
        );
    }

    #[test]
    fn parse_attribute() {
        macro_rules! assert_fn {
            ($string_lit:expr, $tokens: tt ) => {
                println!("Parsing {:?}", $string_lit);
                println!(
                    "{}",
                    PestParser::parse(Rule::attribute, $string_lit).unwrap()
                );
                parses_to! {
                    parser: PestParser,
                    input: $string_lit,
                    rule: Rule::attribute,
                    tokens: $tokens
                }
            };
        }
        assert_fn!(
            "foo = true",
            [attribute(
                0,
                10,
                [
                    ident(0, 3),
                    expr(6, 10, [literal_value(6, 10)]),
                    EOI(10, 10)
                ]
            )]
        );
    }

    #[test]
    fn parse_configfile() {
        macro_rules! assert_fn {
            ($string_lit:expr, $tokens: tt ) => {
                println!("Parsing {:?}", $string_lit);
                println!(
                    "{}",
                    PestParser::parse(Rule::config_file, $string_lit).unwrap()
                );
                parses_to! {
                    parser: PestParser,
                    input: $string_lit,
                    rule: Rule::config_file,
                    tokens: $tokens
                }
            };
        }
        assert_fn!("", [config_file(0, 0, [body(0, 0), EOI(0, 0)])]);
        assert_fn!(
            "foo = \"1\"\n",
            [config_file(
                0,
                10,
                [
                    body(
                        0,
                        10,
                        [body_element(
                            0,
                            10,
                            [attribute(
                                0,
                                10,
                                [
                                    ident(0, 3),
                                    expr(6, 9, [template_expr(6, 9, [string_lit(6, 9)])])
                                ]
                            )]
                        )]
                    ),
                    EOI(10, 10)
                ]
            )]
        );
    }

    #[test]
    fn parse_oneline_block() {
        macro_rules! assert_fn {
            ($string_lit:expr, $tokens: tt ) => {
                println!("Parsing {:?}", $string_lit);
                println!(
                    "{}",
                    PestParser::parse(Rule::one_line_block, $string_lit).unwrap()
                );
                parses_to! {
                    parser: PestParser,
                    input: $string_lit,
                    rule: Rule::one_line_block,
                    tokens: $tokens
                }
            };
        }
        assert_fn!("foo { }\n", [one_line_block(0, 8, [ident(0, 3)])]);
        assert_fn!(
            "foo { bar = \"baz\" }\n",
            [one_line_block(
                0,
                20,
                [
                    ident(0, 3),
                    ident(6, 9),
                    expr(12, 18, [template_expr(12, 17, [string_lit(12, 17)])])
                ]
            )]
        );
        assert_fn!(
            "foo \"bar\" { baz = \"qux\" }\n",
            [one_line_block(
                0,
                26,
                [
                    ident(0, 3),
                    block_extra_id(4, 9, [string_lit(4, 9)]),
                    ident(12, 15),
                    expr(18, 24, [template_expr(18, 23, [string_lit(18, 23)])])
                ]
            )]
        );
    }

    #[test]
    fn parse_variable_expr() {
        parses_to! {
            parser: PestParser,
            input: "foo",
            rule: Rule::variable_expr,
            tokens: [
                variable_expr(0, 3, [
                ])
            ]
        }
    }

    #[test]
    fn parse_function_expr() {
        macro_rules! assert_fn {
            ($string_lit:expr, $tokens: tt ) => {
                println!(
                    "{}",
                    PestParser::parse(Rule::function_call, $string_lit).unwrap()
                );
                parses_to! {
                    parser: PestParser,
                    input: $string_lit,
                    rule: Rule::function_call,
                    tokens: $tokens
                }
            };
        }
        assert_fn!("foo()", [function_call(0, 5, [ident(0, 3)])]);
        assert_fn!(
            "foo(1)",
            [function_call(
                0,
                6,
                [
                    ident(0, 3),
                    expr_newline(4, 5, [literal_value(4, 5, [number(4, 5)])])
                ]
            )]
        );
        assert_fn!(
            "foo(1,2)",
            [function_call(
                0,
                8,
                [
                    ident(0, 3),
                    expr_newline(4, 5, [literal_value(4, 5, [number(4, 5)])]),
                    expr_newline(6, 7, [literal_value(6, 7, [number(6, 7)])])
                ]
            )]
        );
        assert_fn!(
            "foo(1,2,)",
            [function_call(
                0,
                9,
                [
                    ident(0, 3),
                    expr_newline(4, 5, [literal_value(4, 5, [number(4, 5)])]),
                    expr_newline(6, 7, [literal_value(6, 7, [number(6, 7)])])
                ]
            )]
        );
        assert_fn!(
            "foo(1,[1,2]...)",
            [function_call(
                0,
                15,
                [
                    ident(0, 3),
                    expr_newline(4, 5, [literal_value(4, 5, [number(4, 5)])]),
                    expr_newline(
                        6,
                        11,
                        [collection_value(
                            6,
                            11,
                            [tuple(
                                6,
                                11,
                                [
                                    expr(7, 8, [literal_value(7, 8, [number(7, 8)])]),
                                    expr(9, 10, [literal_value(9, 10, [number(9, 10)])])
                                ]
                            )]
                        )]
                    ),
                    ellipsis(11, 14)
                ]
            )]
        );
    }

    #[test]
    fn parse_for_expr() {
        macro_rules! assert_fn {
            ($string_lit:expr, $tokens: tt ) => {
                println!(
                    "{}",
                    PestParser::parse(Rule::for_expr, $string_lit).unwrap()
                );
                parses_to! {
                    parser: PestParser,
                    input: $string_lit,
                    rule: Rule::for_expr,
                    tokens: $tokens
                }
            };
        }
        assert_fn!(
            "[ for value in []: 1 ]",
            [for_expr(
                0,
                22,
                [for_tuple_expr(
                    0,
                    22,
                    [
                        for_intro(
                            2,
                            18,
                            [
                                ident(6, 11),
                                expr(15, 17, [collection_value(15, 17, [tuple(15, 17)])])
                            ]
                        ),
                        expr(19, 21, [literal_value(19, 20, [number(19, 20)])])
                    ]
                )]
            )]
        );
        assert_fn!(
            "[ for key, value in []: 1 ]",
            [for_expr(
                0,
                27,
                [for_tuple_expr(
                    0,
                    27,
                    [
                        for_intro(
                            2,
                            23,
                            [
                                ident(6, 9),
                                ident(11, 16),
                                expr(20, 22, [collection_value(20, 22, [tuple(20, 22)])])
                            ]
                        ),
                        expr(24, 26, [literal_value(24, 25, [number(24, 25)])])
                    ]
                )]
            )]
        );
        assert_fn!(
            "[ for key, value in [1]: 1 ]",
            [for_expr(
                0,
                28,
                [for_tuple_expr(
                    0,
                    28,
                    [
                        for_intro(
                            2,
                            24,
                            [
                                ident(6, 9),
                                ident(11, 16),
                                expr(
                                    20,
                                    23,
                                    [collection_value(
                                        20,
                                        23,
                                        [tuple(
                                            20,
                                            23,
                                            [expr(
                                                21,
                                                22,
                                                [literal_value(21, 22, [number(21, 22)])]
                                            )]
                                        )]
                                    )]
                                )
                            ]
                        ),
                        expr(25, 27, [literal_value(25, 26, [number(25, 26)])])
                    ]
                )]
            )]
        );
        assert_fn!(
            "[ for key, value in []: 1 if 1 ]",
            [for_expr(
                0,
                32,
                [for_tuple_expr(
                    0,
                    32,
                    [
                        for_intro(
                            2,
                            23,
                            [
                                ident(6, 9),
                                ident(11, 16),
                                expr(20, 22, [collection_value(20, 22, [tuple(20, 22)])])
                            ]
                        ),
                        expr(24, 26, [literal_value(24, 25, [number(24, 25)])]),
                        for_cond(
                            26,
                            31,
                            [expr(29, 31, [literal_value(29, 30, [number(29, 30)])])]
                        )
                    ]
                )]
            )]
        );
        assert_fn!(
            "{ for value in []: 1 => 1}",
            [for_expr(
                0,
                26,
                [for_object_expr(
                    0,
                    26,
                    [
                        for_intro(
                            2,
                            18,
                            [
                                ident(6, 11),
                                expr(15, 17, [collection_value(15, 17, [tuple(15, 17)])])
                            ]
                        ),
                        expr(19, 21, [literal_value(19, 20, [number(19, 20)])]),
                        for_value(
                            24,
                            25,
                            [expr(24, 25, [literal_value(24, 25, [number(24, 25)])])]
                        )
                    ]
                )]
            )]
        );
        assert_fn!(
            "{ for key, value in []: 1 => 1 }",
            [for_expr(
                0,
                32,
                [for_object_expr(
                    0,
                    32,
                    [
                        for_intro(
                            2,
                            23,
                            [
                                ident(6, 9),
                                ident(11, 16),
                                expr(20, 22, [collection_value(20, 22, [tuple(20, 22)])])
                            ]
                        ),
                        expr(24, 26, [literal_value(24, 25, [number(24, 25)])]),
                        for_value(
                            29,
                            30,
                            [expr(29, 30, [literal_value(29, 30, [number(29, 30)])])]
                        )
                    ]
                )]
            )]
        );
        assert_fn!(
            "{ for key, value in [1]: 1 => 1 }",
            [for_expr(
                0,
                33,
                [for_object_expr(
                    0,
                    33,
                    [
                        for_intro(
                            2,
                            24,
                            [
                                ident(6, 9),
                                ident(11, 16),
                                expr(
                                    20,
                                    23,
                                    [collection_value(
                                        20,
                                        23,
                                        [tuple(
                                            20,
                                            23,
                                            [expr(
                                                21,
                                                22,
                                                [literal_value(21, 22, [number(21, 22)])]
                                            )]
                                        )]
                                    )]
                                )
                            ]
                        ),
                        expr(25, 27, [literal_value(25, 26, [number(25, 26)])]),
                        for_value(
                            30,
                            31,
                            [expr(30, 31, [literal_value(30, 31, [number(30, 31)])])]
                        )
                    ]
                )]
            )]
        );
        assert_fn!(
            "{ for key, value in []: 1 => 1 if 1 }",
            [for_expr(
                0,
                37,
                [for_object_expr(
                    0,
                    37,
                    [
                        for_intro(
                            2,
                            23,
                            [
                                ident(6, 9),
                                ident(11, 16),
                                expr(20, 22, [collection_value(20, 22, [tuple(20, 22)])])
                            ]
                        ),
                        expr(24, 26, [literal_value(24, 25, [number(24, 25)])]),
                        for_value(
                            29,
                            30,
                            [expr(29, 30, [literal_value(29, 30, [number(29, 30)])])]
                        ),
                        for_cond(
                            31,
                            36,
                            [expr(34, 36, [literal_value(34, 35, [number(34, 35)])])]
                        )
                    ]
                )]
            )]
        );
        assert_fn!(
            "{ for key, value in []: 1 => 1... }",
            [for_expr(
                0,
                35,
                [for_object_expr(
                    0,
                    35,
                    [
                        for_intro(
                            2,
                            23,
                            [
                                ident(6, 9),
                                ident(11, 16),
                                expr(20, 22, [collection_value(20, 22, [tuple(20, 22)])])
                            ]
                        ),
                        expr(24, 26, [literal_value(24, 25, [number(24, 25)])]),
                        for_value(
                            29,
                            33,
                            [
                                expr(29, 30, [literal_value(29, 30, [number(29, 30)])]),
                                ellipsis(30, 33)
                            ]
                        )
                    ]
                )]
            )]
        );
        assert_fn!(
            "{ for key, value in []: 1 => 1... if 1 }",
            [for_expr(
                0,
                40,
                [for_object_expr(
                    0,
                    40,
                    [
                        for_intro(
                            2,
                            23,
                            [
                                ident(6, 9),
                                ident(11, 16),
                                expr(20, 22, [collection_value(20, 22, [tuple(20, 22)])])
                            ]
                        ),
                        expr(24, 26, [literal_value(24, 25, [number(24, 25)])]),
                        for_value(
                            29,
                            33,
                            [
                                expr(29, 30, [literal_value(29, 30, [number(29, 30)])]),
                                ellipsis(30, 33)
                            ]
                        ),
                        for_cond(
                            34,
                            39,
                            [expr(37, 39, [literal_value(37, 38, [number(37, 38)])])]
                        )
                    ]
                )]
            )]
        );
    }

    #[test]
    fn parse_object() {
        macro_rules! assert_fn {
            ($string_lit:expr, $tokens: tt ) => {
                println!("{}", PestParser::parse(Rule::object, $string_lit).unwrap());
                parses_to! {
                    parser: PestParser,
                    input: $string_lit,
                    rule: Rule::object,
                    tokens: $tokens
                }
            };
        }
        macro_rules! assert_fails {
            ($string_lit:expr, $positives: expr, $negatives: expr, $pos: expr ) => {
                println!("Should fail {:?}", $string_lit);
                fails_with! {
                    parser: PestParser,
                    input: $string_lit,
                    rule: Rule::object,
                    positives: $positives,
                    negatives: $negatives,
                    pos: $pos
                }
            };
        }
        // {}, {1:2}, {1:2,2=3}, {1:2,}, {2=3,} {1=2\n3=4}
        assert_fn!("{}", [object(0, 2)]);
        assert_fn!(
            "{1:2}",
            [object(
                0,
                5,
                [object_elem(
                    1,
                    4,
                    [
                        expr(1, 2, [literal_value(1, 2, [number(1, 2)])]),
                        expr(3, 4, [literal_value(3, 4, [number(3, 4)])])
                    ]
                )]
            )]
        );
        assert_fn!(
            "{ 1 : 2 }",
            [object(
                0,
                9,
                [object_elem(
                    2,
                    8,
                    [
                        expr(2, 4, [literal_value(2, 3, [number(2, 3)])]),
                        expr(6, 8, [literal_value(6, 7, [number(6, 7)])])
                    ]
                )]
            )]
        );
        assert_fn!(
            "{1:2,2=3}",
            [object(
                0,
                9,
                [
                    object_elem(
                        1,
                        4,
                        [
                            expr(1, 2, [literal_value(1, 2, [number(1, 2)])]),
                            expr(3, 4, [literal_value(3, 4, [number(3, 4)])])
                        ]
                    ),
                    object_elem(
                        5,
                        8,
                        [
                            expr(5, 6, [literal_value(5, 6, [number(5, 6)])]),
                            expr(7, 8, [literal_value(7, 8, [number(7, 8)])])
                        ]
                    )
                ]
            )]
        );
        assert_fn!(
            "{1:2,}",
            [object(
                0,
                6,
                [object_elem(
                    1,
                    4,
                    [
                        expr(1, 2, [literal_value(1, 2, [number(1, 2)])]),
                        expr(3, 4, [literal_value(3, 4, [number(3, 4)])])
                    ]
                )]
            )]
        );
        assert_fn!(
            "{2=3,}",
            [object(
                0,
                6,
                [object_elem(
                    1,
                    4,
                    [
                        expr(1, 2, [literal_value(1, 2, [number(1, 2)])]),
                        expr(3, 4, [literal_value(3, 4, [number(3, 4)])])
                    ]
                )]
            )]
        );
        // Note that this is out of spec: it is a block, not an object per the grammar and prose
        // but the hcl go parse accepts it as an expression so we must.
        assert_fn!(
            indoc! {"
            {
                1=2
                2=3
            }"},
            [object(
                0,
                19,
                [
                    object_elem(
                        6,
                        9,
                        [
                            expr(6, 7, [literal_value(6, 7, [number(6, 7)])]),
                            expr(8, 9, [literal_value(8, 9, [number(8, 9)])])
                        ]
                    ),
                    object_elem(
                        14,
                        17,
                        [
                            expr(14, 15, [literal_value(14, 15, [number(14, 15)])]),
                            expr(16, 17, [literal_value(16, 17, [number(16, 17)])])
                        ]
                    )
                ]
            )]
        );
        assert_fails!(
            indoc! {"
        { 1
        =
        2}"},
            vec![Rule::index],
            vec![],
            3
        );
        assert_fails!(
            indoc! {"
        { 1 =
        2}"},
            vec![Rule::expr],
            vec![],
            5
        );
        assert_fails!(
            indoc! {"
            {
            1 = 2
            ,
            3 = 4
            }"},
            vec![Rule::object_elem],
            vec![],
            8
        );
        assert_fails!(
            indoc! {"
            {
            1 = 2
            , 3 = 4
            }"},
            vec![Rule::object_elem],
            vec![],
            8
        );
        assert_fails!(
            indoc! {"
            object_4 = {
            1=2 3=4
            }"},
            vec![Rule::object],
            vec![],
            0
        );
    }

    #[test]
    fn parse_object_elem() {
        macro_rules! assert_fn {
            ($string_lit:expr, $tokens: tt ) => {
                println!(
                    "{}",
                    PestParser::parse(Rule::object_elem, $string_lit).unwrap()
                );
                parses_to! {
                    parser: PestParser,
                    input: $string_lit,
                    rule: Rule::object_elem,
                    tokens: $tokens
                }
            };
        }
        // 1:2, (foo)=[4],
        assert_fn!(
            "1:2",
            [object_elem(
                0,
                3,
                [
                    expr(0, 1, [literal_value(0, 1, [number(0, 1)])]),
                    expr(2, 3, [literal_value(2, 3, [number(2, 3)])])
                ]
            )]
        );
        assert_fn!(
            "(foo)=[4]",
            [object_elem(
                0,
                9,
                [
                    expr(0, 5, [expr_newline(1, 4, [variable_expr(1, 4)])]),
                    expr(
                        6,
                        9,
                        [collection_value(
                            6,
                            9,
                            [tuple(
                                6,
                                9,
                                [expr(7, 8, [literal_value(7, 8, [number(7, 8)])])]
                            )]
                        )]
                    )
                ]
            )]
        );
    }

    #[test]
    fn parse_collection_value() {
        macro_rules! assert_fn {
            ($string_lit:expr, $tokens: tt ) => {
                println!(
                    "{}",
                    PestParser::parse(Rule::collection_value, $string_lit).unwrap()
                );
                parses_to! {
                    parser: PestParser,
                    input: $string_lit,
                    rule: Rule::collection_value,
                    tokens: $tokens
                }
            };
        }
        // {}, [],
        assert_fn!("{}", [collection_value(0, 2, [object(0, 2)])]);
        assert_fn!("[]", [collection_value(0, 2, [tuple(0, 2)])]);
    }

    #[test]
    fn parse_expr_term() {
        macro_rules! assert_fn {
            ($string_lit:expr, $tokens: tt ) => {
                println!(
                    "{}",
                    PestParser::parse(Rule::expr_term, $string_lit).unwrap()
                );
                parses_to! {
                    parser: PestParser,
                    input: $string_lit,
                    rule: Rule::expr_term,
                    tokens: $tokens
                }
            };
        }
        // {}, [], 1
        assert_fn!("{}", [collection_value(0, 2, [object(0, 2)])]);
        assert_fn!("[]", [collection_value(0, 2, [tuple(0, 2)])]);
        assert_fn!("1", [literal_value(0, 1, [number(0, 1)])]);
    }

    #[test]
    fn parse_expr() {
        macro_rules! assert_fn {
            ($string_lit:expr, $tokens: tt ) => {
                println!(
                    "{}",
                    PestParser::parse(Rule::expr_term, $string_lit).unwrap()
                );
                parses_to! {
                    parser: PestParser,
                    input: $string_lit,
                    rule: Rule::expr_term,
                    tokens: $tokens
                }
            };
        }
        // {}, [], 1
        assert_fn!("{}", [collection_value(0, 2, [object(0, 2)])]);
        assert_fn!("[]", [collection_value(0, 2, [tuple(0, 2)])]);
        assert_fn!("1", [literal_value(0, 1, [number(0, 1)])]);
    }
}
