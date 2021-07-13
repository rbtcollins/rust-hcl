use pest;
use pest::iterators::Pairs;
use pest::Parser;

/// An error encountered while decoding HCL data.
#[derive(Debug)]
pub enum Error {
    Pest(pest::error::Error<Rule>),
}

/// A parser for HCL data.
#[derive(Debug, Parser)]
#[grammar = "hcl.pest"]
struct HclParser;

/// Parse HCL data contained in a string slice.
pub fn parse(input: &str) -> Result<Pairs<Rule>, Error> {
    HclParser::parse(Rule::hcl, input).map_err(|error| Error::Pest(error))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_true_lit() {
        parses_to! {
            parser: HclParser,
            input: "true",
            rule: Rule::boolean,
            tokens: [
                boolean(0, 4, [])
            ]
        }
    }

    #[test]
    fn parse_false_lit() {
        parses_to! {
            parser: HclParser,
            input: "false",
            rule: Rule::boolean,
            tokens: [
                boolean(0, 5, [])
            ]
        }
    }

    #[test]
    fn parse_number() {
        parses_to! {
            parser: HclParser,
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
            parser: HclParser,
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
            parser: HclParser,
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
            parser: HclParser,
            input: "1e10",
            rule: Rule::number,
            tokens: [
                number(0, 4, [])
            ]
        }
    }

    #[test]
    fn parse_pound_comment() {
        parses_to! {
            parser: HclParser,
            input: "# foo",
            rule: Rule::COMMENT,
            tokens: [
                COMMENT(0, 5)
            ]
        }
    }

    #[test]
    fn parse_double_slash_comment() {
        parses_to! {
            parser: HclParser,
            input: "// foo",
            rule: Rule::COMMENT,
            tokens: [
                COMMENT(0, 6)
            ]
        }
    }

    #[test]
    fn parse_block_comment() {
        parses_to! {
            parser: HclParser,
            input: "/* foo\nbar */",
            rule: Rule::COMMENT,
            tokens: [
                COMMENT(0, 13)
            ]
        }
    }

    #[test]
    fn parse_string() {
        parses_to! {
            parser: HclParser,
            input: "\"foo\"",
            rule: Rule::string,
            tokens: [
                string(0, 5)
            ]
        }
    }

    #[test]
    fn parse_list_empty() {
        parses_to! {
            parser: HclParser,
            input: "[]",
            rule: Rule::list,
            tokens: [
                list(0, 2)
            ]
        }
    }

    #[test]
    fn parse_list_single_boolean() {
        parses_to! {
            parser: HclParser,
            input: "[true]",
            rule: Rule::list,
            tokens: [
                list(0, 6, [
                    boolean(1, 5, [])
                ])
            ]
        }
    }

    #[test]
    fn parse_list_single_number() {
        parses_to! {
            parser: HclParser,
            input: "[1234]",
            rule: Rule::list,
            tokens: [
                list(0, 6, [
                    number(1, 5)
                ])
            ]
        }
    }

    #[test]
    fn parse_list_single_string() {
        parses_to! {
            parser: HclParser,
            input: "[\"foo\"]",
            rule: Rule::list,
            tokens: [
                list(0, 7, [
                    string(1, 6)
                ])
            ]
        }
    }

    #[test]
    fn parse_list_trailing_comma() {
        parses_to! {
            parser: HclParser,
            input: "[true,]",
            rule: Rule::list,
            tokens: [
                list(0, 7, [
                    boolean(1, 5, [])
                ])
            ]
        }
    }

    #[test]
    fn parse_list_multiple() {
        parses_to! {
            parser: HclParser,
            input: "[true,true]",
            rule: Rule::list,
            tokens: [
                list(0, 11, [
                    boolean(1, 5, []),
                    boolean(6, 10, [  ])
                ])
            ]
        }
    }

    #[test]
    fn parse_list_multiple_space_after_comma() {
        parses_to! {
            parser: HclParser,
            input: "[true, true]",
            rule: Rule::list,
            tokens: [
                list(0, 12, [
                    boolean(1, 5, [

                    ]),
                    boolean(7, 11, [

                    ])
                ])
            ]
        }
    }

    #[test]
    fn parse_list_multiple_trailing_comma() {
        parses_to! {
            parser: HclParser,
            input: "[true, true, ]",
            rule: Rule::list,
            tokens: [
                list(0, 14, [
                    boolean(1, 5, [

                    ]),
                    boolean(7, 11, [

                    ])
                ])
            ]
        }
    }

    #[test]
    fn parse_list_heterogenous() {
        parses_to! {
            parser: HclParser,
            input: "[true, 1]",
            rule: Rule::list,
            tokens: [
                list(0, 9, [
                    boolean(1, 5, [

                    ]),
                    number(7, 8)
                ])
            ]
        }
    }

    #[test]
    fn parse_assignment() {
        parses_to! {
            parser: HclParser,
            input: "foo = true",
            rule: Rule::assignment,
            tokens: [
                assignment(0, 10, [
                    ident(0, 3),
                    boolean(6, 10, [])
                ])
            ]
        }
    }

    #[test]
    fn parse_nested_object() {
        parses_to! {
            parser: HclParser,
            input: "foo { bar = \"baz\" }",
            rule: Rule::nested_object,
            tokens: [
                nested_object(0, 19, [
                    ident(0, 3),
                    object(4, 19, [
                        assignment(6, 18, [
                            ident(6, 9),
                            string(12, 17)
                        ])
                    ])
                ])
            ]
        }
    }

    #[test]
    fn parse_multi_nested_object() {
        for pair in
            super::HclParser::parse(Rule::nested_object, "foo \"bar\" { bar = \"baz\" }").unwrap()
        {
            println!("xxx{:?}xxx", pair);
        }

        parses_to! {
            parser: HclParser,
            input: "foo \"bar\" { baz = \"qux\" }",
            rule: Rule::nested_object,
            tokens: [
                nested_object(0, 25, [
                    ident(0, 3),
                    string(4, 9),
                    object(10, 25, [
                        assignment(12, 24, [
                            ident(12, 15),
                            string(18, 23)
                        ])
                    ])
                ])
            ]
        }
    }
}
