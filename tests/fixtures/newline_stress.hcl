function_in_expr_expr = (
  bar
  ()
)

/*
     foo = bar
    ()

Error: Argument or block definition required

  on tests/fixtures/newline_stress.hcl line 7:
   7: ()

An argument or block definition is required here.
*/

legacy_index_in_expr = ({}
.
1234
)

/*
Error: Invalid attribute name

  on tests/fixtures/newline_stress.hcl line 13:
  13: foo = {}.
  14: 1234

An attribute name is required after a dot.
*/

//object_1 = { 1
//=
//2}
/*
Error: Missing attribute value

  on tests/fixtures/newline_stress.hcl line 33:
  33: object_1 = { 1
  34: =

Expected an attribute value, introduced by an equals sign ("=").
*/

//object_1 = { 1 =
//2}
/*
Error: Invalid expression

  on tests/fixtures/newline_stress.hcl line 46:
  46: object_1 = { 1 =
  47: 2}

Expected the start of an expression, but found an invalid expression token.
*/

object_2 = {
  1 = 2
}

//object_3 = {
//  1 = 2
//  ,
//  3 = 4
//}
/*
Error: Invalid expression

  on tests/fixtures/newline_stress.hcl line 64:
  64:   ,

Expected the start of an expression, but found an invalid expression token.
*/

//object_3 = {
//  1 = 2
//  , 3 = 4
//}
/*
Error: Invalid expression

  on tests/fixtures/newline_stress.hcl line 78:
  78:   , 3 = 4

Expected the start of an expression, but found an invalid expression token.
*/

object_3 = {
  1=2
  2=3
}


//object_4 = {
//  1=2 3=4
//}
/*
Error: Missing attribute separator

  on tests/fixtures/newline_stress.hcl line 95:
  94: object_4 = {
  95:   1=2 3=4

Expected a newline or comma to mark the beginning of the next attribute.
*/

tuple_1 = [
]

tuple_2 = [
  1,
  2
]

tuple_3 = [
  1
  , 2
]
