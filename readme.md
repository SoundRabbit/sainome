<div align="center">

# Sainome

A generic dice bot for RPG

</div>

## Features

### Types of Value

| name | description                           | syntax                      | Example                    |
| :--- | :------------------------------------ | :-------------------------- | :------------------------- |
| Bool | An Boolean Value                      | ("True" \| "False")         | `True`                     |
| Fnc  | A Function which receives an argument | "\\\\" Ident "." Expr       | `\\x.x+1`                  |
| List | A Vec of Values                       | "[" ((Expr ",")* Expr)? "]" | `[612, "ABCdef", \\x.x+1]` |
| Num  | A Number of f64                       | ["0"-"9"]+(\.["0"-"9"]+)?   | `61.2`                     |
| Ref  | A reference to grobal values          | {([^"."]+ ",")* [^"."]+}    | `{abc.def.123}`            |
| Str  | A String of String                    | "\\"" [^"\\""]* "\\""       | `"ABCdef"`                 |

### Ident

An ident is a string which is not surrounded with " and has only alphabetic character. You can bind value to Ident.

This is an example:

```text
(a:=1; a+1)
```

The result of this will be 2.

### Operators

| operator | left | right | description                                            |
| :------: | :--- | :---- | :----------------------------------------------------- |
|    #>    | Any  | List  | generate a function which reduce a list from the left  |
|    <#    | List | Any   | generate a function which reduce a list from the right |
|    .     | Fnc  | Any   | call function with right as an argument                |
|    .     | List | Num   | access the item of list by index                       |
|    .     | List | Fnc   | map list with function                                 |
|    .     | Num  | Any   | evalute right expression left times to generate a List |
|    +     | Bool | Bool  | left \|\| right                                        |
|    +     | Num  | Num   | left + right                                           |
|    +     | Str  | Str   | string concatenation                                   |
