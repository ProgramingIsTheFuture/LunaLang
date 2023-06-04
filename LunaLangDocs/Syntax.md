
```ebnf
<name> ::= words

<typnames> ::= Words

<typ> ::=
	| <int>
	| <string>
	| <product> (AxB)
	| <bool>

<sumtypes> ::=
	| type <name> = <typnames> [of <typ>] | <typnames>

<bool> ::= true | false

<const> ::= 
	| n as number
	| s as string
	| <bool>

<expression> ::=
	| <const>
	| let <name> = <expression> in <expression>
	| fun <name> -> <expression>
	| if <bool> then <expression> else <expression>

```
