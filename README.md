# Dyri Language

# Design

## Variables

let \<name\>: \<type\> = \<value\>

```Dyri
let a: int = 10;
let a: string = "Hello, World!";
```

### Type inference

let \<name\> = \<value\>

value must be a valid type

```Dyri
let a = 10;
```

## Named Functions

```Dyri
let print (x: int): int = println x;

let sum (x: int): int -> int = x + 1;

let sum x = x + 1;

sum a;
sum 10;
```

## Anonymous Functions

```Dyri
// Anonymous
x: int -> x + 1;
let sum: int -> int = x: int -> x + 1;
```

## Blocks

```Dyri
// New block of code
// Allocate here
let sum x = x + 1;

let b = {
  let a = 10;

  // Return a value
  add a
};
// Deallocate here
```

## Conditions

Booleans will be used

```Dyri
a = b ? ... : ...
if a = b { ... } else { ... }
```

## Loops

```Dyri
for i = 0 to 10 {
  ...
}

for true {
  if 1 = 1 {
    break;
  }
}
```

# TODO

- [x] Parse code to tokens from multiple sources (files, strings)
- [ ] Test the lex library
- [ ] Continue the type check
- [ ] LLVM Compiler
- [ ] Cli
