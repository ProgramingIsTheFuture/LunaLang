# Dyri Language

<img align="left" src="http://estruyf-github.azurewebsites.net/api/VisitorHit?user=ProgramingIsTheFuture&repo=Dyri&countColorcountColor&countColor=%237B1E7B"/>
<img align="right" src="https://img.shields.io/github/repo-size/ProgramingIsTheFuture/Dyri?style=for-the-badge&logo=appveyor" alt="GitHub repo size"/>

<img align="right" alt="Dyri" src="https://socialify.git.ci/ProgramingIsTheFuture/Dyri/image?description=1&descriptionEditable=Dyri%20is%20a%20general%20purpose%20language!&forks=1&issues=1&logo=https%3A%2F%2Fencrypted-tbn0.gstatic.com%2Fimages%3Fq%3Dtbn%3AANd9GcS3tdS_0pNpeosApm1sD7PrS0LAKq_KHmhYFjO8_24QL14A0r2Y9GyaNQEfl7qAHhk1EBA%26usqp%3DCAU&name=1&owner=1&pattern=Floating%20Cogs&pulls=1&stargazers=1&theme=Dark"/>

<p align="center">
<img src="https://forthebadge.com/images/badges/built-with-love.svg" alt=" forks"/>
</p>

## Documentations for the packages used to build this language can be found at [dyri](https://programingisthefuture.github.io/Dyri/)

# Design

[Design](https://programingisthefuture.github.io/Dyri/cli/design.html) can be found here!

# Features

- Let (Declare variables)
- Let for functions
- Let with blocks of code (Multi line variable/function)
- Anonymous functions
- Arithmetic operators
- Expression based language

# TO-DO

- [x] Deploy docs to github pages with github actions
- Update language design
  - [x] Better base type system
  - [x] Type definition
  - [x] More about the blocks
  - [ ] Import and Export (support multiple files in single project)
- Better AST
  - [x] Standard Ast
  - [x] Typed Ast
  - [ ] Operators (+, -, /, *, %)
- [ ] Types:
  - [x] Int | Bool | String
  - [ ] Lists/Array
- [x] Lex and Parse the AST
  - [ ] Operators
  - [ ] Mora data types
  - [ ] Types definition
  - [ ] Blocks of code
- [ ] Check the types that can be type-inferred and statically typed
  - [ ] Need to check types for the rest of the features
- [ ] Compile to LLVM
- [ ] Int types (Is it an int32 or int? How to choose?)

# Plan

- Lex and Parse the language
- Check the types
- Compile to LLVM
- Support cross-compilation
- Import and Export libraries
- Start building externals libraries to general use of the language

## Want to contribute?
- Take a look at [`contributing guidelines`](CONTRIBUTING.md).
- Refer [GitHub Flow](https://guides.github.com/introduction/flow). 

## Contributors

**Thanks goes to these wonderful people ❤️**

<br/>
<div align="center">
<a href="https://github.com/ProgramingIsTheFuture/Dyri/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=ProgramingIsTheFuture/Dyri&max=100&columns=11" width=50%/>
</a>
</div>

<br>
<br>
<hr>
<h6 align="center">© Dyri 2022 
<br>
All Rights Reserved</h6>
