# type-theory
## Description
There are my [solutions](#solutions) of problems from course of type theory in ITMO, 2019 <br/>

## Table Of Contents
<!--ts-->
   * [Description](#description)
   * [Table Of Contents](#table-of-contents)
   * [Dependencies](#dependencies)
      * [Build dependencies](#build-dependencies)
   * [Installation](#installation)
      * [Build And Run From Source Code](#build-and-run-from-source-code)
      * [Optional](#optional)
   * [Uninstallation](#uninstallation)
      * [Delete math-logic](#delete-math-logic)
   * [Problems](#problems)
      * [Solutions](#solutions)
<!--te-->

## Dependencies
### Build Dependencies
- [GHC (compiler and interactive environment for Haskell)](https://www.haskell.org/ghc/)
- [Alex (tool for generating lexical analysers in Haskell)](https://www.haskell.org/alex/)
- [Happy (parser generator system for Haskell)](https://www.haskell.org/happy/)

**Note:** This tools contains in [Haskell Platform](https://www.haskell.org/platform/).   

## Installation
### Build And Run From Source Code
1. Make sure you have installed all [Dependencies](#dependencies).

2. **Clone** this repository.

    `$ git clone https://github.com/akifev/type-theory.git`
	
3. **Select** problem.

	`$ cd type-theory/A/ # for example problem A`

4. **Run** application.

	`$ make run`

### Optional
- Delete generated files.

	`$ make clean`

- Pack project.

	`$ make pack`

## Uninstallation
### Delete math-logic
1. **Run**.
	
	`$ rm -Rf <path>` 

**Note:** <*path*> - path to *type-theory/* directory    

## Problems
### Solutions 
1. [A. Расстановка скобок в лямбда-выражении](A)

**Note:** Statements are available on Russian language
