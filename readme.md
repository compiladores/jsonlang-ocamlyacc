# Parser LucasLang -> JsonLang using Ocaml

#### Lucas Rial Brandariz - Lenguajes y Compiladores - FIUBA - 2022

<p>&nbsp;</p>

## Instalation (Mac)

```
$ brew install opam
$ opam init
$ eval $(opam env)
-- $ opam switch create 4.14.0
-- $ eval $(opam env)
$ opam install dune merlin ocaml-lsp-server odoc ocamlformat utop dune-release
Install 'OCaml Platform' extension for VSCode
```

<p>&nbsp;</p>

## Run

To compile the Lexer and Parser

```
$ make
```

To run all tests

```
$ make test
```

To open Ocaml interactive terminal

```
$ dune utop src
```

To parse LucasLang string into JsonLang string

```
Interp.Main.parse_jsonLang "...LucasLang...";;
```

<p>&nbsp;</p>

## LucasLang

<pre>
IF                |-> if ... then ...;
IF                |-> if ... then ... else ...;
WHILE             |-> while ... do ...;
FOR               |-> for ... form ... to ... step ... do ...;
FOR               |-> for ... form ... to ... do ...;
DO                |-> do ... until ...;
BREAK             |-> break;
CONTINUE          |-> continue;
DECLARE           |-> let ... = ...;
SET               |-> ... = ...;
CALL FUNCTION     |-> ... (...);
RETURN            |-> return ...;
DECLARE FUNCTION  |-> func ... ([... , ...]){};
</pre>

<p>&nbsp;</p>

## JsonLang

```
type Statement<Expression> = 
| {
  if: { cond: Expression; then: Statement<Expression> }[];
  else?: Statement<Expression>;
}
| { while: Expression; do: Statement<Expression> }
| Statement<Expression>[]
| { iterator: string; from: Expression; to: Expression; step?: Expression; do: Statement<Expression> }
| { do: Statement<Expression>; until: Expression }
| "break"
| "continue"
| { declare: string; value: Expression }
| { set: string; value: Expression }
| { call: string; args: Expression[] }
| { return: Expression }

type Binops =
  | "+"
  | "-"
  | "*"
  | "/"
  | "^"
  | "%"
  | "&"
  | "|"
  | ">>"
  | "<<"
  | "<"
  | "<="
  | ">"
  | ">="
  | "=="
  | "~="
  | "and"
  | "or";

type Expression =
  | { unop: "-" | "!" | "~"; arg: Expression }
  | { binop: Binops; argl: Expression; argr: Expression }
  | string
  | { call: string; args: Expression[] }
  | number;

type DeclarationStatement<Statement>={
  function: string;
  args: string[];
  block: Statement;
};

type TopStatement<Expression> = Statement<Expression> | DeclarationStatement<Statement<Expression>>

type JsonLang = TopStatement<Expression>[];
```

<p>&nbsp;</p>

## Questions

1. ¿Cuán facil fue aprender esta herramienta de parsing? ¿Por qué?

La parte específica del código para compilar el Lexer y el Parser fue bastante
fácil ya que hay varios ejemplos en internet. Lo que fue mucho más complejo fue
la parte en Ocaml encargada de traducir la salida del parser a JsonLang. Es un
lenguaje muy distinto a todo lo que había usado ultimamente. Tanto la sintaxis
como la filosofía detrás del lenguaje supusieron la mayor barrera del trabajo.

2. ¿Recomendarías esta herramienta de parsing a futuros estudiantes de la
   materia? ¿Por qué?

No, a menos que esten familiarizados con Ocaml o lenguajes del estilo. A la gran
mayoría le será muchisimo más efectiva una herramienta que se integre con
Javascript

3. Liste ventajas y desventajas del uso de esta herramienta de parsing.

Ventajas:

- Instalación sencilla
- Pocos archivos
- Curso muy completo online
- Buena documentación
- Buena integración con VS Code
- Pocos Bugs en ejecución

Desventajas:

- Paradigma del lenguaje complejo de procesar en poco tiempo
- Mucho énfasis en los tipos de datos
- Poca discusión en foros
