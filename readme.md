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

1. ??Cu??n facil fue aprender esta herramienta de parsing? ??Por qu???

La parte espec??fica del c??digo para compilar el Lexer y el Parser fue bastante
f??cil ya que hay varios ejemplos en internet. Lo que fue mucho m??s complejo fue
la parte en Ocaml encargada de traducir la salida del parser a JsonLang. Es un
lenguaje muy distinto a todo lo que hab??a usado ultimamente. Tanto la sintaxis
como la filosof??a detr??s del lenguaje supusieron la mayor barrera del trabajo.

2. ??Recomendar??as esta herramienta de parsing a futuros estudiantes de la
   materia? ??Por qu???

No, a menos que esten familiarizados con Ocaml o lenguajes del estilo. A la gran
mayor??a le ser?? muchisimo m??s efectiva una herramienta que se integre con
Javascript

3. Liste ventajas y desventajas del uso de esta herramienta de parsing.

Ventajas:

- Instalaci??n sencilla
- Pocos archivos
- Curso muy completo online
- Buena documentaci??n
- Buena integraci??n con VS Code
- Pocos Bugs en ejecuci??n

Desventajas:

- Paradigma del lenguaje complejo de procesar en poco tiempo
- Mucho ??nfasis en los tipos de datos
- Poca discusi??n en foros
