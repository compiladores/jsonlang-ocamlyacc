# TP Final Compiladores
### Lucas Rial Brandariz
## JsonLang - Ocaml

### Instalation Mac
```
$ brew install opam
$ opam init
$ eval $(opam env)
-- $ opam switch create 4.14.0
-- $ eval $(opam env)
$ opam install dune merlin ocaml-lsp-server odoc ocamlformat utop dune-release
Install 'OCaml Platform' extension for VSCode
```

### JsonLang

```
export type Statement<Expression> = 
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

export type Expression =
  | { unop: "-" | "!" | "~"; arg: Expression }
  | { binop: Binops; argl: Expression; argr: Expression }
  | string
  | { call: string; args: Expression[] }
  | number;

export type DeclarationStatement<Statement>={
  function: string;
  args: string[];
  block: Statement;
};

export type TopStatement<Expression> = Statement<Expression> | DeclarationStatement<Statement<Expression>>

export type JsonLang = TopStatement<Expression>[];
```