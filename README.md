# CBesta

Para rodar o analisador léxico:

```sh
alex lexer.x
```

Para compilar o analisador sintático:

```sh
ghc parser.hs lexer.hs -i terminals/*.hs -i non_terminals/*.hs -i state/*.hs -i execution/*.hs
```

Para rodar o analisador:
```sh
./parser.exe
```

### Adendo

Para alterar o exemplo a ser analisado, basta mudar o arquivo utlizado
na linha 13 de `parser.hs`


### TODO
- funcoes e procs
-- guardarão (nome, retorno, args, codigo) como (String, Type, [Type], [Token])


3.12:
  - so pode chamar funcoes dentro de expressoes
4.12:
- gravar valor em vetor e struct
- struct recursivo (testar)
-- id do lado esquerdo em geral
  - deref pointer
  -- recursivo, mais de uma estrela

5 e 6.12:
  - revisao e controle de erros
  -- assignment
  -- como indicar erros?
  -- dupla declaracao
  --- variaveis
  --- funcs, procs
  --- structs
  -- substr
  --- dar erro ou retornar vazia qnd intervs invalidos sao passados?
  -- compatibilidade de tipos
  --- tamanho de array tem q ser int
