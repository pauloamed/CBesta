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


FALAR COM UMBERTO:
  COMO REPRESENTAR ERROS: FAIL? UNDEF?
  PRINTAR NEWLINE


  update da varaivel na heap n tem updt user type



TODO 
-- finalziar documentacao

-- revisao pra ver o q precisa mudar (compatibilidade)





5 e 6.12:
  - revisao e controle de erros
  -- assignment
  -- como indicar erros?
  -- dupla declaracao
  --- funcs, procs
  --- structs
  -- substr
  --- dar erro ou retornar vazia qnd intervs invalidos sao passados?
  -- compatibilidade de tipos
  --- tamanho de array tem q ser int

  - revisar o codigo enquanto comenta (1h30 ~2h)
  - compatibilidade de tipos
  - fazer os meninos testarem pra dar erro! >:(