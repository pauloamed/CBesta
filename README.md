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
  STRUCT RECURSIVA
  PROBLEMA 4 TIPO RATIONAL_T (MAISCULO)
  B != 0? (P4)
  COMO REPRESENTAR ERROS: FAIL? UNDEF?




TODO 
-- resolver problema da struct
--- discutir valor padrao em strcut. x := expr, mas com exec off expr nao eh avaliada corretamente
-- finalziar documentacao
-- return solto no codigo
--- se tem reutrn, tem que ter contador de subpr >= 1
-- cast int, double e bool pra string
-- revisao pra ver o q precisa mudar (compatibilidade)
-- ver se acessos ao estado estao com execOn como premissa
--- ctrl f nos get e ver se tao dentro de isExecOn
--- pattern matching no memtable pra crud ser sempre com execon





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