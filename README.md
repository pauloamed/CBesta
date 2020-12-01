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


1.12:
  - funcao e procs


-variaveis do lado esqerdo

  - ponteiro

-- como re executar a lista de tokens? -- OK
-- como controlar os returns?
-- como usar o cabecalho (parametros) na exeuccao da funcao? -- OK
- while e for
-- funcao recursiva para simular iteracao
- getStringFromId
-- hj em dia trata token, mas vai ter que tratar Parser (indexacao)
-- botar um get scope no parser do id tbm?
- sair do escopo, limpar as variaveis

- erros (ussar fail)
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


- restringir o free pra desalocar somente as variaveis alcoaadas na heap?
-- perguntar pra umberto

-- deref pointer: estrela de estrela
