# CBesta

Para rodar o analisador léxico:

```sh
alex lexer.x
```

Para compilar o analisador sintático:

```sh
ghc parser.hs lexer.hs -i terminals/*.hs -i non_terminals/*.hs -i symtables/*.hs -i execution/*.hs
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
-- como re executar a lista de tokens?
-- como controlar os returns?
- while e for
-- funcao recursiva para simular iteracao
- struct
-- como usar uma struct ja declarada?
- void commands
-- read
--- como?
-- free
- pointer
-- como funciona o ponteiro para array e seus elementos?
- array
-- acesso/controle dos elementos
- tupla
-- acesso/controle dos elementos (29.11)
- ret commands
-- cast (29.11)
-- alloc
-- addr (29.11)
-- len
-- substr
--- dar erro ou retornar vazia qnd intervs invalidos sao passados?
- organizar codigo (29.11)
- getStringFromId
-- hj em dia trata token, mas vai ter que tratar Parser (indexacao)
-- botar um get scope no parser do id tbm?
- sair do escopo, limpar as variaveis
