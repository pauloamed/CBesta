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
