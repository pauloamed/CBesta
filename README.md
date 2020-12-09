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
./parser.exe <arquivo_fonte>
```



