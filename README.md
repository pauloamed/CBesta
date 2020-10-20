# CBesta

Para rodar o analisador léxico:

```sh
alex lexer.x
```

Para rodar o analisador sintático:

```sh
ghc sint_pars.hs lexer.hs -i terminals/*.hs -i non_terminals/*.hs
```
