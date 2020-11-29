Variavel: Nome, Tipo, Valor, Escopo, Endereço e Tempo de vida
  - Nome: será armazenado numa string
  - Tipo: será armazenado no tipo Type
  - Valor: será armazado no tipo Type
  - Escopo: será armazenado numa string
  - Endereço: teoricamente, o endereço é um identif. único da variável.
              Pra isso, usaremos a tupla: (Nome, escopo)
  - Tempo de vida: não vai ter um valor explícito. É o tempo que a variável permanece
                    na lista de memória


  A memória (sem recursão) vai guardar então ('\*' representa a vírgula para tuplas):
    - (String * String * Type)
      - Nome, Escopo, (Tipo + Valor)

  Quando a recursão é permitida, deve ser implementada a representação da pilha
  de execução. Isso será feito criando uma pilha de valores na representação interna de
  variável. Assim, a representação fica:
    - (String * String * [Type])



  Quais tabelas serão implementadas?
    - types_table: vai guardar todos os tipos que ja foram declarados e que podem
    ser utilizados pelo usuario. Sera inicializada com os tipos padrao.
    Sera uma lista de formato:
      -- [Type]
    - proc_table: vai guardar todos os procedimentos que ja foram declarados
      -- [(String, [Type])]
    - func_table: vai guardar todas as funcoes que ja foram declaradas
      -- [(String, Type, [Type])]
    - memory: vai guardar todas as variaveis ativas. tem suporte para recursao e para
    funcoes. tambem ja engloba a heap.
      -- [(String, String, [Type])]
