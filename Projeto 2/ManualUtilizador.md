# **Manual de Utilizador - Jogo do Cavalo (Projeto 2)**

## **Inteligência Artificial**

![Imagem do Cavalo](https://e7.pngegg.com/pngimages/281/850/png-clipart-chess-piece-knight-pawn-jeu-des-petits-chevaux-chess-horse-game.png)

- **Docente:** Filipe Mariano
- **Realizado por:** João Jardin Nº 20180002 SW-04


# **Índice**

- [Introdução](#1-introdução)
- [Objetivo](#2-objetivo)
- [Funcionamento do programa](#3-funcionamento-do-programa)
- [Limitações do programa](#4-limitações-do-programa)



# **1. Introdução**

O Jogo do Cavalo é uma variante do problema matemático conhecido como o Passeio do Cavalo, cujo
objetivo é, através dos movimentos do cavalo, visitar todas as casas de um tabuleiro similar ao de xadrez. Esta
versão decorrerá num tabuleiro de 10 linhas e 10 colunas (10x10), em que cada casa possui uma pontuação.

Cada casa tem um valor associado, representado por um número e cada vez que o cavalo passa por uma casa acumula os pontos dessa mesmo casa.

O jogo do cavalo joga-se com 2 jogadores em que cada jogador possui uma peça do xadrez
tradicional, que é o cavalo. O Jogador 1 joga com o cavalo branco e o Jogador 2 joga com o cavalo
preto.

O jogo começa com a colocação do cavalo branco na casa de maior pontuação da 1ª linha (A1-J1
do tabuleiro).

Após a primeira jogada (colocar o cavalo branco) segue-se a jogada do adversário, com colocação do
cavalo preto na casa de maior pontuação da 10ª linha (A10-J10) do tabuleiro.

# **2. Objetivo**

O objetivo deste projeto é o de criar dois modos de jogo: 
1. Humano vs Computador
2. Computador vs Computador

No qual as jogadas do computador são efeutadas através do algoritmo alfabeta com cortes beta e alfa.

# **3. Funcionamento do programa**

## **3.1. Iniciar o programa**

Para abrir o programa o utilizador tem que abrir o LispWorks e carregar o ficheiro `projeto.lisp.

```lisp
; Loading text file C:\Users\PC\Desktop\Universidade\IA2023\IA\Projeto\P2\interact.lisp
;  Loading text file C:\Users\PC\Desktop\Universidade\IA2023\IA\Projeto\P2\jogo.lisp
;  Loading text file C:\Users\PC\Desktop\Universidade\IA2023\IA\Projeto\P2\algoritmo.lisp

---- Press Space to continue ----
```

De seguida o utilizador deverá de iniciar o programa, da seguinte maneira:

```lisp
CL-USER 13 : 4 > (iniciar-jogo)
Escolha o modo de jogo:
1 - Humano vs Computador
2 - Computador vs Computador
3 - Sair 
Escolha uma opção: 
```

## **3.2. Humano vs Computador**

Ao selecionar a opção Humano vs Computador:
- O utilizador de seguida terá de selecionar o tempo-limite para a jogada do computador
- A profundidade máxima a ser considerada pelo alfabeta 
- Decidir quem joga primeiro.
```lisp
CL-USER 13 : 4 > (iniciar-jogo)
Escolha o modo de jogo:
1 - Humano vs Computador
2 - Computador vs Computador
3 - Sair 
Escolha uma opção: 1
Defina o tempo limite para jogadas do computador (1000 a 5000 milissegundos): 5000
Escolha a profundidade para o algoritmo alfabeta (1 a 5): 3
Quem deve começar a partida?
1 - Humano
2 - Computador
Escolha uma opção: 1
```
Neste exemplo o primeiro jogador a fazer a sua jogada é o humano e como tal para jogar tem que escolher uma linha e coluna. De forma a ser mais fácil averiguar que jogadas pode fazer são apresentados os movimentos possíveis através de uma lista de conjuntos (linha coluna):
```lisp
Tabuleiro atual:

((73 42 56 22 43 9 58 -1 32 37)
 (88 NIL 38 99 2 61 3 45 94 92)
 (68 11 80 23 30 64 83 82 91 65)
 (15 5 20 62 39 25 71 12 85 87)
 (47 89 14 67 96 13 70 54 34 52)
 (76 66 46 53 74 55 0 29 41 27)
 (33 24 59 36 90 8 35 98 57 63)
 (31 51 86 44 50 49 81 4 95 72)
 (93 6 75 60 21 NIL 78 40 18 28)
 (16 17 10 19 77 1 7 -2 69 26))

Movimentos possíveis do Jogador -1: ((2 8) (1 9) (1 5) (2 6))

Jogador -1, por favor insira a nova linha para o cavalo: 2

Agora insira a nova coluna para o cavalo: 8

Pontos Ganhos: 91

((73 42 56 22 43 9 58 NIL 32 37)
 (88 NIL 38 99 2 61 3 45 94 92)
 (68 11 80 23 30 64 83 82 -1 65)
 (15 5 20 62 39 25 71 12 85 87)
 (47 89 14 67 96 13 70 54 34 52)
 (76 66 46 53 74 55 0 29 41 27)
 (33 24 59 36 90 8 35 98 57 63)
 (31 51 86 44 50 49 81 4 95 72)
 (93 6 75 60 21 NIL 78 40 18 28)
 (16 17 10 NIL 77 1 7 -2 69 26))

Pontos do Jogador 1: 175
Pontos do Jogador 2: 79
```
Depois de efetuar a jogada o computador faz a sua jogada e são apresentadas algumas estatísticas do alfabeta no ecrã, que depois por si podem ser consultadas no ficheiro `log.dat`
```lisp
Vez do jogador: -2

 Jogador: -2, Tempo Gasto: 270 ms, Nós analisados: 111, Profundidade: 3, Valor: -5.0, Cortes Alfa: 8, Cortes Beta: 1

Jogador -2 move cavalo para (7, 8).
Pontos Ganhos: 95

((73 42 56 22 43 9 58 NIL 32 37)
 (88 NIL 38 99 2 61 3 45 94 92)
 (68 11 80 23 30 64 83 82 -1 65)
 (15 5 20 62 39 25 71 12 85 87)
 (47 89 14 67 96 13 70 54 34 52)
 (76 66 46 53 74 55 0 29 41 27)
 (33 24 NIL 36 90 8 35 98 57 63)
 (31 51 86 44 50 49 81 4 -2 72)
 (93 6 75 60 21 NIL 78 40 18 28)
 (16 17 10 NIL 77 1 7 NIL 69 26))

Pontos do Jogador 1: 175
Pontos do Jogador 2: 174
```
O jogo então desenrola-se assim até que o jogo chegue ao fim sendo que isso pode acontecer porque:
- Um jogador já não tem movimentos possiveis e o outro jogador tem mais pontos
- Ambos os jogadores já não conseguem movimentar o respetivo cavalo.
```lisp
Pontos Ganhos: 79

((25 NIL 21 77 NIL 51 26 NIL 74 NIL)
 (13 22 89 61 23 NIL NIL NIL 4 NIL)
 (32 NIL 87 NIL 63 NIL NIL NIL -1 NIL)
 (66 27 52 46 73 70 NIL NIL NIL NIL)
 (40 85 NIL 7 11 12 NIL NIL 58 NIL)
 (3 15 28 78 72 62 NIL 36 2 NIL)
 (98 30 41 31 60 42 20 64 NIL 47)
 (44 NIL 48 NIL 0 43 -2 NIL 14 NIL)
 (NIL 55 NIL 24 83 NIL 38 54 34 NIL)
 (37 82 NIL 45 NIL 81 84 6 18 16))

Pontos do Jogador 1: 599
Pontos do Jogador 2: 737


Fim do jogo!
Jogador 2 venceu!
Pontos finais do Jogador 1: 599
Pontos finais do Jogador 2: 737
NIL
```


## **3.3. Computador vs Computador**

De forma semelhante o utilizador ao selecionar opção Computador vs Computador, precisa de definir qual o tempo limite máximo para jogada dos computadores bem como a profundidade máxima a ser considerada pelo algoritmo alfabeta.


```lisp
CL-USER 12 : 4 > (iniciar-jogo)
Escolha o modo de jogo:
1 - Humano vs Computador
2 - Computador vs Computador
3 - Sair 
Escolha uma opção: 2
Defina o tempo limite para jogadas do computador (1000 a 5000 milissegundos): 4000
Escolha a profundidade para o algoritmo alfabeta (1 a 5): 2
```
O jogo é então iniciado pelo jogador 1 e desenrola-se de maneira semelhante ao modo humano vs computador.
```lisp
Vez do jogador: -1

 Jogador: -1, Tempo Gasto: 50 ms, Nós analisados: 18, Profundidade: 2, Valor: -82.5, Cortes Alfa: 0, Cortes Beta: 0

Jogador -1 move cavalo para (1, 2).
Pontos Ganhos: 14

((NIL 85 60 61 32 50 19 26 56 53)
 (88 83 -1 38 52 30 44 98 64 24)
 (79 5 72 12 75 90 91 89 71 63)
 (9 NIL 4 95 93 NIL 35 97 77 17)
 (31 84 22 80 18 49 36 25 29 86)
 (43 81 23 10 20 57 15 59 39 45)
 (2 70 1 0 76 7 82 46 66 78)
 (11 58 47 74 6 92 67 68 27 62)
 (55 3 37 65 42 73 28 13 34 16)
 (33 51 48 54 -2 94 87 8 40 21))

Pontos do Jogador 1: 110
Pontos do Jogador 2: 99


Vez do jogador: -2

 Jogador: -2, Tempo Gasto: 84 ms, Nós analisados: 24, Profundidade: 2, Valor: 14.0, Cortes Alfa: 0, Cortes Beta: 2

Jogador -2 move cavalo para (7, 5).
Pontos Ganhos: 92

((NIL 85 60 61 32 50 19 26 56 53)
 (88 83 -1 38 52 30 44 98 64 24)
 (79 5 72 12 75 90 91 89 71 63)
 (9 NIL 4 95 93 NIL 35 97 77 17)
 (31 84 22 80 18 49 36 25 NIL 86)
 (43 81 23 10 20 57 15 59 39 45)
 (2 70 1 0 76 7 82 46 66 78)
 (11 58 47 74 6 -2 67 68 27 62)
 (55 3 37 65 42 73 28 13 34 16)
 (33 51 48 54 NIL 94 87 8 40 21))

Pontos do Jogador 1: 110
Pontos do Jogador 2: 191
```

Caso um dos jogadores não tenha movimentos possíveis e o outro jogador tenha menos pontos e tenha movimentos possíveis, o jogador passa a vez:
```lisp
Jogador -1 não tem movimentos válidos. Passando a vez...

Vez do jogador: -2

 Jogador: -2, Tempo Gasto: 8 ms, Nós analisados: 1, Profundidade: 2, Valor: -1152921504606846976, Cortes Alfa: 1, Cortes Beta: 0

Jogador -2 move cavalo para (9, 9).
Pontos Ganhos: 37

((18 -1 NIL 16 NIL 81 92 31 32 62)
 (19 NIL 29 NIL 11 26 22 9 NIL 98)
 (NIL NIL NIL NIL 24 NIL 48 NIL 21 67)
 (90 2 66 42 38 47 55 5 43 70)
 (NIL 41 14 74 33 84 56 50 72 91)
 (83 61 23 NIL NIL 60 25 7 3 6)
 (1 NIL NIL 44 80 87 NIL 54 78 40)
 (NIL 12 53 30 NIL 76 49 NIL 65 8)
 (NIL 89 17 NIL NIL 34 94 NIL 52 27)
 (71 35 NIL 20 13 45 0 10 4 -2))

Pontos do Jogador 1: 509
Pontos do Jogador 2: 537


Fim do jogo!
Jogador 2 venceu!
Pontos finais do Jogador 1: 509
Pontos finais do Jogador 2: 537
```

# **4. Limitações do programa**
- Atendendo que não se conseguiu de uma maneira arbitrária modificar o caminho dos ficheiros:
  - Para inicializar o ficheiro `interact.lisp`: O utilizador terá de alterar o caminho conforme necessário, no início do ficheiro.
    ```lisp
    (load "C:/Users/PC/Desktop/Universidade/IA2023/IA/Projeto/P2/jogo.lisp")
    (load "C:/Users/PC/Desktop/Universidade/IA2023/IA/Projeto/P2/algoritmo.lisp")
    ```
  - No ficheiro `algoritmo.lisp` na função `escrever-ficheiro`:
    ```lisp
    (defun escrever-ficheiro (mensagem)
      (let ((caminho "C:/Users/PC/Desktop/Universidade/IA2023/IA/Projeto/P2/log.dat"))
    ```
- No menu da escolha do problema, se o utilizador escolher uma opção inválida, o programa termina. Como tal, o utilizador tem que reiniciar o programa.
