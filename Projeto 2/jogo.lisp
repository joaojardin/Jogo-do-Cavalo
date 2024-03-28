;;;; jogo.lisp
;;;; Disciplina de IA - 2023 / 2024
;;;; Ficheiro responsável pela lógica do jogo
;;;; Autor: João Jardin Nº20180002 SW-04

;;; Tabuleiros


(defvar *jogador1* -1)
(defvar *jogador2* -2)


(defun tabuleiro-p1 ()
  '(
    (94 25 54 89 21 8 36 14 41 96) 
    (78 47 56 23 5 49 13 12 26 60) 
    (0 27 17 83 34 93 74 52 45 80) 
    (69 9 77 95 55 39 91 73 57 30) 
    (24 15 22 86 1 11 68 79 76 72) 
    (81 48 32 2 64 16 50 37 29 71) 
    (99 51 6 18 53 28 7 63 10 88) 
    (59 42 46 85 90 75 87 43 20 31) 
    (3 61 58 44 65 82 19 4 35 62) 
    (33 70 84 40 66 38 92 67 98 97)
    )
)



(defun linha (indice tabuleiro)
  (nth indice tabuleiro))

(defun celula (indice-linha indice-coluna tabuleiro)
  (let ((linha (linha indice-linha tabuleiro)))
    (nth indice-coluna linha)))

(defun remover-se(pred lista)
  (cond ((null lista) NIL) 
        ((funcall pred (car lista)) (remover-se pred (cdr lista)))
        (T (cons (car lista) (remover-se pred (cdr lista))))))


(defun baralhar (lista)
  (cond ((null lista) nil)
  (t (let ((elemento-aleatorio (nth (random (length lista)) lista)))
  (cons elemento-aleatorio (baralhar (remover-se (lambda (x) (equal x elemento-aleatorio)) lista)))))))


(defun tabuleiro-aleatorio (&optional (lista (baralhar (lista-numeros 100))) (n 10))
  (cond
    ((null lista) nil)
    (t (cons (subseq lista 0 n) (tabuleiro-aleatorio (subseq lista n) n)))))

(defun substituir-posicao (indice lista &optional (novo-valor nil))
  (cond
    ((and (>= indice 0) (< indice (length lista)))
     (let ((antes (subseq lista 0 indice))
           (depois (subseq lista (+ indice 1))))
       (append antes (list novo-valor) depois)))
    (t lista)))


(defun substituir (linha coluna tabuleiro &optional (novo-valor nil))
  (let ((linha-alterada (substituir-posicao coluna (nth linha tabuleiro) novo-valor)))
    (substituir-posicao linha tabuleiro linha-alterada)))

(defun posicao-cavalo (tabuleiro jogador &optional (i 0) (j 0))
  "Encontra a posição do cavalo no tabuleiro para o jogador dado."
  (cond
    ((>= i (length tabuleiro)) nil) 
    ((and (< j (length (nth i tabuleiro)))
          (eql (nth j (nth i tabuleiro)) jogador)) 
     (list i j)) 
    ((< j (1- (length (nth i tabuleiro))))
     (posicao-cavalo tabuleiro jogador i (1+ j))) 
    (t (posicao-cavalo tabuleiro jogador (1+ i))))) 


(defun remover-simetrico (simetrico tabuleiro)
  (mapcar (lambda (linha) (mapcar (lambda (celula) (if (eql celula simetrico) nil celula)) linha)) tabuleiro))


(defun remover-duplo (maior-duplo tabuleiro)
  (mapcar (lambda (linha) (mapcar (lambda (celula) (if (eql celula maior-duplo) nil celula)) linha)) tabuleiro))

(defun posicionar-cavalo (tabuleiro jogador)
  (let* ((linha (if (= jogador *jogador1*) 0 9))
         (valores-linha (nth linha tabuleiro))
         (valores-linha-filtrados (remove-if #'null valores-linha))
         (max-valor (apply #'max valores-linha-filtrados)) 
         (max-index (position max-valor valores-linha)))
    (cond
      ((numero-duplo-p max-valor)
       (let* ((duplos (lista-duplos (concatena tabuleiro)))
              (maior-duplo (if (null duplos) max-valor (apply #'max duplos))))
         (remover-duplo maior-duplo (substituir linha max-index tabuleiro jogador))))

      ((numero-simetrico-p max-valor)
       (let ((simetrico (numero-simetrico-p max-valor)))
         (remover-simetrico simetrico (substituir linha max-index tabuleiro jogador))))
      (t
       (substituir linha max-index tabuleiro jogador)))))







(defun operadores ()
  (list 'operador-1 'operador-2 'operador-3 'operador-4 'operador-5 'operador-6 'operador-7 'operador-8))

(defun movimentar-cavalo (tabuleiro deslocamento-linha deslocamento-coluna jogador)
  "Move o cavalo no tabuleiro com base no deslocamento fornecido, se válido."
  (let ((cavalo-posicao (posicao-cavalo tabuleiro jogador)))
    (if cavalo-posicao
        (let ((nova-linha (+ (first cavalo-posicao) deslocamento-linha))
              (nova-coluna (+ (second cavalo-posicao) deslocamento-coluna)))
          (if (movimento-valido nova-linha nova-coluna tabuleiro jogador)
              (let ((casa-destino (celula nova-linha nova-coluna tabuleiro)))
                (cond
                  ((and casa-destino (numero-duplo-p casa-destino))
                   (let* ((duplos (lista-duplos (concatena tabuleiro)))
                          (maior-duplo (when duplos (apply #'max duplos))))
                     (values (remover-duplo maior-duplo (substituir nova-linha nova-coluna (substituir (first cavalo-posicao) (second cavalo-posicao) tabuleiro nil) jogador))
                             (if (numberp casa-destino) casa-destino 0)
                             nova-linha nova-coluna)))
                  ((and casa-destino (numero-simetrico-p casa-destino))
                   (let ((simetrico (numero-simetrico-p casa-destino)))
                     (values (remover-simetrico simetrico (substituir nova-linha nova-coluna (substituir (first cavalo-posicao) (second cavalo-posicao) tabuleiro nil) jogador))
                             (if (numberp casa-destino) casa-destino 0)
                             nova-linha nova-coluna)))
                  (t (values (substituir nova-linha nova-coluna (substituir (first cavalo-posicao) (second cavalo-posicao) tabuleiro nil) jogador)
                             (if (numberp casa-destino) casa-destino 0)
                             nova-linha nova-coluna))))
              nil)))))




(defun operador-1 (tabuleiro jogador)
  "Move o cavalo 2 linhas para baixo e 1 coluna para a direita."
  (movimentar-cavalo tabuleiro 2 1 jogador))

(defun operador-2 (tabuleiro jogador)
  "Move o cavalo 1 linha para baixo e 2 colunas para a direita."
  (movimentar-cavalo tabuleiro 1 2 jogador))

(defun operador-3 (tabuleiro jogador)
  "Move o cavalo 1 linha para cima e 2 colunas para a direita."
  (movimentar-cavalo tabuleiro -1 2 jogador))

(defun operador-4 (tabuleiro jogador)
  "Move o cavalo 2 linhas para cima e 1 coluna para a direita."
  (movimentar-cavalo tabuleiro -2 1 jogador))

(defun operador-5 (tabuleiro jogador)
  "Move o cavalo 2 linhas para cima e 1 coluna para a esquerda."
  (movimentar-cavalo tabuleiro -2 -1 jogador))

(defun operador-6 (tabuleiro jogador)
  "Move o cavalo 1 linha para cima e 2 colunas para a esquerda."
  (movimentar-cavalo tabuleiro -1 -2 jogador))

(defun operador-7 (tabuleiro jogador)
  "Move o cavalo 1 linha para baixo e 2 colunas para a esquerda."
  (movimentar-cavalo tabuleiro 1 -2 jogador))

(defun operador-8 (tabuleiro jogador)
  "Move o cavalo 2 linhas para baixo e 1 coluna para a esquerda."
  (movimentar-cavalo tabuleiro 2 -1 jogador))


(defun lista-numeros (n &optional (num 99))
  (cond ((< num 0) nil)
        (t (cons num (lista-numeros n (- num 1))))))


(defun movimento-valido (linha coluna tabuleiro jogador)
    (cond
      ((or (null linha) (null coluna) (null tabuleiro)) nil)
      ((or (> linha 9) (< linha 0) (> coluna 9) (< coluna 0)) nil)
      ((null (celula linha coluna tabuleiro)) nil)
      ((and (numberp (celula linha coluna tabuleiro)) 
            (>= (celula linha coluna tabuleiro) 0) 
            (<= (celula linha coluna tabuleiro) 99)) t)
      (t nil)))

              
(defun numero-simetrico-p (numero)
  (and (numberp numero)
       (+ (* (mod numero 10) 10) (floor numero 10))))


(defun numero-duplo-p (numero)
  (and (numberp numero)
       (not (= numero 0))
       (= (mod numero 10) (floor numero 10))))


(defun lista-duplos (tabuleiro)
  (cond
    ((null tabuleiro) nil)
    ((numero-duplo-p (car tabuleiro))
     (cons (car tabuleiro) (lista-duplos (cdr tabuleiro))))
    (t (lista-duplos (cdr tabuleiro)))))


(defun concatena (lista)
  (apply #'append lista))


(defun trocar-jogador (jogador)
  (cond ((= jogador *jogador1*) *jogador2*)
        ((= jogador *jogador2*) *jogador1*)))


(defun gerar-movimentos-possiveis (tabuleiro jogador operadores)
  (let ((resultados (mapcar (lambda (operador)
                              (let* ((novo-tabuleiro (funcall operador tabuleiro jogador))
                                     (nova-posicao (when novo-tabuleiro (posicao-cavalo novo-tabuleiro jogador))))
                                (when (and nova-posicao
                                           (not (null (celula (first nova-posicao) (second nova-posicao) tabuleiro)))
                                           (movimento-valido (first nova-posicao) (second nova-posicao) tabuleiro jogador))
                                  nova-posicao)))
                            operadores)))
    ;; Filtra os valores nil
    (remove-if #'null resultados)))


(defun criar-no (tabuleiro pai pontos-jogador1 pontos-jogador2 profundidade)
  "Cria um nó com o estado do tabuleiro, o nó pai, os pontos dos jogadores 1 e 2, e a profundidade."
  (list tabuleiro pai pontos-jogador1 pontos-jogador2 profundidade))


(defun estado-do-no (no)
  "Retorna o estado atual (tabuleiro) do nó"
  (first no))

(defun pai-do-no (no)
  "Retorna o nó-pai do nó"
  (second no))

(defun pontos-jogador1 (no)
  "Retorna os pontos do jogador 1 do nó"
  (third no))

(defun pontos-jogador2 (no)
  "Retorna os pontos do jogador 2 do nó"
  (fourth no))

(defun profundidade-do-no (no)
  "Retorna a profundidade do nó."
  (fifth no))


(defun novo-sucessor (no operador jogador)
  (multiple-value-bind (novo-tabuleiro valor jogada-linha jogada-coluna)
      (funcall operador (estado-do-no no) jogador)
    (when novo-tabuleiro ; Continua apenas se o novo-tabuleiro não for nil.
      (let ((nova-profundidade (1+ (profundidade-do-no no))))
        (values 
         (cond
           ((= jogador *jogador1*)
            (criar-no novo-tabuleiro (pai-do-no no) (+ (pontos-jogador1 no) valor) (pontos-jogador2 no) nova-profundidade))
           (t
            (criar-no novo-tabuleiro (pai-do-no no) (pontos-jogador1 no) (+ (pontos-jogador2 no) valor) nova-profundidade)))
         jogada-linha jogada-coluna)))))



(defun sucessores (no lista-op jogador)
  "Gera uma lista de sucessores para um dado nó."
  (remove-if #'null
             (mapcar (lambda (operador)
                       (multiple-value-bind (sucessor jogada-linha jogada-coluna)
                           (novo-sucessor no operador jogador)
                         (when sucessor
                           (list sucessor jogada-linha jogada-coluna jogador))))
                     lista-op)))





(defun no-teste ()
  (let* ((tabuleiro (tabuleiro-aleatorio))
         (linha-jogador1 (first tabuleiro))
         (linha-jogador2 (first (last tabuleiro)))
         (max-valor-jogador1 (and linha-jogador1 (apply #'max linha-jogador1)))
         (max-valor-jogador2 (and linha-jogador2 (apply #'max linha-jogador2)))
         (tabuleiro-com-jogador1 (when max-valor-jogador1 (posicionar-cavalo tabuleiro *jogador1*))))
    (when (and max-valor-jogador1 max-valor-jogador2 tabuleiro-com-jogador1)  
      (let ((max-index-jogador1 (position max-valor-jogador1 linha-jogador1))
            (max-index-jogador2 (position max-valor-jogador2 linha-jogador2))
            (tabuleiro-com-jogadores (posicionar-cavalo tabuleiro-com-jogador1 *jogador2*)))
        ;; Criar e retornar o nó
        (list tabuleiro-com-jogadores nil max-valor-jogador1 max-valor-jogador2 0)))))

