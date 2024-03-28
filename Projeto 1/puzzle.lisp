;;;; puzzle.lisp
;;;; Disciplina de IA - 2023 / 2024
;;;; Ficheiro responsável pela lógica do jogo
;;;; Autor: João Jardin Nº20180002 SW-04

;;; Tabuleiros


(defun tabuleiro-teste ()
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



(defun tabuleiro-jogado ()
  '(
    (T 25 54 89 21 8 36 14 41 96) 
    (78 47 56 23 5 NIL 13 12 26 60) 
    (0 NIL 17 83 34 93 74 52 45 80) 
    (69 9 77 95 55 39 91 73 57 30) 
    (24 15 22 86 1 11 68 79 76 72) 
    (81 48 32 2 64 16 50 37 29 71) 
    (99 51 6 18 53 28 7 63 10 88) 
    (59 42 46 85 90 75 87 43 20 31) 
    (3 61 58 44 65 82 19 4 35 62) 
    (33 70 84 40 66 38 92 67 98 97)
    )
)


(defun tabuleiro-meio ()
  '(
    (94 25 54 89 21 8 36 14 41 96) 
    (78 47 56 23 5 49 13 12 26 60) 
    (0 27 17 83 34 93 74 52 45 80) 
    (69 9 77 95 NIL 39 91 73 57 30) 
    (24 15 28 86 T 11 68 79 76 72) 
    (81 48 32 2 64 16 50 37 29 71) 
    (99 51 6 18 53 22 7 63 10 88) 
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


(defun posicao-cavalo (tabuleiro &optional (i 0) (j 0))
  (cond
    ((>= i (length tabuleiro)) nil)
    ((and (< j (length (nth i tabuleiro)))
          (eql (nth j (nth i tabuleiro)) 'T))
     (list i j))
    ((< j (1- (length (nth i tabuleiro))))
     (posicao-cavalo tabuleiro i (1+ j)))
    (t (posicao-cavalo tabuleiro (1+ i)))))

(defun remover-simetrico (simetrico tabuleiro)
  (mapcar (lambda (linha) (mapcar (lambda (celula) (if (eql celula simetrico) nil celula)) linha)) tabuleiro))


(defun remover-duplo (maior-duplo tabuleiro)
  (mapcar (lambda (linha) (mapcar (lambda (celula) (if (eql celula maior-duplo) nil celula)) linha)) tabuleiro))

(defun posicionar-cavalo (tabuleiro)
  (cond
    ((posicao-cavalo tabuleiro) tabuleiro) 
    (t (substituir 0 0 tabuleiro 'T)))) 


(defun operadores ()
  (list 'operador-1 'operador-2 'operador-3 'operador-4 'operador-5 'operador-6 'operador-7 'operador-8))


(defun movimentar-cavalo (tabuleiro deslocamento-linha deslocamento-coluna)
  (let* ((tabuleiro-com-cavalo (posicionar-cavalo tabuleiro))
         (cavalo-posicao (posicao-cavalo tabuleiro-com-cavalo))
         (nova-linha (+ (first cavalo-posicao) deslocamento-linha))
         (nova-coluna (+ (second cavalo-posicao) deslocamento-coluna)))
    (if (movimento-valido nova-linha nova-coluna tabuleiro-com-cavalo)
        (let ((casa-destino (celula nova-linha nova-coluna tabuleiro-com-cavalo)))
          (cond
            ((and casa-destino (numero-duplo-p casa-destino))
             (let* ((duplos (lista-duplos (concatena tabuleiro-com-cavalo)))
                    (maior-duplo (if duplos (apply #'max duplos))))
               (values (remover-duplo maior-duplo (substituir nova-linha nova-coluna (substituir (first cavalo-posicao) (second cavalo-posicao) tabuleiro-com-cavalo nil) 'T)) 
                       (if (numberp casa-destino) casa-destino 0))))
            ((and casa-destino (numero-simetrico-p casa-destino))
             (let ((simetrico (numero-simetrico-p casa-destino)))
               (values (remover-simetrico simetrico (substituir nova-linha nova-coluna (substituir (first cavalo-posicao) (second cavalo-posicao) tabuleiro-com-cavalo nil) 'T)) 
                       (if (numberp casa-destino) casa-destino 0))))
            (t (values (substituir nova-linha nova-coluna (substituir (first cavalo-posicao) (second cavalo-posicao) tabuleiro-com-cavalo nil) 'T) 
                       (if (numberp casa-destino) casa-destino 0)))))
        nil)))




(defun operador-1 (tabuleiro)
  "Move o cavalo 2 linhas para baixo e 1 coluna para a direita."
  (movimentar-cavalo tabuleiro 2 1))

(defun operador-2 (tabuleiro)
  "Move o cavalo 1 linha para baixo e 2 colunas para a direita."
  (movimentar-cavalo tabuleiro 1 2))

(defun operador-3 (tabuleiro)
  "Move o cavalo 1 linha para cima e 2 colunas para a direita"
  (movimentar-cavalo tabuleiro -1 2))

(defun operador-4 (tabuleiro)
  "Move o cavalo 2 linhas para cima e 1 coluna para a direita"
  (movimentar-cavalo tabuleiro -2 1))

(defun operador-5 (tabuleiro)
  "Move o cavalo 2 linhas para cima e 1 coluna para a esquerda"
  (movimentar-cavalo tabuleiro -2 -1))

(defun operador-6 (tabuleiro)
  "Move o cavalo 1 linha para cima e 2 colunas para a esquerda"
  (movimentar-cavalo tabuleiro -1 -2))

(defun operador-7 (tabuleiro)
  "Move o cavalo 1 linha para baixo e 2 colunas para a esquerda"
  (movimentar-cavalo tabuleiro 1 -2))

(defun operador-8 (tabuleiro)
  "Move o cavalo 2 linhas para baixo e 1 coluna para a esquerda"
  (movimentar-cavalo tabuleiro 2 -1))

(defun lista-numeros (n &optional (num 100))
  (cond ((<= num 0) nil)
  (t (cons num (lista-numeros n (- num 1))))))


(defun movimento-valido (linha coluna tabuleiro)
  (cond
    ((or (null linha) (null coluna) (null tabuleiro)) nil)
    ((or (> linha 9) (< linha 0) (> coluna 9) (< coluna 0)) nil)
    ((null (celula linha coluna tabuleiro)) nil)
    (t t)))

              
(defun numero-simetrico-p (numero)
  "Retorna o simétrico de um número."
  (and (numberp numero)
       (+ (* (mod numero 10) 10) (floor numero 10))))


(defun numero-duplo-p (numero)
  "Verifica se o número é um número duplo."
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

(defun criar-no (tabuleiro pai h profundidade pontos objetivo)
  "Cria um nó com o estado do tabuleiro, o nó pai, a heurística, a profundidade, os pontos e o objetivo."
  (list tabuleiro pai h profundidade pontos objetivo))


(defun estado-do-no (no)
  "Retorna o estado atual (tabuleiro) do nó"
  (first no))


(defun pai-do-no (no)
  "Retorna o nó-pai do nó"
  (second no))

(defun h-do-no (no)
  "Retorna o valor heurístico do nó"
  (third no))

(defun profundidade-do-no (no)
  "Retorna a profundidade do nó"
  (fourth no))

(defun pontos-do-no (no)
  "Retorna os pontos acumulados do nó"
  (fifth no))

(defun objetivo-do-no (no)
  "Retorna o objetivo de pontos (constante)."
  (sixth no))


(defun no-custo (no)
  "Calcula o custo de um nó, soma da profundidade e da heurística."
  (+ (profundidade-do-no no) (h-do-no no)))


(defun ordenar-nos (lista)
  "Ordena uma lista de nós em ordem crescente com base no custo de cada nó."
  (sort lista #'< :key #'no-custo))


(defun novo-sucessor (no operador)
  (multiple-value-bind (novo-tabuleiro pontos-ganhos) ; Para obter o tabuleiro após aplicação do operador bem como os pontos ganhos pela função movimentar-cavalo
      (funcall operador (estado-do-no no))
    (when novo-tabuleiro ; Continua apenas se o novo-tabuleiro não for nil.
      (let ((nova-profundidade (1+ (profundidade-do-no no)))
            (pontos-acumulados (+ (pontos-do-no no) pontos-ganhos))
            (objetivo (objetivo-do-no no)))
        (let ((nova-heuristica (heuristica novo-tabuleiro pontos-acumulados objetivo)))
          (criar-no novo-tabuleiro no nova-heuristica nova-profundidade pontos-acumulados objetivo))))))



(defun sucessores (no lista-op &optional algoritmo maxprofundidade)
  (cond 
    ((and (equal algoritmo 'dfs) maxprofundidade (= (profundidade-do-no no) maxprofundidade))
     nil) 
    (t (let ((sucessores (remove-if #'null (mapcar #'(lambda (operador) (novo-sucessor no operador)) lista-op))))
    sucessores))))


(defun somar-pontos-tabuleiro (tabuleiro)
  (reduce #'+ (mapcan (lambda (linha) (mapcar (lambda (celula) (if (numberp celula) celula 0)) linha)) tabuleiro) :initial-value 0))


(defun media-por-casa (tabuleiro)
  (let* ((total-pontos (somar-pontos-tabuleiro tabuleiro))
          (total-casas (contar-casas-nao-nulas tabuleiro)))
    (cond
      ((> total-casas 0)
       (/ (float total-pontos) total-casas))
      (t
       0.0))))



(defun heuristica (tabuleiro pontos-acumulados objetivo)  (let ((media (media-por-casa tabuleiro))
        (restantes (- objetivo pontos-acumulados)))
    (if (> media 0)
        (abs(/ restantes media)) 
        0))) 


(defun no-teste ()
  "Cria um nó teste em que o nó-inicial é o no-solucao"
  (let* ((tabuleiro-inicial (tabuleiro-teste))
         (profundidade-inicial 0)
         (pontos-iniciais (celula 0 0 tabuleiro-inicial))
         (objetivo 94)
         (heuristica-inicial (heuristica tabuleiro-inicial pontos-iniciais objetivo)))
    (criar-no tabuleiro-inicial nil heuristica-inicial profundidade-inicial pontos-iniciais objetivo)))


;;; Nós-problema
(defun no-teste-a ()
  (let* ((tabuleiro-inicial (tabuleiro-a))
         (profundidade-inicial 0)
         (pontos-iniciais (celula 0 0 tabuleiro-inicial))
         (objetivo 70)
         (heuristica-inicial (heuristica tabuleiro-inicial pontos-iniciais objetivo)))
    (criar-no tabuleiro-inicial nil heuristica-inicial profundidade-inicial pontos-iniciais objetivo)))


(defun no-teste-b ()
  (let* ((tabuleiro-inicial (tabuleiro-b))
         (profundidade-inicial 0)
         (pontos-iniciais (celula 0 0 tabuleiro-inicial))
         (objetivo 60)
         (heuristica-inicial (heuristica tabuleiro-inicial pontos-iniciais objetivo)))
    (criar-no tabuleiro-inicial nil heuristica-inicial profundidade-inicial pontos-iniciais objetivo)))

(defun no-teste-c ()
  (let* ((tabuleiro-inicial (tabuleiro-c))
         (profundidade-inicial 0)
         (pontos-iniciais (celula 0 0 tabuleiro-inicial))
         (objetivo 270)
         (heuristica-inicial (heuristica tabuleiro-inicial pontos-iniciais objetivo)))
    (criar-no tabuleiro-inicial nil heuristica-inicial profundidade-inicial pontos-iniciais objetivo)))

(defun no-teste-d ()
  (let* ((tabuleiro-inicial (tabuleiro-d))
         (profundidade-inicial 0)
         (pontos-iniciais (celula 0 0 tabuleiro-inicial))
         (objetivo 600)
         (heuristica-inicial (heuristica tabuleiro-inicial pontos-iniciais objetivo)))
    (criar-no tabuleiro-inicial nil heuristica-inicial profundidade-inicial pontos-iniciais objetivo)))

(defun no-teste-e ()
  (let* ((tabuleiro-inicial (tabuleiro-e))
         (profundidade-inicial 0)
         (pontos-iniciais 0)
         (objetivo 300)
         (heuristica-inicial (heuristica tabuleiro-inicial pontos-iniciais objetivo)))
    (criar-no tabuleiro-inicial nil heuristica-inicial profundidade-inicial pontos-iniciais objetivo)))

(defun no-teste-f ()
  (let* ((tabuleiro-inicial (tabuleiro-f))
         (profundidade-inicial 0)
         (pontos-iniciais (celula 0 0 tabuleiro-inicial))
         (objetivo 2000)
         (heuristica-inicial (heuristica tabuleiro-inicial pontos-iniciais objetivo)))
    (criar-no tabuleiro-inicial nil heuristica-inicial profundidade-inicial pontos-iniciais objetivo)))


;;Problemas

;Problema A

(defun tabuleiro-a ()
  (list
   (list 2 20 44 NIL NIL NIL NIL NIL NIL NIL)
   (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
   (list NIL 3 30 NIL NIL NIL NIL NIL NIL NIL)
   (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
   (list NIL NIL NIL 22 NIL NIL NIL NIL NIL NIL)
   (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
   (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
   (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
   (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
   (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)))

(defun tabuleiro-b ()
  (list
   (list 2 NIL 4 NIL 6 NIL 8 NIL 10 NIL)
   (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
   (list NIL 3 NIL 5 NIL 7 NIL 9 NIL 11)
   (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
   (list NIL NIL NIL 22 NIL NIL NIL NIL NIL NIL)
   (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
   (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
   (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
   (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
   (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)))


(defun tabuleiro-c ()
  (list
   (list 1 12 3 23 NIL 88 NIL NIL NIL NIL)
   (list 21 45 43 NIL NIL NIL NIL NIL NIL NIL)
   (list NIL 56 NIL 78 NIL 7 NIL 9 NIL 11)
   (list 89 NIL 99 54 NIL NIL NIL NIL NIL NIL)
   (list NIL NIL NIL 22 NIL NIL NIL NIL NIL NIL)
   (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
   (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
   (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
   (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
   (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)))


(defun tabuleiro-d ()
  (list
   (list 98 97 96 95 94 93 92 91 90 89)
   (list 1 2 3 4 5 55 6 7 8 9)
   (list NIL 66 NIL 78 NIL 7 NIL 9 NIL 11)
   (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
   (list NIL NIL 22 NIL NIL NIL NIL NIL 33 NIL)
   (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
   (list NIL NIL NIL 88 NIL NIL NIL 44 NIL NIL)
   (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
   (list NIL NIL NIL NIL 77 NIL NIL NIL NIL NIL)
   (list NIL NIL NIL NIL NIL NIL 99 NIL NIL NIL)))

(defun tabuleiro-e ()
  (list
   (list NIL 5 NIL NIL NIL 15 NIL NIL NIL 25)
   (list NIL NIL NIL 6 NIL NIL NIL 16 NIL NIL)
   (list NIL 4 NIL NIL NIL 14 NIL NIL NIL 24)
   (list NIL NIL NIL 7 NIL NIL NIL 17 NIL NIL)
   (list NIL 3 NIL NIL NIL 13 NIL NIL NIL 23)
   (list NIL NIL NIL 8 NIL NIL NIL 18 NIL NIL)
   (list NIL 2 NIL NIL NIL 12 NIL NIL NIL 22)
   (list NIL NIL NIL 9 NIL NIL NIL 19 NIL NIL)
   (list NIL 1 NIL NIL NIL 11 NIL NIL NIL 21)
   (list NIL NIL NIL 10 NIL NIL NIL 20 NIL NIL)))

(defun tabuleiro-f ()
  (tabuleiro-aleatorio))

(defun tabuleiro-com-cavalo-mexido ()
  '((NIL 25 54 89 21 8 36 14 41 96)
    (78 47 T 23 5 49 13 12 26 60)
    (0 27 17 83 34 93 74 52 45 80)
    (69 9 95 77 55 39 91 73 57 30)
    (24 15 22 86 1 11 68 79 76 72)
    (81 48 32 2 64 16 50 37 29 71)
    (99 51 6 18 53 28 7 63 10 88)
    (59 42 46 85 90 75 87 43 20 31)
    (3 61 58 44 65 82 19 4 35 62)
    (33 70 84 40 66 38 92 67 98 97)))


(defun tabuleiro-com-cavalo-2 ()
'((NIL 97 96 95 94 93 92 91 90 89)
  (1 2 3 4 5 55 6 7 8 9)
  (NIL T NIL 78 NIL 7 NIL 9 NIL 11)
  (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
  (NIL NIL 22 NIL NIL NIL NIL NIL 33 NIL)
  (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
  (NIL NIL NIL 88 NIL NIL NIL 44 NIL NIL)
  (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
  (NIL NIL NIL NIL 77 NIL NIL NIL NIL NIL)
  (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)))

(defun contar-casas-nao-nulas (tabuleiro)
  (let ((soma 0))
    (mapcar (lambda (linha) (mapcar (lambda (casa) (if casa (incf soma) (incf soma 0))) linha)) tabuleiro) soma))


(defun tabuleiro-sucessor-a ()
  (list (list NIL 20 44 NIL NIL NIL NIL NIL NIL NIL)
        (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
        (list NIL T 30 NIL NIL NIL NIL NIL NIL NIL)
        (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
        (list NIL NIL NIL 22 NIL NIL NIL NIL NIL NIL)
        (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
        (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
        (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
        (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
        (list NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)))
