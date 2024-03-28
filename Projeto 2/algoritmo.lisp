;;;; algoritmo.lisp
;;;; Disciplina de IA - 2023 / 2024
;;;; Ficheiro responsável pela implementação do algoritmo alfabeta
;;;; Autor: João Jardin Nº20180002 SW-04

;;; Tabuleiros



(defvar *cortes-alfa* 0)
(defvar *cortes-beta* 0)
(defvar *nos-analisados* 0)


(defun no-solucao (no)
  "Verifica se o nó é uma solução, retornando o resultado do jogo."
  (let* ((tabuleiro (estado-do-no no))
         (pontos-jogador1 (pontos-jogador1 no))
         (pontos-jogador2 (pontos-jogador2 no))
         (movimentos-possiveis-jogador1 (gerar-movimentos-possiveis tabuleiro *jogador1* (operadores)))
         (movimentos-possiveis-jogador2 (gerar-movimentos-possiveis tabuleiro *jogador2* (operadores))))
    (cond
      ;; Caso ambos os jogadores não tenham movimentos possíveis.
      ((and (null movimentos-possiveis-jogador1) (null movimentos-possiveis-jogador2))
       (progn
         (format t "~%Fim do jogo!~%")
         (cond
           ((> pontos-jogador1 pontos-jogador2) (format t "Jogador 1 venceu!~%"))
           ((< pontos-jogador1 pontos-jogador2) (format t "Jogador 2 venceu!~%"))
           (t (format t "Empate!~%")))
         t))  ; Retorna verdadeiro indicando que o jogo terminou.

      ;; Caso um dos jogadores não tenha movimentos e o outro já tenha ultrapassado seus pontos.
      ((or (and (null movimentos-possiveis-jogador1) (> pontos-jogador2 pontos-jogador1))
           (and (null movimentos-possiveis-jogador2) (> pontos-jogador1 pontos-jogador2)))
       (progn
         (format t "~%Fim do jogo!~%")
         (if (> pontos-jogador1 pontos-jogador2)
             (format t "Jogador 1 venceu!~%")
             (format t "Jogador 2 venceu!~%"))
         t))  ; Retorna verdadeiro indicando que o jogo terminou.

      ;; Caso um jogador não tenha movimentos e o outro ainda tenha mais pontos.
      ((and (null movimentos-possiveis-jogador1) (> pontos-jogador2 pontos-jogador1))
       (progn
         (format t "~%Fim do jogo!~%")
         (format t "Jogador 1 não tem mais movimentos possíveis.~%")
         (format t "Jogador 2 venceu!~%")
         t))  ; Retorna verdadeiro indicando que o jogo terminou.

      ((and (null movimentos-possiveis-jogador2) (> pontos-jogador1 pontos-jogador2))
       (progn
         (format t "~%Fim do jogo!~%")
         (format t "Jogador 2 não tem mais movimentos possíveis.~%")
         (format t "Jogador 1 venceu!~%")
         t))  ;

      ;; Caso o jogo ainda possa continuar.
      (t nil))))



(defun avaliar-no (no tipo-jogador)
  (let* ((tabuleiro (estado-do-no no))
         (pontos-jogador1 (third no))
         (pontos-jogador2 (fourth no))
         (diferenca-pontos (- pontos-jogador1 pontos-jogador2))
         (mobilidade-jogador1 (length (gerar-movimentos-possiveis tabuleiro *jogador1* (operadores))))
         (mobilidade-jogador2 (length (gerar-movimentos-possiveis tabuleiro *jogador2* (operadores)))))

    (cond ((eql tipo-jogador *jogador1*)
           (+ diferenca-pontos
              (* 0.5 mobilidade-jogador1)
              (* -0.5 mobilidade-jogador2)))
          (t
           (- diferenca-pontos
              (* 0.5 mobilidade-jogador2)
              (* -0.5 mobilidade-jogador1))))))


(defun alfabeta (no profundidade alfa beta jogador &optional (chamada-final t))
  (let ((inicio-tempo (get-internal-real-time)))
    ;; Inicialização dos contadores se esta for a chamada final
    (when chamada-final
      (setf *cortes-alfa* 0)
      (setf *cortes-beta* 0)
      (setf *nos-analisados* 0))

    (cond ((zerop profundidade)
           (incf *nos-analisados*)
           (values (avaliar-no no jogador) nil))
          (t (multiple-value-bind (valor jogada)
                 (alfabeta-aux (sucessores no (operadores) jogador) profundidade
                               (if (= jogador *jogador1*) most-negative-fixnum most-positive-fixnum)
                               nil alfa beta jogador)
               (when chamada-final
                 (let ((tempo-final (* (/ (- (get-internal-real-time) inicio-tempo) internal-time-units-per-second) 1000)))
                   (let ((estatistica (format nil "~% Jogador: ~A, Tempo Gasto: ~A ms, Nós analisados: ~A, Profundidade: ~A, Valor: ~A, Cortes Alfa: ~A, Cortes Beta: ~A"
                                             jogador tempo-final *nos-analisados* profundidade valor *cortes-alfa* *cortes-beta*)))
                     ;; Escreve no log
                     (escrever-ficheiro estatistica)
                     (format t "~A~%" estatistica))))
               (values valor jogada))))))

(defun alfabeta-aux (sucessores profundidade melhor-valor melhor-jogada alfa beta jogador)
  (cond 
   ((null sucessores)
    (values melhor-valor melhor-jogada))
   (t
    (let* ((sucessor-info (first sucessores))
           (sucessor (first sucessor-info))
           (jogada-linha (second sucessor-info))
           (jogada-coluna (third sucessor-info)))
      (multiple-value-bind (valor _)
          (alfabeta sucessor (- profundidade 1) alfa beta (trocar-jogador jogador) nil)
        (incf *nos-analisados*)
        (let ((novo-melhor-valor (cond ((= jogador *jogador1*) (max valor melhor-valor))
                                       (t (min valor melhor-valor))))
              (nova-melhor-jogada (cond ((or (and (= jogador *jogador1*) (> valor melhor-valor))
                                             (and (not (= jogador *jogador1*)) (< valor melhor-valor)))
                                         (list jogada-linha jogada-coluna))
                                        (t melhor-jogada)))
              (novo-alfa (cond ((= jogador *jogador1*) (max alfa valor))
                               (t alfa)))
              (novo-beta (cond ((not (= jogador *jogador1*)) (min beta valor))
                               (t beta))))
          (cond
           ((or (and (= jogador *jogador1*) (>= novo-alfa beta))
                (and (not (= jogador *jogador1*)) (<= novo-beta alfa)))
            (if (= jogador *jogador1*)
                (incf *cortes-beta*)
                (incf *cortes-alfa*))
            (return-from alfabeta-aux (values novo-melhor-valor nova-melhor-jogada)))
           (t (alfabeta-aux (rest sucessores) profundidade novo-melhor-valor nova-melhor-jogada novo-alfa novo-beta jogador)))))))))

(defun jogada-humano (no jogador)
  "Permite ao jogador humano fazer uma jogada no tabuleiro."
  (format t "~%Tabuleiro atual:~%")
  (pprint (estado-do-no no))

  (let ((movimentos-possiveis (gerar-movimentos-possiveis (estado-do-no no) jogador (operadores))))
    (format t "~%Movimentos possíveis do Jogador ~A: ~A~%" jogador movimentos-possiveis)
    (format t "~%Jogador ~A, por favor insira a nova linha para o cavalo: " jogador)
    (let ((linha (read)))
      (format t "~%Agora insira a nova coluna para o cavalo: ")
      (let ((coluna (read)))
        (let* ((posicao-atual (posicao-cavalo (estado-do-no no) jogador))
               (deslocamento-linha (- linha (first posicao-atual)))
               (deslocamento-coluna (- coluna (second posicao-atual)))
               (operador (identificar-operador deslocamento-linha deslocamento-coluna)))
          (cond
            ((and operador (movimento-valido linha coluna (estado-do-no no) jogador))
             (let ((novo-no (novo-sucessor no operador jogador)))
               (cond
                 (novo-no
                  (let ((pontos-ganhos (if (= jogador *jogador1*)
                                           (- (pontos-jogador1 novo-no) (pontos-jogador1 no))
                                           (- (pontos-jogador2 novo-no) (pontos-jogador2 no)))))
                    (format t "~%Pontos Ganhos: ~A~%" pontos-ganhos))
                  novo-no)
                 (t
                  (format t "~%Movimento inválido. Tente novamente.~%")
                  (jogada-humano no jogador)))))
            (t
             (format t "~%Movimento inválido. Tente novamente.~%")
             (jogada-humano no jogador))))))))



(defun jogada-computador (no profundidade jogador tempo-limite)
  "Realiza uma jogada do computador para o jogador especificado dentro do tempo limite."
  (format t "~%Vez do jogador: ~A~%" jogador)
  (let ((inicio-tempo (get-internal-real-time))
        (movimentos-possiveis (gerar-movimentos-possiveis (estado-do-no no) jogador (operadores))))
    (cond
      ((null movimentos-possiveis)
       (format t "~%Jogador ~A não tem movimentos válidos. Passando a vez.~%" jogador)
       (jogada-computador no profundidade (trocar-jogador jogador) tempo-limite))
      (t
       (let ((alfa most-negative-fixnum)
             (beta most-positive-fixnum))
         (multiple-value-bind (valor jogada) (alfabeta no profundidade alfa beta jogador)
           (cond
             ((>= (- (get-internal-real-time) inicio-tempo) tempo-limite)
              (format t "Tempo limite excedido para a jogada do computador.~%")
              nil)
             ((and valor jogada)
              (let* ((posicao-atual (posicao-cavalo (estado-do-no no) jogador))
                     (nova-linha (first jogada))
                     (nova-coluna (second jogada))
                     (deslocamento-linha (- nova-linha (first posicao-atual)))
                     (deslocamento-coluna (- nova-coluna (second posicao-atual)))
                     (operador (identificar-operador deslocamento-linha deslocamento-coluna)))
                (cond
                  (operador
                   (let ((novo-no (novo-sucessor no operador jogador)))
                     (format t "~%Jogador ~A move cavalo para (~A, ~A).~%" jogador nova-linha nova-coluna)
                     (let ((pontos-ganhos (if (= jogador *jogador1*)
                                              (- (pontos-jogador1 novo-no) (pontos-jogador1 no))
                                              (- (pontos-jogador2 novo-no) (pontos-jogador2 no)))))
                       (format t "Pontos Ganhos: ~A~%" pontos-ganhos))
                     novo-no))
                  (t
                   (format t "~%Movimento inválido.~%")
                   nil))))
             (t
              (format t "~%Nenhuma jogada válida encontrada.~%")
              nil))))))))



(defun identificar-operador (deslocamento-linha deslocamento-coluna)
  "Identifica o operador de movimento do cavalo com base nos deslocamentos de linha e coluna."
  (cond
    ((and (= deslocamento-linha 2) (= deslocamento-coluna 1)) #'operador-1)
    ((and (= deslocamento-linha 1) (= deslocamento-coluna 2)) #'operador-2)
    ((and (= deslocamento-linha -1) (= deslocamento-coluna 2)) #'operador-3)
    ((and (= deslocamento-linha -2) (= deslocamento-coluna 1)) #'operador-4)
    ((and (= deslocamento-linha -2) (= deslocamento-coluna -1)) #'operador-5)
    ((and (= deslocamento-linha -1) (= deslocamento-coluna -2)) #'operador-6)
    ((and (= deslocamento-linha 1) (= deslocamento-coluna -2)) #'operador-7)
    ((and (= deslocamento-linha 2) (= deslocamento-coluna -1)) #'operador-8)
    (t nil))) 



(defun escrever-ficheiro (mensagem)
  (let ((caminho "C:/Users/PC/Desktop/Universidade/IA2023/IA/Projeto/P2/log.dat"))
    (with-open-file (stream caminho
                            :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)
      (format stream "~A~%" mensagem))))