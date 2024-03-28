;;;; puzzle.lisp
;;;; Disciplina de IA - 2023 / 2024
;;;; Ficheiro responsável pela lógica do jogo
;;;; Autor: João Jardin Nº20180002 SW-04

;;; Tabuleiros


(load "C:/Users/PC/Desktop/Universidade/IA2023/IA/Projeto/P2/jogo.lisp")
(load "C:/Users/PC/Desktop/Universidade/IA2023/IA/Projeto/P2/algoritmo.lisp")


(defun iniciar-jogo ()
  "Inicia o jogo perguntando o modo de jogo, define o tempo limite e a profundidade para o algoritmo alfabeta."
  (format t "Escolha o modo de jogo:~%1 - Humano vs Computador~%2 - Computador vs Computador~%3 - Sair ~%Escolha uma opção: ")
  (let ((modo-de-jogo (read)))
    (cond
      ((not (numberp modo-de-jogo))
       (format t "Escolha inválida. Por favor, insira um número.~%"))
      ((= modo-de-jogo 3)
       (format t "Jogo encerrado.~%"))
      (t
       (format t "Defina o tempo limite para jogadas do computador (1000 a 5000 milissegundos): ")
       (let ((tempo-limite (read)))
         (cond
           ((not (and (numberp tempo-limite) (>= tempo-limite 1000) (<= tempo-limite 5000)))
            (format t "Valor inválido. Por favor, insira um número entre 1000 e 5000 milissegundos.~%"))
           (t
            (format t "Escolha a profundidade para o algoritmo alfabeta (1 a 5): ")
            (let ((profundidade (read)))
              (cond
                ((not (and (numberp profundidade) (>= profundidade 1) (<= profundidade 5)))
                 (format t "Valor inválido. Por favor, insira um número entre 1 e 5.~%"))
                (t
                 (cond
                   ((= modo-de-jogo 1)
                    (format t "Quem deve começar a partida?~%1 - Humano~%2 - Computador~%Escolha uma opção: ")
                    (let ((escolha-inicial (read)))
                      (cond
                        ((not (numberp escolha-inicial))
                         (format t "Escolha inválida. Por favor, insira um número.~%"))
                        (t
                         (jogar (no-teste) (if (= escolha-inicial 1) *jogador1* *jogador2*) tempo-limite profundidade modo-de-jogo)))))
                   ((= modo-de-jogo 2)
                    (jogar (no-teste) *jogador1* tempo-limite profundidade modo-de-jogo))
                   (t
                    (format t "Escolha inválida. Tente novamente.~%")))))))))))))



(defun jogar (no jogador tempo-limite profundidade modo-de-jogo &optional ultimo-resultado-valido)
  "Executa um turno do jogo e continua recursivamente até que o jogo termine."
  (cond
    ;; Verifica se o jogo chegou ao fim
    ((no-solucao no)
     (progn
       (format t "Pontos finais do Jogador 1: ~A~%" (pontos-jogador1 no))
       (format t "Pontos finais do Jogador 2: ~A~%" (pontos-jogador2 no))))
    (t
     (let ((movimentos-possiveis (gerar-movimentos-possiveis (estado-do-no no) jogador (operadores))))
       (cond
         ;; Se o jogador atual não tem movimentos válidos
         ((null movimentos-possiveis)
          (progn
            (format t "~%Jogador ~A não tem movimentos válidos. Passando a vez...~%" jogador)
            (jogar no (trocar-jogador jogador) tempo-limite profundidade modo-de-jogo ultimo-resultado-valido)))
         (t
          (let* ((novo-no (cond
                           ((= modo-de-jogo 1) (if (= jogador *jogador1*) 
                                                   (jogada-humano no jogador) 
                                                   (jogada-computador no profundidade jogador tempo-limite)))
                           ((= modo-de-jogo 2) (jogada-computador no profundidade jogador tempo-limite)))))
            (cond
              ((null novo-no)
               (cond
                 ((eq ultimo-resultado-valido 'nenhum-movimento)
                  (progn
                    (format t "~%Ambos os jogadores estão sem jogadas válidas.")
                    (format t "~%Fim do jogo|~%")
                    (format t "Pontos finais do Jogador 1: ~A~%" (pontos-jogador1 no))
                    (format t "Pontos finais do Jogador 2: ~A~%" (pontos-jogador2 no))
                    (cond
                      ((> (pontos-jogador1 no) (pontos-jogador2 no))
                       (format t "Jogador 1 vence!~%"))
                      ((< (pontos-jogador1 no) (pontos-jogador2 no))
                       (format t "Jogador 2 vence!~%"))
                      (t
                       (format t "Empate!~%")))))
                 (t
                  (jogar no (trocar-jogador jogador) tempo-limite profundidade modo-de-jogo 'nenhum-movimento))))
              (t
               (progn
                 (pprint (estado-do-no novo-no))
                 (format t "~%")
                 (format t "~%Pontos do Jogador 1: ~A~%" (pontos-jogador1 novo-no))
                 (format t "Pontos do Jogador 2: ~A~%" (pontos-jogador2 novo-no))
                 (format t "~%")
                 (jogar novo-no (trocar-jogador jogador) tempo-limite profundidade modo-de-jogo nil)))))))))))



