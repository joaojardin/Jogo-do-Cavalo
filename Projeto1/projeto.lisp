;;;; projeto.lisp
;;;; Disciplina de IA - 2023 / 2024
;;;; Ficheiro responsável para interação com o utilizador
;;;; Autor: João Jardin Nº20180002 SW-04



(load "C:/Users/PC/Desktop/Universidade/IA2023/IA/Projeto/puzzle.lisp")
(load "C:/Users/PC/Desktop/Universidade/IA2023/IA/Projeto/procura.lisp")


(defun obter-problemas ()
  (make-pathname :host "c" 
                 :directory '(:absolute "Users" "PC" "Desktop" "Universidade" "IA2023" "IA" "Projeto" "lisp")
                 :name "problemas" 
                 :type "dat"))


(defun ler-tabuleiros ()
  (let ((file-path (obter-problemas)))
    (cond
      ((probe-file file-path)
       (with-open-file (ficheiro file-path)
         (ler-tabuleiros-aux ficheiro)))
      (t
       (format t "Falha ao abrir o arquivo: ~a~%" file-path)
       '()))))

(defun ler-tabuleiros-aux (ficheiro)
  (let ((next (read ficheiro nil 'eof)))
    (cond
      ((equal next 'eof) '())
      (t (cons next (ler-tabuleiros-aux ficheiro))))))



(defun menu-inicial ()
  (format t "Bem-vindo ao Jogo do Cavalo!~%")
  (format t "Escolha uma opção:~%")
  (format t "1. Jogar~%")
  (format t "2. Sair~%")
  (format t "Opção: ")
  (let ((opcao (read)))
    (cond
      ((= opcao 1) (menu-jogar))
      ((= opcao 2) (format t "Obrigado por jogar. Até breve!~%"))
      (t (format t "Opção inválida. Por favor, escolha novamente.~%")
         (menu-inicial)))))


(defun menu-jogar ()
  (format t "Escolha um problema:
              1. Problema A (Objetivo: 70 pontos)
              2. Problema B (Objetivo: 60 pontos)
              3. Problema C (Objetivo: 270 pontos)
              4. Problema D (Objetivo: 600 pontos)
              5. Problema E (Objetivo: 300 pontos)
              6. Problema F (Objetivo: 2000 pontos)
              0. Voltar
              Opção: ")
  (let ((opcao-problema (read)))
    (case opcao-problema
      (1 (menu-algoritmo 'problema-a))
      (2 (menu-algoritmo 'problema-b))
      (3 (menu-algoritmo 'problema-c))
      (4 (menu-algoritmo 'problema-d))
      (5 (menu-algoritmo 'problema-e))
      (6 (menu-algoritmo 'problema-f))
      (0 (menu-inicial))
      (t (format t "Opção inválida. Tente novamente.~%")))))


(defun jogar-com-algoritmo (problema algoritmo &optional limite-profundidade)
  (let* ((no-inicial (case problema
                       ('problema-a (no-teste-a))
                       ('problema-b (no-teste-b))
                       ('problema-c (no-teste-c))
                       ('problema-d (no-teste-d))
                       ('problema-e (no-teste-e))
                       ('problema-f (no-teste-f))))
         (funcao-objetivo (case problema
                           ('problema-a #'objetivo-a)
                           ('problema-b #'objetivo-b)
                           ('problema-c #'objetivo-c)
                           ('problema-d #'objetivo-d)
                           ('problema-e #'objetivo-e)
                           ('problema-f #'objetivo-f)))
         (pontos-objetivo (case problema
                            ('problema-a 70)
                            ('problema-b 60)
                            ('problema-c 270)
                            ('problema-d 600)
                            ('problema-e 300)
                            ('problema-f 2000))))
    (cond
      ((eql algoritmo 'bfs) 
       (bfs no-inicial funcao-objetivo #'sucessores (operadores)))
      ((eql algoritmo 'dfs) 
       (dfs no-inicial funcao-objetivo #'sucessores (operadores) limite-profundidade))
      ((eql algoritmo 'a*) 
       (a* no-inicial funcao-objetivo #'sucessores (operadores) #'heuristica)))))

(defun menu-algoritmo (problema)
  (format t "Escolha um algoritmo para resolver o problema:
              1. BFS
              2. DFS
              3. A*
              0. Voltar
              Opção: ")
  (let ((input (read-line)))
    (let* ((opcao-algoritmo (parse-integer input :junk-allowed t))
           (objetivo-pontos (case problema
                              ('problema-a 70)
                              ('problema-b 60)
                              ('problema-c 270)
                              ('problema-d 600)
                              ('problema-e 300)
                              ('problema-f 2000))))
      (cond
        ((and (numberp opcao-algoritmo) (= opcao-algoritmo 1))
         (exibir-informacoes (jogar-com-algoritmo problema 'bfs) 'bfs objetivo-pontos))
        ((and (numberp opcao-algoritmo) (= opcao-algoritmo 2))
         (let ((limite-profundidade (menu-profundidade-maxima)))
           (exibir-informacoes (jogar-com-algoritmo problema 'dfs limite-profundidade) 'dfs objetivo-pontos)))
        ((and (numberp opcao-algoritmo) (= opcao-algoritmo 3))
         (exibir-informacoes (jogar-com-algoritmo problema 'a*) 'a* objetivo-pontos))
        ((and (numberp opcao-algoritmo) (= opcao-algoritmo 0))
         (menu-jogar))
        (t (format t "Opção inválida. Tente novamente.~%")
           (menu-algoritmo problema)))))) 






(defun menu-profundidade-maxima ()
  (format t "Escolha a profundidade máxima para o DFS (pressione Enter para valor por defeito 8): ")
  (let ((input (read-line)))
    (cond
      ((equal input "") 8)
      ((every #'digit-char-p input) (parse-integer input))
      (t (format t "Entrada inválida. Por favor, insira um número.~%")
         (menu-profundidade-maxima)))))

;;objetivos

(defun objetivo-a (no)
  (no-solucaop no 70))

(defun objetivo-b (no)
  (no-solucaop no 60))

(defun objetivo-c (no)
  (no-solucaop no 270))

(defun objetivo-d (no)
  (no-solucaop no 600))

(defun objetivo-e (no)
  (no-solucaop no 300))

(defun objetivo-f (no)
  (no-solucaop no 2000))

