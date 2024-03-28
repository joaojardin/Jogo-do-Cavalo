;;;; procura.lisp
;;;; Disciplina de IA - 2023 / 2024
;;;; Ficheiro responsável pela implementação dos algoritmos a utilizar
;;;; Autor: João Jardin Nº20180002 SW-04


(defun abertos-bfs (abertos sucessores)
  (append abertos sucessores))

(defun abertos-dfs (abertos sucessores)
  (append sucessores abertos))


(defun colocar-sucessores-em-abertos (abertos sucessores)
  (sort (append abertos sucessores) #'< :key #'no-custo))


(defun no-existep (no lista)
  (some #'(lambda (n) (equal (estado-do-no no) (estado-do-no n))) lista))

; (pprint(bfs (no-teste-b) #'(lambda (no) (no-solucaop no 60)) #'sucessores (operadores))) (para 60 pontos)
; (pprint(bfs (no-teste) #'(lambda (no) (no-solucaop no 94)) #'sucessores (operadores))) (caso em que nó inicial é nó objetivo)


;; Definição de Variáveis Globais
(defvar *abertos* nil)
(defvar *fechados* nil)


(defun bfs (no-inicial objetivo-func sucessores-func operadores)
  (setf *abertos* (list no-inicial))
  (setf *fechados* nil)

  (cond ((funcall objetivo-func no-inicial) no-inicial)
        (t
         (let ((ultimo-no-expandido nil))
           (loop
             (when (null *abertos*)
               (return ultimo-no-expandido)) 

             (let ((no-atual (pop *abertos*)))
               (setf ultimo-no-expandido no-atual) 

               (when (funcall objetivo-func no-atual)
                 (return no-atual))

               (let ((sucessores (funcall sucessores-func no-atual operadores nil nil)))
                 (dolist (sucessor sucessores)
                   (unless (no-existep sucessor *abertos*)
                     (unless (no-existep sucessor *fechados*)
                       (push sucessor *abertos*)))))

               (push no-atual *fechados*)))))))



(defun dfs (no-inicial objetivo-func sucessores-func operadores &optional (profundidade-maxima 8))
  (setf *abertos* (list no-inicial))
  (setf *fechados* nil)
  
  (cond ((funcall objetivo-func no-inicial) no-inicial)
        (t
  (let ((ultimo-no-expandido nil))
    (loop while *abertos* do
      (let ((no (pop *abertos*)))
        (setf ultimo-no-expandido no)

        (when (or (funcall objetivo-func no)
                  (>= (profundidade-do-no no) profundidade-maxima))
          (return-from dfs no))

        (push no *fechados*)

        (let ((sucessores (funcall sucessores-func no operadores)))
          (dolist (sucessor sucessores)
            (unless (or (no-existep sucessor *abertos*)
                        (no-existep sucessor *fechados*))
              (setf *abertos* (cons sucessor *abertos*)))))))
    ultimo-no-expandido))))


; (pprint(a* (no-teste) #'(lambda (no) (no-solucaop no 94)) #'sucessores (operadores) #'heuristica)) (caso em que nó inicial é nó objetivo)

(defun a* (no-inicial objetivo-func sucessores-func operadores h-func)
  (setf *abertos* (list no-inicial))
  (setf *fechados* nil)

  (cond ((funcall objetivo-func no-inicial) no-inicial)
        (t
  (let ((ultimo-no-expandido nil))
    (loop
      (when (null *abertos*)
        (return ultimo-no-expandido))

      (let ((no-atual (reduce (lambda (n1 n2)
                                (if (< (no-custo n1) (no-custo n2))
                                    n1
                                    n2))
                              *abertos*)))
        (setf ultimo-no-expandido no-atual) 
        (setf *abertos* (remove no-atual *abertos*))

        (when (funcall objetivo-func no-atual)
          (return no-atual))

        (push no-atual *fechados*)

        (let ((sucessores (funcall sucessores-func no-atual operadores 'a*)))
          (dolist (sucessor sucessores)
            (unless (or (member sucessor *abertos*)
                        (member sucessor *fechados*))
              (push sucessor *abertos*))))))))))

(defun no-solucaop (no objetivo)
  (>= (pontos-do-no no) objetivo))

(defun funcao (b l valor-t)
  (- (/ (* b (- (expt b l) 1)) (- b 1)) valor-t))

(defun bisseccao (func a b l valor-t y)
  (let ((fa (funcall func a l valor-t))
        (fb (funcall func b l valor-t)))
    (cond
      ((< (abs (- a b)) y) (/ (+ a b) 2))
      ((zerop fa) a)
      ((zerop fb) b)
      (t (let ((apro (/ (+ a b) 2)))
           (let ((fapro (funcall func apro l valor-t)))
             (cond
               ((or (zerop fapro) (< (abs fapro) y)) apro) 
               ((> fapro 0) (bisseccao func a apro l valor-t y)) 
               (t (bisseccao func apro b l valor-t y))))))))) 

(defun fator-ramificacao (l valor-t)
  (bisseccao #'funcao 0.1 10.0 l valor-t 0.0001))


(defun exibir-informacoes (no algoritmo objetivo-pontos)
  (let* ((nos-abertos (length *abertos*))
         (nos-fechados (length *fechados*))
         (nos-gerados (+ nos-abertos nos-fechados))
         (nos-expandidos nos-fechados)
         (estado-final (estado-do-no no))
         (profundidade (profundidade-do-no (or no 0)))
         (pontos-acumulados (pontos-do-no (or no 0)))
         (heuristica (when (and (eql algoritmo 'a*) no) (h-do-no no)))
         (penetrancia (if (> nos-expandidos 0) (/ profundidade (float nos-gerados)) 0))
         (fator-ramificacao-calculado (fator-ramificacao profundidade nos-gerados))
         (objetivo-atingido (>= pontos-acumulados objetivo-pontos))
         (caminho-arquivo "C:/Users/PC/Desktop/Universidade/IA2023/IA/Projeto/estatisticas.txt"))
    
    (format t "~%Algoritmo: ~A~%" (string-upcase algoritmo))
    (format t "~%Jogadas efetuadas:~%")
    (pprint (mostrar-jogadas no))
    (format t "~%~%Estado final:~%")
    (pprint estado-final)
    (format t "~%~%Nós Gerados: ~A~%" nos-gerados)
    (format t "Nós Expandidos: ~A~%" nos-expandidos)
    (format t "Profundidade: ~A~%" profundidade)
    (format t "Pontos Acumulados: ~A~%" pontos-acumulados)
    (when heuristica
      (format t "Heurística: ~A~%" heuristica))
    (format t "Penetrância: ~A~%" penetrancia)
    (format t "Fator de Ramificação Média: ~A~%" fator-ramificacao-calculado)
    (format t "Objetivo ~:[não atingido.~;atingido.~]~%~%" objetivo-atingido)
    (with-open-file (stream caminho-arquivo
                            :direction :output 
                            :if-exists :append 
                            :if-does-not-exist :create)
      (format stream "~%Algoritmo: ~A~%" (string-upcase algoritmo))
      (format stream "Estado final:~%")
      (pprint estado-final stream)
      (format stream "~%~%Nós Gerados: ~A~%" nos-gerados)
      (format stream "Nós Expandidos: ~A~%" nos-expandidos)
      (format stream "Profundidade: ~A~%" profundidade)
      (format stream "Pontos Acumulados: ~A~%" pontos-acumulados)
      (when heuristica
        (format stream "Heurística: ~A~%" heuristica))
      (format stream "Penetrância: ~A~%" penetrancia)
      (format stream "Fator de Ramificação Média: ~A~%" fator-ramificacao-calculado)
      (format stream "Objetivo ~:[não atingido.~;atingido.~]~%~%" objetivo-atingido))))

;;(setq resultado (bfs (no-teste-b) #'(lambda (no) (no-solucaop no 60)) #'sucessores (operadores)))
;;(mostrar-jogadas resultado)
(defun mostrar-jogadas (no)
  (cond ((null no) '()) 
        (t (append (mostrar-jogadas (pai-do-no no)) 
                   (list (estado-do-no no)))))) 
