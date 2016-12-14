;;;; menu.lisp
;;;; Exemplo de utilização de menus
;;;; E de definição de caminhos para os ficheiros
;;;; Autor: Cédric Grueau
;;;; Data: 2 de dezembro de 2016
;;;; Versão 1


(defun pedir-directoria ()
	"Pede a directoria dos ficheiros ao utilizador"
	(progn
		(format t "Insira o diretório: ")
		(read)
	)
)

(defun puzzle ()
	"Permite iniciar o programa"
	(let 
		(
			(diretoria-atual (pedir-directoria))
		)
		(progn
			(load (compile-file (concatenate 'string diretoria-atual "puzzle.lisp")))
			(load (compile-file (concatenate 'string diretoria-atual "procura.lisp")))
			(menu-principal)
		)
	)
)


;;; MENU PRINCIPAL
(defun menu-principal ()
	"Apresenta o menu principal com as opcões do programa"
  (loop
    (progn
      (format t "~% ------------------------------------------------------")
      (format t "~%|         PUZZLE DOS PONTOS E DAS CAIXAS               |")
      (format t "~%|                                                      |")
      (format t "~%|            1-Resolver um tabuleiro                   |")
      (format t "~%|            2-Regras do Puzzle                        |")
      (format t "~%|            3-Mostrar um Puzzle                       |")
      (format t "~%|            4-Sair                                    |")
      (format t "~%|                                                      |")
      (format t "~% ------------------------------------------------------")
      (format t "~%~%Escolha:")
      )
    (cond ((not (let ((escolha (read)))
               (cond 
                ((and (< escolha 5) (> escolha 0)) (case escolha
                                                    (1 (progn (menu-jogar) t))
                                                    (2 (progn (regras) t))
                                                    (3 (progn (imprime-tabuleiro) t))
                                                    (4 (progn (format t "PROGRAMA TERMINADO") nil)))
                )
                ( T (progn  (format t "~%ESCOLHA INVALIDA~%~%Escolha: ")
                            (setf escolha (read))
                            )))
               
               )) (return)))
    )
  )

(defun menu-jogar()
	
)

;;;Regras do puzzle
(defun regras () 
    (format t "
   -------------------------- Regras do Puzzle dos Pontos e das Caixas -------------------
  |                                                                                      |
  |                                                                                      |
  |                                                                                      |
  |                                                                                      |
  |                                                                                      |
  |                                                                                      |
  |                                                                                      |
  |                                                                                      |
  ----------------------------------------------------------------------------------------
  "
  )
)

(defun imprime-tabuleiro ()
	
)


;*standard-output*
(defun imprimir-resultado (stream resultado)
	(progn 
		(write-line "Resultado:" stream)
		(write-line (format nil "Nó objetivo: ~a" (first resultado)) stream)
		(write-line (format nil "Tempo de resolução: ~a" (second resultado)) stream)
		(write-line (format nil "Número de nós gerado: ~a" (third resultado)) stream)
		(write-line (format nil "Número de nós expandidos: ~a" (fourth resultado)) stream)
		(write-line (format nil "Profundidade: ~a" (fifth resultado)) stream)
		(write-line (format nil "Penetrancia: ~,4f" (float (sixth resultado))) stream)
		(write-line (format nil "Fator de ramificação: ~,4f" (float (seventh resultado))) stream)
	)
)