;;;; menu.lisp
;;;; Exemplo de utilização de menus
;;;; E de definição de caminhos para os ficheiros
;;;; Autor: Cédric Grueau
;;;; Data: 2 de dezembro de 2016
;;;; Versão 1

(defun puzzle ()
  "Permite iniciar o programa"
  (load (concatenate 'string (diretoria-atual) "problema.lisp"))
  (load (concatenate 'string (diretoria-atual) "procura.lisp"))
  (menu-principal)
  )

(defun diretoria-atual ()
  "Define o caminho para os ficheiros do projeto a partir da raiz"
  ; Para mac
  ;(let ((caminho "/Users/paulo/Dropbox/EST-IPS/Aulas/IA_15_16/laboratorios/lab5/"))
  ; para PC MS Windows
  (let ((path "C:\\Documents and Settings\\projetos-ia\\projecto1\\"))
    path
    )
  )

;;; MENU PRINCIPAL
(defun menu-principal ()
	"Apresenta o menu principal com as opcões do programa"
  (loop
    (progn
      (format t "~% ------------------------------------------------------")
      (format t "~%|         PUZZLE DOS PONTOS E  DAS CAIXAS              |")
      (format t "~%|                                                      |")
      (format t "~%|            1-Resolver um tabuleiro                   |")
      (format t "~%|            2-Regras do Puzzle                        |")
      (format t "~%|            3-Mostrar um Puzzle                       |")
      (format t "~%|            3-Sair                                    |")
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