;;;; menu.lisp
;;;; Exemplo de utilização de menus
;;;; E de definição de caminhos para os ficheiros
;;;; Autor: Cédric Grueau
;;;; Data: 2 de dezembro de 2016
;;;; Versão 1


(defun iniciar ()	
"Função que inicializa o programa, chamando a função que apresenta o menu inicial."
	(progn
		(compile-file (concatenate 'string (diretoria-atual)"procura.lisp"))  
		(compile-file (concatenate 'string (diretoria-atual)"puzzle.lisp"))
		(load (concatenate 'string (diretoria-atual)"procura.ofasl")) 
		(load (concatenate 'string (diretoria-atual)"puzzle.ofasl"))
		(menu-principal)
	)
)

(defun diretoria-atual () 
"Função que define um caminho para leitura dos ficheiros."
	(let (
			;(path-ricardo)
			(path-tiago  "C:\\Users\\pcts\\Desktop\\ProjIA\\Projeto\\"))
			;(path-professor ""))
			
		path-tiago
		;path-ricardo
		;path-professor
	)
)

(defun pedir-directoria ()
	"Pede a directoria dos ficheiros ao utilizador"
	(progn
		(format t "Insira o diretório: ")
		(read)
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
	
	(let*
	(
		(tabuleiro (escolher-tabuleiro))
		(objetivo (obter-objectivo tabuleiro))
		(algoritmo (escolher-algoritmo))
		(profundidade (cond ((eql algoritmo 'dfs) (obter-profundidade)) (T 9999)))
		
	)
	
	(cond
		((eql algoritmo 'dfs) (resultado-simulacao (teste-dfs objetivo profundidade tabuleiro)))
		((eql algoritmo 'bfs) (resultado-simulacao (teste-bfs objetivo tabuleiro)))
		(T nil)

	)
	)
	
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

(defun escolher-tabuleiro() 

(progn
		(format t "~%>")
		(format t "~%> Escolha tabuleiro inicial do problema ")
		
		(format t "~%> 	a) Tabuleiro A ")
		(format t "~%> 	b) Tabuleiro B ")
		(format t "~%> 	c) Tabuleiro C ")
		(format t "~%> 	d) Tabuleiro D ")
		(format t "~%> 	e) Tabuleiro E ")
		(format t "~%> 	f) Tabuleiro F ")
		(format t "~%> 	g) Tabuleiro G (por inserir)")
		(format t "~%> Estado inicial: ")
		(format t "~%> ")

			(let* ((opcao (read))
			   (opcao-valida (opcao-existe opcao '(a b c d e f))))
					(with-open-file (ficheiro (concatenate 'string (diretoria-atual)"problemas.dat") :direction :input :if-does-not-exist :error)
						(cond
							((not opcao-valida) (progn
													(format t "~%> Opcao Invalida!")
													(format t "~%  ")
													(terpri)
													(ler-tabuleiro)))
							((equal opcao 'a) (progn (format t "~%> Tabuleiro a") (nth 0 (read ficheiro))))
							((equal opcao 'b) (nth 1 (read ficheiro)))
							((equal opcao 'c) (nth 2 (read ficheiro)))
							((equal opcao 'd) (nth 3 (read ficheiro)))
							((equal opcao 'e) (nth 4 (read ficheiro)))
							((equal opcao 'f) (nth 5 (read ficheiro)))
							;((equal opcao 'g) (nth 6 (read ficheiro)))	; se for adicionado ao nosso ficheiro é o problema 6, se for adicionado num ficheiro novo é o problema 1
						)
					)
		)
	)

)


(defun opcao-existe (elemento lista)

	(cond
		((null lista) nil)
		((eql elemento (car lista)) T)
		(T (opcao-existe elemento (cdr lista)))
	)

)


	(defun resultado-simulacao(resultado)

	(with-open-file (ficheiro (concatenate 'string (diretoria-atual)"estatisticas.dat")
							:direction :output
							:if-exists :append
							:if-does-not-exist :create)

		;; Esta parte será escrita no ficheiro do tipo .DAT
		(imprimir-resultado ficheiro resultado)
		;(format ficheiro "~%> resultado ~a" resultado)

		;(format ficheiro "Profundidade da Solução: ~s ~%" (second (car abertos)))
		(format ficheiro "___________________________________________________~%")

		)
	

	)



(defun obter-objectivo(tabuleiro) "Le do utilizador o número objectivo de caixas a fechar"
	(progn
		(format t "~%> Qual o objectivo ?")
		(format t "~%> ")
		(let ((resposta (read)))
			(cond
				((not (numberp resposta)) (progn (format t "~%> Insira um objectivo valido")(format t "~%> ")(obter-objectivo)))
				((and (>= resposta 1) (<= resposta (* (numero-caixas-horizontal tabuleiro) (numero-caixas-vertical tabuleiro)))) resposta)
				(T (obter-objectivo tabuleiro))))
	)
)



(defun escolher-algoritmo()

	(progn
		(format t "~%> Qual o algoritmo que pretende usar?")
		(format t "~%> 	bfs) Breadth-first Search")
		(format t "~%> 	dfs) Depth-first Search")
		(format t "~%> 	a-asterisco) A* Search")
		(format t "~%> 	ida-asterisco) IDA* Search")
		(format t "~%>  ESCOLHA: ")

		(let* ((resposta (read))
				 (opcao-valida (opcao-existe resposta '(bfs dfs a-asterisco ida-asterisco))))
			(cond
				(opcao-valida resposta)
				(T (progn
						(format t "~%> Opcao Invalida!")
						(format t "~%  ")
						(terpri)
						(escolher-algoritmo)
					)
				)
			)
		)
	)

)


(defun obter-profundidade()

	(progn
		(format t "~%> Qual a profundidade que pretende ?")
		(format t "~%> ESCOLHA ")
		(let ((resposta (read)))
			(cond
				((or (not (numberp resposta)) (or (> resposta 9000) (<= resposta 0)))
					(progn
						(format t "~%> Opcao Invalida! Valores compreendidos entre [0,9000]")
						(format t "~%  ")
						(terpri)
						(obter-profundidade)
					))
				(T resposta)
			)
		)
	)


)