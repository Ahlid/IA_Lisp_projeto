

(defun iniciar ()	
"Função que inicializa o programa, chamando a função que apresenta o menu inicial."
	(progn
		(compile-file (concatenate 'string (diretoria-atual)"procura.lisp"))  ;compila o ficheiro procura.lisp
		(compile-file (concatenate 'string (diretoria-atual)"puzzle.lisp"))	;compila o ficheiro puzzle.lisp
		(load (concatenate 'string (diretoria-atual)"procura.ofasl"))  ;faz load do ficheiro compilado da procura.lisp
		(load (concatenate 'string (diretoria-atual)"puzzle.ofasl")) ;faz load do ficheiro compilado do puzzle.lisp
		(menu-principal)
	)
)

(defun diretoria-atual () 
	"Função que define um caminho para leitura dos ficheiros."
	(let (

			(path-ricardo "C:/Users/Ricardo Morais/Documents/IA_Lisp_projeto/Projeto/")
			;(path-tiago  "C:\\Users\\pcts\\Desktop\\ProjIA\\Projeto\\"))
			;(path-professor (pedir-directoria))
		)
			
		;path-tiago
		path-ricardo
		;path-professor
	)
)
(let ((diretoria nil))
(defun pedir-directoria ()
	"Pede a directoria dos ficheiros ao utilizador"
	(cond
	((null diretoria) (progn
		(format t "Insira o diretório: ")
		(setf diretoria (read))
	))
	(t diretoria)
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
                                                    (1 (progn (menu-jogar) t)) ;vai para o menu jogar
                                                    (2 (progn (regras) t)) ;ve as regras
                                                    (3 (progn (imprime-tabuleiro) t)) ;imprime um tabuleiro
                                                    (4 (progn (format t "PROGRAMA TERMINADO") nil))) ;acaba o programa
                )
                ( T (progn  (format t "~%ESCOLHA INVALIDA~%~%Escolha: ")
                            (setf escolha (read))
                            )))
               
               )) (return)))
    )
  )

(defun menu-jogar()
	"função responsavel por fazer uma simulação onde se escolhe o tabuleiro, objetivo, algoritmo e se necessario profundidade e heuristica"
	(let*
		(
			(tabuleiro (escolher-tabuleiro)) ;obtem o tabuleiro
			(objetivo (obter-objectivo tabuleiro)) ;obtem o objetivos
			(algoritmo (escolher-algoritmo)) ;obtem o algoritmo
			(profundidade (cond ((eql algoritmo 'dfs) (obter-profundidade)) (T 9999))) ;se o algoritmo for dfs pede profundidade
			(heuristica (cond ((not (or (eql algoritmo 'dfs) (eql algoritmo 'bfs))) (escolher-heuristica)) (T nil))) ; se necessitar heuristica pede
			
		)
		(cond
			((eql algoritmo 'dfs) (resultado-simulacao (teste-dfs objetivo profundidade tabuleiro))) ;se dfs
			((eql algoritmo 'bfs) (resultado-simulacao (teste-bfs objetivo tabuleiro))) ;se bfs
			((eql algoritmo 'a-asterisco) ; se a*
				(cond
					((eql heuristica 'heuristica)(resultado-simulacao (teste-a-asterisco objetivo tabuleiro))); se heuristica 1
					(t (resultado-simulacao (teste-a-asterisco-h2 objetivo tabuleiro))) ; se heuristica 2
				)
			)
			((eql algoritmo 'ida-asterisco) ; se ida*
				(cond
					((eql heuristica 'heuristica)(resultado-simulacao (teste-ida-asterisco objetivo tabuleiro))) ; se heuristica 1
					(t (resultado-simulacao (teste-ida-asterisco-h2 objetivo tabuleiro))) ; se heuristica 2
				)
			)
			(T nil)
		)
	)
	
)

;;;Regras do puzzle
(defun regras () 
"função que devolve as regras"
   (format t "
   -------------------------- Regras do Puzzle dos Pontos e das Caixas -------------------
  |                                                                                      |
  |     O objetivo do puzzle é fechar um determinado número de caixas a partir de        |
  |     uma configuração inicial do tabuleiro. Para atingir este objetivo, é possível    |
  |     desenhar um arco entre dois pontos adjacentes, na horizontal ou na vertical.     |
  |     Quando o número de caixas por fechar é atingido, o puzzle está resolvido.        |
  |     A resolução do puzzle consiste portanto em executar a sucessão de traços que     |
  |     permite chegar a um estado onde o número de caixas por fechar é alcançado.       |                                                                             |
  |                                                                                      |
  ----------------------------------------------------------------------------------------
  "
  )
  
)

(defun imprime-tabuleiro ()
	"Imprime um tabuleiro escolhido pelo utilizador"
	(desenhar-tabuleiro (escolher-tabuleiro) *standard-output*)
)

(defun criar-linha-horizontal (lista)
	"Imprime uma linha de valores booleanos como um linha de traços horizontais"
	(cond 
		((null lista) "o")
		(t 	(concatenate 'string 
							(cond 
								((first lista) "o---") ; se o elemento é t devolve a linha						
								(t "o   ") ; senão devolve espaço em branco		
							) 
							(criar-linha-horizontal (rest lista)) ; chama recusivamente a função com o rest da lista
			)
		)
	)
)

(defun criar-linha-vertical (lista)
	"Imprime uma linha de valores booleanos como um linha de traços verticais"
	(cond 
		( (null lista) "" ) ; se não houver elementos na lista devolve uma string vazia
		( t 
			(concatenate 'string 	
							(cond 
								((and (first lista) (> (length lista) 1)) "|   ") ; se o elemento é t  e não é o ultimo elemento da lista	devolve a linha com espaços
								((and (first lista) (<= (length lista) 1)) "|") ; se o elemento é t  e é o ultimo elemento da lista devolve a linha
								(t "    ") ; senão imprime espaços em branco
							) 
							(criar-linha-vertical (rest lista)) ; chama recusivamente a função com o rest da lista
			) 
		)
	)
)

(defun desenhar-tabuleiro-aux (matriz1 matriz2 stream)
	"Ajuda a desenhar o tabuleiro recebendo duas listas"
	(cond 
		((and (null matriz1) (null matriz2)) nil) ; quando as duas listas estiverem vazias retorna nil
		(t
			(progn
				(cond 
					((> (length (first matriz1)) 0) ; se já não existir elementos da matriz1 não escreve mais linhas horizontais
						(write-line (criar-linha-horizontal (first matriz1)) stream) ; se existe uma linha horizontal escreve-a no stream
					)
				)
				(cond 
					((> (length (first matriz2)) 0) ; se já não existir elementos da matriz2 não escreve mais linhas verticais
						(write-line (criar-linha-vertical (first matriz2)) stream) ; se existe uma linha vertical escreve-a no stream
					)
				)
				(desenhar-tabuleiro-aux (rest matriz1) (rest matriz2) stream) ; faz a chamada recursiva com o rest das listas
			)
		)
	)
)	


(defun desenhar-tabuleiro (tabuleiro stream)
	"Desenha o tabuleiro"
	(desenhar-tabuleiro-aux ; Desenha o tabuleiro no stream
		(get-arcos-horizontais tabuleiro) ; Matriz dos arcos horizontais
		(matriz2d-transposta (get-arcos-verticais tabuleiro)) ; Transpõe a matriz dos arcos verticais de forma a termos uma lista de linhas linhas
		stream ; stream de escrita
	)
)



(defun imprimir-resultado (stream resultado)
"função que imprime o resultado de uma simulação"
	(progn 
		(write-line "Resultado:" stream)
		
		(write-line (format nil "Resolução:"  stream))
		(imprime-pai (first resultado) stream)
		(write-line (format nil "Tempo de resolução: ~a" (second resultado)) stream)
		(write-line (format nil "Número de nós gerado: ~a" (third resultado)) stream)
		(write-line (format nil "Número de nós expandidos: ~a" (fourth resultado)) stream)
		(write-line (format nil "Profundidade: ~a" (fifth resultado)) stream)
		(write-line (format nil "Penetrancia: ~,4f" (float (sixth resultado))) stream)
		(write-line (format nil "Fator de ramificação: ~,4f" (float (seventh resultado))) stream)
	)
)


(defun escolher-tabuleiro() 
"função que pede ao jogador o tabuleiro a usar"
	(progn
		(format t "~%>")
		(format t "~%> Escolha tabuleiro ")
		(format t "~%> 	a) Tabuleiro A ")
		(format t "~%> 	b) Tabuleiro B ")
		(format t "~%> 	c) Tabuleiro C ")
		(format t "~%> 	d) Tabuleiro D ")
		(format t "~%> 	e) Tabuleiro E ")
		(format t "~%> 	f) Tabuleiro F ")
		(format t "~%> 	g) Tabuleiro G (por inserir)")
		(format t "~%> Tabuleiro: ")
		(format t "~%> ")

		(let* 	
			(
				(opcao (read))
				(opcao-valida (opcao-existe opcao '(a b c d e f g)))
			)
			(with-open-file (ficheiro (concatenate 'string (diretoria-atual)"problemas.dat") :direction :input :if-does-not-exist :error)
				 (cond
					((not opcao-valida) (progn
											(format t "~%> Opcao Invalida!")
											(format t "~%  ")
											(terpri)
											(escolher-tabuleiro)))

					((equal opcao 'a) (converter-estado-novo-para-antigo (nth 0 (read ficheiro))))
					((equal opcao 'b) (converter-estado-novo-para-antigo (nth 1 (read ficheiro))))
					((equal opcao 'c) (converter-estado-novo-para-antigo (nth 2 (read ficheiro))))
					((equal opcao 'd) (converter-estado-novo-para-antigo (nth 3 (read ficheiro))))
					((equal opcao 'e) (converter-estado-novo-para-antigo (nth 4 (read ficheiro))))
					((equal opcao 'f) (converter-estado-novo-para-antigo (nth 5 (read ficheiro))))
					((equal opcao 'g) (converter-estado-novo-para-antigo (nth 6 (read ficheiro))))	; se for adicionado ao nosso ficheiro é o problema 6, se for adicionado num ficheiro novo é o problema 1
				)
			)
		)	
	)
)


(defun opcao-existe (elemento lista)
	"função que verifica se a opção que o utilizador inserio está dentro das hipoteces"
	(cond
		((null lista) nil)
		((eql elemento (car lista)) T)
		(T (opcao-existe elemento (cdr lista)))
	)
)


(defun resultado-simulacao(resultado)
	"Função que mostra e grava o resultado da simulação"
	(progn
		(imprimir-resultado *standard-output* resultado)
		(with-open-file (ficheiro (concatenate 'string (diretoria-atual)"estatisticas.dat")
								:direction :output
								:if-exists :append
								:if-does-not-exist :create)

			;; Esta parte será escrita no ficheiro do tipo .DAT
			(imprimir-resultado ficheiro resultado)
			(format ficheiro "___________________________________________________~%")

		)
	)
	
)




(defun obter-objectivo(tabuleiro) 
	"Le do utilizador o número objectivo de caixas a fechar"
	(progn
		(format t "~%> Qual o objectivo ?")
		(format t "~%> ")
		(let ((resposta (read)))
			(cond
				((not (numberp resposta)) (progn (format t "~%> Insira um objectivo valido")(format t "~%> ")(obter-objectivo tabuleiro)))
				((and (>= resposta 1) (<= resposta (* (numero-caixas-horizontal tabuleiro) (numero-caixas-vertical tabuleiro)))) resposta)
				(T (obter-objectivo tabuleiro))))
	)
)



(defun escolher-algoritmo()
	"função que pede o algoritmo a usar ao utilizador"
	(progn
		(format t "~%> Qual o algoritmo que pretende usar?")
		(format t "~%> 	a) Breadth-first Search")
		(format t "~%> 	b) Depth-first Search")
		(format t "~%> 	c) A* Search")
		(format t "~%> 	d) IDA* Search")
		(format t "~%>  ESCOLHA: ")

		(let* ((opcao (read)))
			(cond
					((equal opcao 'a) 'bfs)
					((equal opcao 'b) 'dfs)
					((equal opcao 'c) 'a-asterisco)
					((equal opcao 'd) 'ida-asterisco)
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
	"função que pede a profundidade ao utilizador"
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


(defun escolher-heuristica () 
	"Recebe do utilizador a decisão de qual heurística usar"
	(progn
		(format t "~%> Qual a heuristica que pretende aplicar?")
		(format t "~%> 	1) Proposta pelos professores")
		(format t "~%> 	2) Proposta pelos alunos")
		(format t "~%> Heuristica a usar: ")

		(let* ((resposta (read)))
			(cond
				((or (not (numberp resposta)) (or (> resposta 2) (< resposta 1)))
					(progn
						(format t "~%> Opcao Invalida!")
						(format t "~%  ")
						(terpri)
						(escolher-heuristica)
					))
				(T (cond
						((= resposta 1) 'heuristica)
						((= resposta 2) 'heuristica-2)
					)
				)
			)
		)
	)
) 


(defun imprime-pai(no stream) 
	"Função que imprime recursivamente todas as etapas do tabuleiro desde a raiz até ao estado atual"

	(cond
		((null no) nil)
		(T	
			(progn
			(imprime-pai (no-pai no) stream)
			(desenhar-tabuleiro (no-estado no) stream)
			
			(format stream "___________________________________________________~%")
			)
		)
	)
)
