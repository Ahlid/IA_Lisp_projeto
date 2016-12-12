
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDA* procura-generica
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tutorial: https://algorithmsinsight.wordpress.com/graph-theory-2/ida-star-algorithm-in-general/
; wikipedia: https://en.wikipedia.org/wiki/Iterative_deepening_A*





(defun procura-generica-ida-asterisco-aux 	(	no-inicial ; nó inicial
												f-solucao ; função que verifica se um nó é uma solucao
												f-sucessores ; função que gera os sucessores
												f-algoritmo ; algoritmo
												lista-operadores ; lista dos operadores
												limite ; limite de custo f da procura
												heuristica ; heuristica
												&optional 
													(abertos (list no-inicial)) ; lista de abertos
													(fechados nil) ; lista de fechados
													(nos-gerados 0) ; numero de nos gerados
													(nos-expandidos 0) ; numero de nos expandidos
													(margem-bisecao 0.5) ; margem de erro utilizada no metodo de bisecao
											)
  "Permite procurar a solucao de um problema usando a procura no espaço de estados. A partir de um estado inicial,
 de uma funcao que gera os sucessores e de um dado algoritmo. De acordo com o algoritmo pode ser usada um limite
 de profundidade, uma heuristica e um algoritmo de ordenacao"
 	(cond
		; nao existe solucao ao problema
		((null abertos) nil)
		; se o no ja existe nos fechados é ignorado
		((existep (first abertos) fechados f-algoritmo) (procura-generica-ida-asterisco-aux 	no-inicial ; nó ínicial
																			f-solucao ; função que verifica se um nó é uma solucao
																			f-sucessores ; função que gera os sucessores
																			f-algoritmo ; algoritmo
																			lista-operadores ; lista dos operadores
																			limite ; limite de custo f da procura
																			heuristica ; heuristica
																			(cdr abertos) ; resto da lista de abertos
																			fechados ; lista de fechados
																			nos-gerados ; número de nós gerados
																			nos-expandidos ; número de nós expandidos
																			margem-bisecao ; margem de erro utilizada no metodo de bisecao
														)
		)	
		; se a custo f do primeiro no de abertos é superior ao limite, devolve esse custo f
		( (> (no-controlo-f  (first abertos)) limite) 
								(let
									(
										(resultado (procura-generica-ida-asterisco-aux 	no-inicial ; nó ínicial
																			f-solucao ; função que verifica se um nó é uma solucao
																			f-sucessores ; função que gera os sucessores
																			f-algoritmo ; algoritmo
																			lista-operadores ; lista dos operadores
																			limite ; limite de custo f da procura
																			heuristica ; heuristica
																			(cdr abertos) ; resto da lista de abertos
																			fechados ; lista de fechados
																			nos-gerados ; número de nós gerados
																			nos-expandidos ; número de nós expandidos
																			margem-bisecao ; margem de erro utilizada no metodo de bisecao
														)
										)
									)
									(cond 
										( (null resultado) (no-controlo-f  (first abertos)) )
										( (numberp resultado) (min (no-controlo-f (first abertos)) resultado) )
										( (listp resultado) resultado )
									)
								)
		)
									
									
		; se o primeiro dos abertos e solucao este no e devolvido
		( (funcall f-solucao (first abertos))   (list 	(car abertos) ; primeiro nó de abertos
														nos-gerados ; número de nós gerados
														nos-expandidos ; número de nós expandidos 
														(no-profundidade (car abertos)) ; função heuristica
														(/ (no-profundidade (car abertos)) nos-gerados) ; penetrância
														(bisecao (no-profundidade (car abertos)) nos-gerados margem-bisecao) ; fator de ramificacao
												)
		)
		(T (let*
				(
					;lista dos sucessores do primeiro dos abertos
					(lista-sucessores 	(funcall f-sucessores ; gerar os sucessores
													(first abertos) ; primeiro nó de abertos 
													lista-operadores ; lista de operadores
													f-algoritmo ; algoritmo
													nil ; profundidade máxima
													heuristica ; função heuristica
										)
					)
					(solucao (existe-solucao lista-sucessores f-solucao f-algoritmo));verifica se existe uma solucao nos sucessores para o dfs
				)
				(cond
					; devolve a solucao
					(solucao 	(list 	solucao ; nó solução
										nos-gerados ; número de nós gerados
										nos-expandidos ; número de nós expandidos
										(no-profundidade solucao) ; profundidade do nó solução
										(/ (no-profundidade solucao) nos-gerados) ; penetrância
										(bisecao (no-profundidade solucao) nos-gerados margem-bisecao) ; fator de ramificacao
								)
					)
					; expande a arvore se o primeiro dos abertos nao for solucao
					(T (let
							(
								(resultado (procura-generica-ida-asterisco-aux 	no-inicial ; nó ínicial
																f-solucao ; função que verifica se um nó é uma solucao
																f-sucessores ; função que gera os sucessores
																f-algoritmo ; algoritmo
																lista-operadores ; lista dos operadores
																limite ; limite de custo f da procura
																heuristica ; heuristica
																(funcall f-algoritmo (rest abertos) lista-sucessores) ; utiliza o algoritmo para juntar o resto da lista de abertos e a lista de sucessores para a próxima lista de abertos
																(cons (car abertos) fechados) ; adiciona o primeiro nó de abertos aos fechados e envia para a proxima lista de fechados
																(+ nos-gerados (length lista-sucessores)) ; incrementa os número de nós gerados com o tamanho da lista de sucessores
																(1+ nos-expandidos) ; incrementa o número de nós expandidos
																margem-bisecao
											)
								)			
							)
							resultado
						)
					)
				)
			)
		)
   	)
)


(defun procura-generica-ida-asterisco 	(	no-inicial ; nó inicial	
											f-solucao ; função que verifica se um nó é uma solucao
											f-sucessores ; função que gera os sucessores
											f-algoritmo ; algoritmo
											lista-operadores ; lista dos operadores
											heuristica
											&optional
												(limite 0)
												(tempo-inicial (get-universal-time)) ; timestamp em que foi iniciada a procura		
												(margem-bisecao 0.5) ; margem de erro do fator de ramificacao
											)
	(let
		(
			(resultado 	(procura-generica-ida-asterisco-aux 	no-inicial ; nó inicial
																f-solucao ; função que verifica se um nó é uma solucao
																f-sucessores ; função que gera os sucessores
																f-algoritmo ; algoritmo
																lista-operadores ; lista dos operadores
																limite ; limite de custo f da procura
																heuristica ; heuristica
						)
			)
		)
		(cond ;;TODO: as estatisticas têm que acompanhar i algoritmo
			( (numberp resultado) 	(procura-generica-ida-asterisco 	no-inicial
																		f-solucao
																		f-sucessores
																		f-algoritmo
																		lista-operadores
																		heuristica
																		resultado
									)
			)
			( (listp resultado) resultado ) 
		)
	)					
)

(defun ida-asterisco (abertos sucessores)
	""
	(append sucessores abertos)
)

(defun teste-ida-asterisco (n m o)
	(procura-generica-ida-asterisco (no-criar (criar-tabuleiro-vazio n m) nil 0 '(0 0 0)) (criar-solucao o) 'sucessores 'ida-asterisco (criar-operacoes n m) (heuristica o))
)