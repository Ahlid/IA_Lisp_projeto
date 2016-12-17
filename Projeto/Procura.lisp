
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Genéricos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun existep (no lista f-algoritmo)
 	"Definir o predicado existep que permite verificar se um nó existe numa lista .
O predicado recebe três parâmetros; um nó, uma lista de nós e o nome do algoritmo.
Retorna verdadeiro se o nó existir na lista. Deve ter em atenção que para o algoritmo dfs,
o conceito de nó repetido é particular-
No algoritmo dfs um nó só é considerado igual se a sua profundidade for inferior às profundidades existentes na lista"
  (cond
    ( (null lista) nil ) ;  a lista está vazia
    ( t
      (let*
		  (
			(is-dfs (eql f-algoritmo 'dfs)) ; se é o algoritmo depth first
			(proximo-no-lista (first lista)) ; buscar o próximo da lista
			(estados-iguais (equal (no-estado no) (no-estado proximo-no-lista))) ; verifica se o estado do proximo no da lista é igual ao estado do no que estamos a avaliar
			(profundidade-superior (> (no-profundidade no) (no-profundidade proximo-no-lista)) ) ; se a profundidade do nó avalidado é maior que a do proximo no da lista
			(dfs-exitep (and is-dfs estados-iguais profundidade-superior)) ; condição para o depth first => estados iguais e profundidade do nó é superior
			(else-existep (and (not is-dfs) estados-iguais)) ; condição para os restantes algoritmos => estados iguais
		  )

        (cond
          ( dfs-exitep t ) ; se existir o nó igual e com profundidade superior é considerado que o no a avaliar já existe na lista
          ( else-existep t ) ; se simplesmente existe na lista
          ( t (existep no (rest lista) f-algoritmo) ) ; volta a fazer com o resto da lista
        )
      )
    )
  )
)

(defun existe-solucao (lista f-solucao f-algoritmo)
	"Verifica se existe uma solucao ao problema numa lista de sucessores para o algoritmo dfs"
	(cond
		((not (eql f-algoritmo 'dfs)) nil) ; se não for o algoritmo dfs devolve nil pois não vamos avaliar os sucessores nesta altura
		((null lista) nil) ; se a lista estiver vazia devolve nil
		((funcall f-solucao (car lista)) (car lista)) ; verifica se o primeiro elemento da lista é solução e se for retorna esse elemento
		(T (existe-solucao (cdr lista) f-solucao f-algoritmo)) ; volta a chamar a função com o resto da lista
	)
)

(defun calcular-numero-nos-gerados (fator-ramificacao profundidade)
	"Calcula o número de nos gerados a partir do fator de ramificação e a profundidade"
	(cond
		( (<= profundidade 1) fator-ramificacao) ; se a profundidade é menor ou igual a devolvemos o fator ramificacao elevado a 1
		( t (+ (expt fator-ramificacao profundidade) (calcular-numero-nos-gerados fator-ramificacao (1- profundidade))) ) ; faz a soma recursiva do polinómio
	)
)

(defun bisecao (profundidade numero-nos-gerados margem &optional (minimo 0) (maximo numero-nos-gerados))
	(let*
		(
			(media (/ (+ minimo maximo) 2)) ; faz a média entre o mínimo e o máximo
			(numero-nos-gerado-calculado (calcular-numero-nos-gerados media profundidade)) ; calcula o numero de nos gerados utilizando a média como fator de ramificacao
			(diferenca (abs (- numero-nos-gerados numero-nos-gerado-calculado))) ; calcula a diferenca entre o numero de nos gerados e o numero de nos gerados calculado através da média
			(p (< numero-nos-gerado-calculado numero-nos-gerados)) ; verifica se o numero de nos gerado através da média é inferior ao numero de nos gerados
		)
		(cond
			( (< diferenca margem) media ) ; se a diferença for inferior à margem de erro, devolve a média atual
			( p (bisecao profundidade numero-nos-gerados margem media maximo) ) ; se a media inferior ao fator de ramificacao utilizamos a média como limite inferior
			( t (bisecao profundidade numero-nos-gerados margem minimo media) ) ; se a media superior ao fator de ramificacao utilizamos a média como limite superior
		)
	)
)

(defun procura-generica (no-inicial ; nó inicial
						 f-solucao ; função que verifica se um nó é uma solucao
						 f-sucessores ; função que gera os sucessores
						 f-algoritmo ; algoritmo
						 lista-operadores ; lista dos operadores
						&optional
							(prof-max  nil) ; profundidade máxima
							(heuristica nil) ; heuristica
							(abertos (list no-inicial)) ; lista de abertos
							(fechados nil) ; lista de fechados
							(tempo-inicial (get-universal-time)) ; timestamp de inicio da procura
							(nos-gerados 0) ; numero de nos gerados
							(nos-expandidos 0) ; numero de nos expandidos
							(margem-bisecao 0.5)
						)
  "Permite procurar a solucao de um problema usando a procura no espaço de estados. A partir de um estado inicial,
 de uma funcao que gera os sucessores e de um dado algoritmo. De acordo com o algoritmo pode ser usada um limite
 de profundidade, uma heuristica e um algoritmo de ordenacao"
 	(cond
		; nao existe solucao ao problema
		((null abertos) nil)
		; se o primeiro dos abertos e solucao este no e devolvido
		((funcall f-solucao (car abertos))  (list 	(car abertos) ; primeiro nó de abertos
													(- (get-universal-time) tempo-inicial) ; tempo em segundos que a procura levou a encontrar a solução
													nos-gerados ; número de nós gerados
													nos-expandidos ; número de nós expandidos
													(no-profundidade (car abertos)) ; função heuristica
													(cond ((= nos-gerados 0) 1) (t (/ (no-profundidade (car abertos)) nos-gerados))) ; penetrância
													(bisecao (no-profundidade (car abertos)) nos-gerados margem-bisecao) ; fator de ramificacao
											)
		)
		; se o no ja existe nos fechados é ignorado
		((existep (first abertos) fechados f-algoritmo) (procura-generica 	no-inicial ; nó ínicial
																			f-solucao ; função que verifica se um nó é uma solucao
																			f-sucessores ; função que gera os sucessores
																			f-algoritmo ; algoritmo
																			lista-operadores ; lista dos operadores
																			prof-max ; profundidade máxima
																			heuristica ; heuristica
																			(cdr abertos) ; resto da lista de abertos
																			fechados ; lista de fechados
																			tempo-inicial ; timestamp em que foi iniciada a procura
																			nos-gerados ; número de nós gerados
																			nos-expandidos ; número de nós expandidos
																			margem-bisecao ; margem de erro utilizada no metodo de bisecao
														)
		)
		(T (let*
				(
					;lista dos sucessores do primeiro dos abertos
					(lista-sucessores 	(funcall f-sucessores ; gerar os sucessores
													(first abertos) ; primeiro nó de abertos
													lista-operadores ; lista de operadores
													f-algoritmo ; algoritmo
													prof-max ; profundidade máxima
													heuristica ; função heuristica
										)
					)
					(novo-nos-gerados (+ nos-gerados (length lista-sucessores)))
					(solucao (existe-solucao lista-sucessores f-solucao f-algoritmo));verifica se existe uma solucao nos sucessores para o dfs
				)
				(cond
					; devolve a solucao
					(solucao 	(list 	solucao ; nó solução
										(- (get-universal-time) tempo-inicial) ; tempo em segundos que a procura levou a encontrar a solução
										novo-nos-gerados ; número de nós gerados
										(1+ nos-expandidos) ; número de nós expandidos
										(no-profundidade solucao) ; profundidade do nó solução
										(cond ((= nos-gerados 0) 1) (t (/ (no-profundidade (car abertos)) nos-gerados)))  ; penetrância
										(bisecao (no-profundidade solucao) novo-nos-gerados margem-bisecao) ; fator de ramificacao
								)
					)
					; expande a arvore se o primeiro dos abertos nao for solucao
					(T (procura-generica 	no-inicial ; nó ínicial
											f-solucao ; função que verifica se um nó é uma solucao
											f-sucessores ; função que gera os sucessores
											f-algoritmo ; algoritmo
											lista-operadores ; lista dos operadores
											prof-max ; profundidade máxima
											heuristica ; heuristica
											(funcall f-algoritmo (rest abertos) lista-sucessores) ; utiliza o algoritmo para juntar o resto da lista de abertos e a lista de sucessores para a próxima lista de abertos
											(cons (car abertos) fechados) ; adiciona o primeiro nó de abertos aos fechados e envia para a proxima lista de fechados
											tempo-inicial ; timestamp em que foi iniciada a procura
											novo-nos-gerados ; incrementa os número de nós gerados com o tamanho da lista de sucessores
											(1+ nos-expandidos) ; incrementa o número de nós expandidos
											margem-bisecao)
					)
				)
			)
		)
   	)
)

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
													(nos-gerados 0) ; numero de nos gerados
													(nos-expandidos 0) ; numero de nos expandidos
													(abertos (list no-inicial)) ; lista de abertos
													(fechados nil) ; lista de fechados
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
																								nos-gerados ; número de nós gerados
																								nos-expandidos ; número de nós expandidos
																								(cdr abertos) ; resto da lista de abertos
																								fechados ; lista de fechados
																								margem-bisecao ; margem de erro utilizada no metodo de bisecao
														)
		)	
		; se a custo f do primeiro no de abertos é superior ao limite, utilizamos como comparação do resultado da proxima expansão da arvore
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
																						nos-gerados ; número de nós gerados
																						nos-expandidos ; número de nós expandidos
																						(cdr abertos) ; resto da lista de abertos
																						fechados ; lista de fechados
																						margem-bisecao ; margem de erro utilizada no metodo de bisecao
													)
										)
									)
									(cond 
										( (null resultado) 	(list 	(no-controlo-f  (first abertos)) 
																	nos-gerados ; número de nós gerados
																	nos-expandidos ; número de nós expandidos
															)
										)
										( (numberp (first resultado)) (cons (min (no-controlo-f (first abertos)) (first resultado)) (rest resultado)) )
										( (listp resultado) resultado )
									)
								)
		)
									
									
		; se o primeiro dos abertos e solucao este no e devolvido
		( (funcall f-solucao (first abertos))   (list 	(car abertos) ; primeiro nó de abertos
														nos-gerados ; número de nós gerados
														nos-expandidos ; número de nós expandidos 
														(no-profundidade (car abertos)) ; função heuristica
														(cond ((= nos-gerados 0) 1) (t (/ (no-profundidade (car abertos)) nos-gerados))) ; penetrância
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
					(novo-nos-gerados (+ nos-gerados (length lista-sucessores)))
				)
				(cond
					; devolve a solucao
					(solucao 	(list 	solucao ; nó solução
										novo-nos-gerados ; número de nós gerados
										nos-expandidos ; número de nós expandidos
										(no-profundidade solucao) ; profundidade do nó solução
										(cond ((= novo-nos-gerados 0) 1) (t (/ (no-profundidade (car abertos)) novo-nos-gerados))) ; penetrância
										(bisecao (no-profundidade solucao) novo-nos-gerados margem-bisecao) ; fator de ramificacao
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
																novo-nos-gerados ; incrementa os número de nós gerados com o tamanho da lista de sucessores
																(1+ nos-expandidos) ; incrementa o número de nós expandidos
																(funcall f-algoritmo (rest abertos) lista-sucessores) ; utiliza o algoritmo para juntar o resto da lista de abertos e a lista de sucessores para a próxima lista de abertos
																(cons (car abertos) fechados) ; adiciona o primeiro nó de abertos aos fechados e envia para a proxima lista de fechados
																margem-bisecao ; Margem de erro da bisecao
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
												(numero-nos-gerados 0)
												(numero-nos-expandidos 0)
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
																numero-nos-gerados ; numero de nos gerados
																numero-nos-expandidos ; numero de nos expandidos
						)
			)
		)
		(cond ;;TODO: as estatisticas têm que acompanhar i algoritmo
			( (numberp (first resultado)) 	(procura-generica-ida-asterisco 	no-inicial ; nó inicial
																				f-solucao ; função que verifica se um nó é uma solucao
																				f-sucessores ; função que gera os sucessores
																				f-algoritmo ; algoritmo
																				lista-operadores ; lista dos operadores
																				heuristica ; heuristica
																				(first resultado) ; no
																				tempo-inicial ; tempo inicial
																				margem-bisecao ; margem da bisecao
																				(second resultado) ; numero de nos gerados
																				(third resultado) ; numero de nos expandidos
											)
			)
			( (listp resultado) (append (list (first resultado) (- (get-universal-time) tempo-inicial)) (rest resultado)) ) ; junta o tempo ao resultado
		)
	)					
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Funções auxiliares
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun substituir (i valor l)
	"Substitui um elemento de uma lista correpondente ao índice i pelo valor"
	(cond
		( (null l) nil ) ; se a lista está vazia devolve nil
		( (= i 0) (cons valor (rest l)) ) ;se o indice é igual 0, mete o valor atrás da lista
		( t (cons (first l) (substituir (1- i) valor (rest l))) ) ; chama a função com o rest da lista e com o indice decrementado e nao receber o valor junta no ínicio o primeiro elemento da lista
	)
)

(defun elemento-por-indice (i l)
	"Devolve o elemento de uma lista correspondente ao índice i"
	(cond
		( (null l) nil ) ; devolve nil se a lista l está vazia
		( (= i 0) (first l) ) ; se i igual a 0 devolve o primeiro elemento da lista
		( t (elemento-por-indice (1- i) (rest l)) ) ; chama a função com o resta da lista e com o indice decrementado
	)
)

(defun matriz2d-transposta (m)
	"Faz a transposta da matriz m"
	(apply  #'mapcar (cons #'list m)) ; transpões a matriz
)

(defun limpar-nils (lista)
	"Remove os nils de uma lista"
	(apply #'append ; remove todos os nils fazendo o append das listas com os elementos
		(mapcar (lambda (e)
						(cond
							((null e) nil) ; caso o elemento seja nil, devolve nil
							(t (list e)) ; caso contrario cria uma lista com o elemento
						)
				) lista
		)
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manipulação de nós
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun no-criar (estado &optional (pai nil) (profundidade 0) (controlo nil))
  "Cria um nó"
  (list estado pai profundidade controlo)
)

(defun no-estado (no)
	"Devolve o estado do nó"
	(elemento-por-indice 0 no)
)

(defun no-pai (no)
	"Devolve o pai do nó"
	(elemento-por-indice 1 no)
)

(defun no-profundidade (no)
	"Devolve a profundidade do nó"
	(elemento-por-indice 2 no)
)

(defun no-controlo (no)
	(elemento-por-indice 3 no)
)

(defun set-no-estado (no estado)
 "Altera o estado de um nó"
  (substituir 0 estado no)
)

(defun set-no-pai (no pai)
 "Altera o pai do nó"
  (substituir 1 pai no)
)

(defun set-no-profundidade (no profundidade)
 "Altera o pai do nó"
  (substituir 2 profundidade no)
)

(defun set-no-controlo (no controlo)
 "Altera o elemeto de controlo do nó"
  (substituir 3 controlo no)
)

(defun no-controlo-g (no)
  "Devolve o g do nó"
  (elemento-por-indice 0 (no-controlo no))
)

(defun no-controlo-h (no)
  "Devolve o h do nó"
  (elemento-por-indice 1 (no-controlo no))
)

(defun no-controlo-f (no)
  "Devolve o f do nó"
  (elemento-por-indice 2 (no-controlo no))
)

(defun set-no-g (no g)
	"Altera o valor g do nó"
	(set-no-controlo no (substituir 0 g (no-controlo no)))
)

(defun set-no-h (no h)
	"Altera o valor h do nó"
	(set-no-controlo no (substituir 1 h (no-controlo no)))
)

(defun set-no-f (no f)
	"Altera o valor f do nó"
	(set-no-controlo no (substituir 2 f (no-controlo no)))
)