
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
													(/ (no-profundidade (car abertos)) nos-gerados) ; penetrância
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
					(novo-nos-gerado (+ nos-gerados (length lista-sucessores)))
					(solucao (existe-solucao lista-sucessores f-solucao f-algoritmo));verifica se existe uma solucao nos sucessores para o dfs
				)
				(cond
					; devolve a solucao
					(solucao 	(list 	solucao ; nó solução
										(- (get-universal-time) tempo-inicial) ; tempo em segundos que a procura levou a encontrar a solução
										novo-nos-gerado ; número de nós gerados
										(1+ nos-expandidos) ; número de nós expandidos
										(no-profundidade solucao) ; profundidade do nó solução
										(/ (no-profundidade solucao) novo-nos-gerado) ; penetrância
										(bisecao (no-profundidade solucao) novo-nos-gerado margem-bisecao) ; fator de ramificacao
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
											novo-nos-gerado ; incrementa os número de nós gerados com o tamanho da lista de sucessores
											(1+ nos-expandidos) ; incrementa o número de nós expandidos
											margem-bisecao)
					)
				)
			)
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
;; Criação de tabuleiros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun criar-tabuleiro-vazio (n m)
	"Gera um tabuleiro vazio com n linhas e m colunas"
	(list
		(make-list (1+ n) :initial-element (make-list m)) ; cria uma matriz vazia com n+1 linhas e m colunas preechida com nils
		(make-list (1+ m) :initial-element (make-list n)) ; cria uma matriz vazia com m+1 linhas e n colunas preechida com nils
	)
)

(defun criar-tabuleiro-cheio (n m)
	"Gera um tabuleiro cheio com n linhas e m colunas"
	(list
		(make-list (1+ n) :initial-element (make-list m :initial-element T)) ; cria uma matriz vazia com n+1 linhas e m colunas preechida com t
		(make-list (1+ m) :initial-element (make-list n :initial-element T)) ; cria uma matriz vazia com m+1 linhas e n colunas preechida com t
	)
)


(defun tabuleiro-a ()
	"Devolve o tabuleiro da alinea A"
	'(((nil nil nil) (nil nil t) (nil t t) (nil nil t))
	((nil nil nil)(nil t nil)(nil nil t)(nil t t)))
)

(defun tabuleiro-b ()
	"Devolve o tabuleiro da alinea B"
	'(((nil nil t nil)(t t t t)(nil nil t t)(nil nil t t)(nil nil t t))
	((nil nil t t)(nil nil t t)(t t t t)(t nil t t)(nil t t t)))
)

(defun tabuleiro-c ()
	"Devolve o tabuleiro da alinea C"
	'(((nil nil t nil)(t nil t t)(nil nil t t)(nil nil t t)(nil nil t t))
	((nil nil t t)(nil nil t t)(nil nil t t)(t nil t t)(nil t t t)))
)

(defun tabuleiro-d ()
	"Devolve o tabuleiro da alinea D"
	'(((nil nil nil nil nil)(nil nil nil nil nil)(nil nil nil nil nil)(nil nil nil nil nil)(nil nil nil nil nil))
	((nil nil nil nil)(nil nil nil nil)(nil nil nil nil)(nil nil nil nil)(nil nil nil nil)(nil nil nil nil)))
)

(defun tabuleiro-e ()
	"Devolve o tabuleiro da alinea E"
	'(((nil nil nil t nil nil)(nil nil nil t t t)(t t t t t nil)(nil nil nil t t nil)(nil nil nil t t nil)(nil nil t t t t)(nil nil t t t t))
	((nil nil nil t t t)(nil t nil nil t t)(nil t t nil t t)(nil nil t t nil nil)(t nil t nil t nil)(nil nil t t nil nil)(nil t t t t t)))
)

(defun tabuleiro-f ()
	"Devolve o tabuleiro da alinea F"
	'(((nil nil nil nil nil nil nil)(nil nil nil nil nil nil nil)(nil nil nil nil nil nil nil)(nil nil nil nil nil nil nil)(nil t nil nil nil nil nil)(nil t nil nil nil nil nil)(nil nil nil nil nil nil nil)(nil nil nil nil nil nil nil))
	((nil nil nil nil nil nil nil)(nil nil nil nil t nil nil)(nil nil nil nil nil nil nil)(nil nil nil nil nil nil nil)(nil nil nil nil nil nil nil)(nil nil nil nil nil nil nil)(nil nil nil nil nil nil nil)(nil nil nil nil nil nil nil)))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manipulação de tabuleiros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-arcos-horizontais (tabuleiro)
	"Retorna a lista dos arcos horizontais de um tabuleiro"
	(first tabuleiro) ; devolve o primeiro elemento do tabuleiro
)


(defun get-arcos-verticais (tabuleiro)
	"Retorna a lista dos arcos verticiais de um tabuleiro"
	(first (rest tabuleiro)) ; devolve o segundo elemento do tabuleiro
)



(defun arco-na-posicao (i lista)
	"Recebe uma lista de arcos e tenta inserir um arco na posição i"
	(substituir (1- i) T lista) ; substitui pelo T no indice
)


(defun arco-aux (x y matriz)
	"Recebe uma matriz de arcos e tenta inserir um arco na posição x y"
	(let*
		(
			(x-aux (1- x)) ; altera a indexação do x para x-1
			(lista (elemento-por-indice x-aux matriz)) ;vai buscar a lista a matriz na posição x-1
			(nova-lista (arco-na-posicao y lista)) ; mete o arco na posição
		)
		(cond
			((null nova-lista) nil) ; se devolveu nil devolve nil
			(T (substituir x-aux nova-lista matriz)) ; caso contrário substitui a lista
		)

	)
)


(defun arco-horizontal (x y tabuleiro)
	"Recebe um tabuleiro e tenta inserir um arco na posição x y dos arcos horizontais"
	(let*
		(
			(arcos-horizontais (get-arcos-horizontais tabuleiro)) ; vai buscar a matriz de arcos horizontais ao tabuleiro
			(arcos-horizontais-resultado (arco-aux x y arcos-horizontais)) ; mete o arco na posição x e y da matriz
		)
		(cond
			((null arcos-horizontais-resultado) nil) ; se devolveu nil devolve nil
			(t (substituir 0 arcos-horizontais-resultado tabuleiro)) ; caso contrário substitui a matriz nos arcos horizontais do tabuleiro
		)

	)
)


(defun arco-vertical (x y tabuleiro)
	"Recebe um tabuleiro e tenta inserir um arco na posição x y dos arcos verticais"
	(let*
		(
			(arcos-verticais (get-arcos-verticais tabuleiro)) ; vai buscar a matriz de arcos verticais ao tabuleiro
			(arcos-verticais-resultado (arco-aux x y arcos-verticais)) ; mete o arco na posição x e y da matriz
		)
		(cond
			( (null arcos-verticais-resultado) nil) ; se devolveu nil devolve nil
			( t (substituir 1 arcos-verticais-resultado tabuleiro) ) ; caso contrário substitui a matriz nos arcos verticais do tabuleiro
		)
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operadores
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun criar-operacao (x y funcao)
	"Cria uma função lambda que representa uma operação através de uma operação (arco-horizontal/arco-vertical) e a posição x e y"
	(lambda (no) ; operador
			(let
				(
					( tabuleiro (funcall funcao x y (no-estado no)) ) ;executa a operação sobre o no
				)
				(cond
					((equal (no-estado no) tabuleiro) nil) ; se o estado do antecessor é igual ao estado do sucessor, é discartando devolvendo nil
					(t 	(set-no-profundidade  ; altera a profundidade do nó
							(set-no-pai ; altera a pai do nó antecessor devolvendo um novo nó
									(set-no-estado no tabuleiro) ; altera o estado do nó
									no
							)
							(1+ (no-profundidade no)) ; altera a profundidade do nó
						)
					)
				)
			)

	)
)


(defun criar-operacoes-decrementarY (x y funcao)
	"Decrementa o valor de y recursivamente e vai criando operações com o valor de x e y e a função"
	(cond
		( (= y 0) nil ) ; se y igual a 0 devolve nil
		( t (cons (criar-operacao x y funcao) (criar-operacoes-decrementarY x (1- y) funcao)) ) ; cria a operação para x e y e chama recusivamente a função com y-1
	)
)


(defun criar-operacoes-decrementarX (x y funcao)
	"Decrementa o valor de x recursivamente e vai chamando a função 'criar-operacoes-decrementarY' com o valor de x e y e a funcao"
	(cond
		( (= x 0) nil ) ; se x igual a 0 devolve nil
		( t (append (criar-operacoes-decrementarY x y funcao) (criar-operacoes-decrementarX (1- x) y funcao)) ) ; chama a função que cria as operações decrementando y para x e começando em y e chama recusivamente a função com x-1
	)
)


(defun criar-operacoes (n m)
	"Gera todos os operadores possíveis para um tabuleiro de n por m"
	(append
		(criar-operacoes-decrementarX (1+ n) m 'arco-horizontal) ; chama a função que cria as operações decrementando x partindo de x = (n+1) e y = m
		(criar-operacoes-decrementarX (1+ m) n 'arco-vertical) ; chama a função que cria as operações decrementando x partindo de x = (m+1) e y = n
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun aplicar-consecutivamente (tabuleiro operacoes)
	"Aplica um conjunto de operações consecutivas a um tabuleiro"
	(cond
		( (null operacoes) tabuleiro )
		( t (aplicar-consecutivamente (funcall (first operacoes) tabuleiro) (rest operacoes)) )
	)
)

(defun teste-preecher (n m)
	"Realiza um teste que gera todos os operadores possiveis e os aplica num tabuleiro n por m consecutivo, com objetivo a preecher todo o tabuleiro com arcos"
	(let
		(
			(operacoes (criar-operacoes n m))
			(tabuleiro (criar-tabuleiro-vazio n m))
		)
		(aplicar-consecutivamente tabuleiro operacoes)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Classificação do tabuleiro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mapear-bool-binario (lista)
	"Mapeia uma lista de valores booleanos (t e nil) para uma lista de valores binarios (1 0)"
	(mapcar
		(lambda
			(elemento)
			(cond
				(elemento 1) ; se o elemento é T devolve 1
				(t 0) ; caso contrário devolve 0
			)
		)
		lista
	)
)

(defun criar-candidatos-aux (matriz)
	(mapcar
		(lambda
			(linha)
			(reverse (rest (reverse linha)))
		)
		matriz
	)
)

(defun alisa (lista)
	"Retorna a lista com todos os elementos contidos na lista principal"
	(cond
		( (null lista) nil )
		( t (append (first lista) (alisa (rest lista))) )
	)
)

(defun criar-candidatos (matriz)
	"Cria uma matriz com os candidatos tendo em conta que arcos paralelos na mesma caixa são considerados candidatos"
	(criar-candidatos-aux
		(mapcar
			(lambda
				(linha)
				(maplist
					(lambda
						(lista)
						(cond
							( (< (length lista) 2) nil ) ; se exitirem menos de 2 elementos em paralelo não pode existir um candidato
							( t (and (first lista) (second lista)) ) ; se existirem ambos os arcos paralelos e consecutivos devolve t, caso contrário nil
						)
					)
					linha
				)
			)
			(matriz2d-transposta matriz) ; tranposta da matriz para que se consiga ter as linhas da matriz com os arcos paralelos
		)
	)
)



(defun numero-caixas-fechadas (tabuleiro)
	"Devolve o número fechadas num tabuleiro"
	(let
		(
			(candidatos1 (alisa (criar-candidatos (get-arcos-horizontais tabuleiro)))) ; gera os candidatos dos arcos horizontais num lista linear
			(candidatos2 (alisa (matriz2d-transposta (criar-candidatos (get-arcos-verticais tabuleiro))))) ; gera os candidatos dos arcos verticais numa lista linear
		)
		(apply  '+ 	(mapear-bool-binario ; mapeia a lista para elementos binários e somas os seus valores
						(mapcar
							(lambda (&rest lista)
									(and (first lista) (second lista)); aplica um and entre o candidato dos horizontais e o candidato dos verticais, caso ambos sejam t existe de facto um quadrado
							)
							candidatos1
							candidatos2
						)
					)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ordenação
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;f-algoritmo
(defun bfs (abertos sucessores)
	"Função de ordenação e junção da lista de abertos com a lista de sucessores no algoritmo breadth-first"
	(append abertos sucessores) ; mete a lista de abertos à esquerda da lista de sucessores
)

(defun dfs (abertos sucessores)
	"Função de ordenação e junção da lista de abertos com a lista de sucessores no algoritmo depth-first"
	(append sucessores abertos) ; mete a lista de abertos à direita da lista de sucessores
)

(defun a-asterisco (abertos sucessores)
	"Função de ordenação e junção da lista de abertos com a lista de sucessores no algoritmo a*"
	(sort (append abertos sucessores) (lambda (no1 no2) (<= (no-controlo-f no1) (no-controlo-f no2)))) ; junta os aberto e os sucessores e ordena a lista resultante através do custo f
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sucessores
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sucessores (no lista-operadores f-algoritmo prof-max &optional (heuristica nil))
	"Gera os sucessores"
	(cond
		( (and (eql f-algoritmo 'dfs) (>= (no-profundidade no) prof-max)) nil) ;se for o algoritmo depth-first e a profundidade do nó for igual ou superior à profundidade máxima, devolve uma lista vazia
		( t (let
				(
					(funcao (lambda (op) ;função que irá gerar os nós sucessores para cada operação
									(let*
										(
											(sucessor (funcall op no))
										)
										(cond
											((null sucessor) nil) ; se o sucessor gerado pelo operador não pôde ser aplicado, devolve nil
											((not (or (eql f-algoritmo 'a-asterisco) (eql f-algoritmo 'ida-asterisco))) sucessor) ; se não for uma procura informada devolve o sucessor (não tem elemento de controle de custos)
											( t
												(let*
													(
														(g (1+ (no-controlo-g no))) ; calculo custo g do sucessor
														(h (funcall heuristica sucessor)) ; calculo custo h* do sucessor
														(f (+ g h)) ; calculo custo f do sucessor
													)
													(set-no-controlo sucessor (list g h f)) ; alterar o elemento de controlo do sucessor
												)
											)
										)
									)
							)
					)
				)
				(limpar-nils (mapcar funcao lista-operadores)) ; executa todas as operações e limpa aquelas que não foram aplicadas	
			)
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Heurísticas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun criar-solucao (o)
  (lambda (no) (= (numero-caixas-fechadas (no-estado no)) o))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Heurísticas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun heuristica (o)
	(lambda (no) (- o (numero-caixas-fechadas (no-estado no)) 1))
)

; aqui estava a fazer cada iteração, acho que consegues reaproveitar
(defun calcular-heuristica2-arcos-faltam 	(
												n-caixas-faltam ; número de caixas que faltam preencher
												n ; numero de arcos a faltar
												n-caixas-faltar-n-arcos ; número de caixas onde faltam n arcos
												n-partilhas-relevantes ; número de partilhas que são relevantes às caixas onde faltam n arcos
											)
	(let*
		(
			(n-caixas-ficam-a-faltar (- n-caixas-faltam (min n-caixas-faltam n-caixas-faltar-n-arcos)) ); número de caixas que ficaram a faltar se utilizarmos todas as caixas possiveis
		)
		(cond
			( 	(= n-caixas-ficam-a-faltar 0)
				(list   0 ; número de caixas que ficam a faltar
						(- (* n-caixas-faltar-n-arcos n) n-partilhas-relevantes) ; número de arcos necessários para as caixas
				)
			)
			( 	t
				(list   n-caixas-ficam-a-faltar ; número de caixas que ficam a faltar
						(- (* n-caixas-faltar-n-arcos n) n-partilhas-relevantes) ; número de arcos necessários para as caixas
				)
			)
		)
	)



)

(defun calcular-heuristica2-aux (	n-caixas-faltam
									n-caixas-faltar-1-arcos
									n-caixas-faltar-2-arcos
									n-caixas-faltar-3-arcos
									n-caixas-faltar-4-arcos)

	-1
)

(defun calcular-heuristica2 (	n-caixas-objetivo
								n-caixas-fechadas
								n-caixas-faltar-1-arcos
								n-caixas-faltar-2-arcos
								n-caixas-faltar-3-arcos
								n-caixas-faltar-4-arcos
								n-partilhas-4-4
								n-partilhas-4-3
								n-partilhas-4-2
								n-partilhas-4-1
								n-partilhas-3-3
								n-partilhas-3-2
								n-partilhas-3-1
								n-partilhas-2-2
								n-partilhas-2-1
								n-partilhas-1-1
							)
	(let
		(
			(n-caixas-faltam (- n-caixas-objetivo n-caixas-fechadas))
		)



	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun teste-bfs (n m o tabuleiro)
   (procura-generica (no-criar tabuleiro) (criar-solucao o) 'sucessores 'bfs (criar-operacoes n m))
)

(defun teste-dfs (n m o p tabuleiro)
   (procura-generica (no-criar tabuleiro) (criar-solucao o) 'sucessores 'dfs (criar-operacoes n m) p)
)

(defun teste-a-asterisco (n m o tabuleiro)
	(procura-generica (no-criar tabuleiro nil 0 '(0 0 0)) (criar-solucao o) 'sucessores 'a-asterisco (criar-operacoes n m) nil (heuristica o))
)
(defun teste-a-asterisco-h2 (n m o tabuleiro)
	(procura-generica (no-criar tabuleiro nil 0 '(0 0 0)) (criar-solucao o) 'sucessores 'a-asterisco (criar-operacoes n m) nil (heuristica-2 o))
)

(defun teste-ida-asterisco-h2 (n m o tabuleiro)
	(procura-generica-ida-asterisco (no-criar tabuleiro nil 0 '(0 0 0)) (criar-solucao o) 'sucessores 'a-asterisco (criar-operacoes n m) nil (heuristica-2 o))
)
