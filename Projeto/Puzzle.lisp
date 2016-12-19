

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adapter - nova-para-antiga
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun tabuleiro-teste-novo ()
	""
	'(
		(
			((0 0) (0 1))
			((0 1) (0 2))
			((0 2) (0 3))
			((0 3) (0 4))
			((3 2) (3 3))
		)
		4
		4
	)
)


(defun get-arcos-nova-representacao (novo-estado)
	"Devolve a componente dos arcos do tabuleiro"
	(first novo-estado)
)

(defun get-coordenada-x-maxima-tabuleiro (novo-estado)
	"Devolve a componente dos arcos do tabuleiro"
	(second novo-estado)
)

(defun get-coordenada-y-maxima-tabuleiro (novo-estado)
	"Devolve a componente dos arcos do tabuleiro"
	(third novo-estado)
)

(defun adicionar-arco-coordenadas (coordenada1 coordenada2 estado-antigo)
	"Adiciona um arco através de duas coordenadas a um estado com representacao antiga"
	(let*
		(
			(sub1 (abs (- (first coordenada1) (first coordenada2))) )
			(sub2 (abs (- (second coordenada1) (second coordenada2))) )
			(min-x (min (first coordenada1) (first coordenada2)) )
			(min-y (min (second coordenada1) (second coordenada2)) )
			(horizontais (first estado-antigo))
			(verticais (second estado-antigo))
		)
		(cond 
			( (= sub1 1) 	(list 	horizontais
									(substituir min-y ; substituir a linha
												(substituir min-x 
															T 
															(elemento-por-indice min-y verticais)
												)
												verticais
									)
									
							)
			)
						
			( (= sub2 1) 	(list 	(substituir min-x
												(substituir min-y 
															T 
															(elemento-por-indice min-x horizontais)
												)
												horizontais
									)
									verticais
							
							)
							
			)
		)
	)
)

(defun aplicar-arcos (arcos-nova-representacao estado-antigo)
	"Aplica uma lista de arcos num estado com representação antiga"
	(cond 
		((null arcos-nova-representacao) estado-antigo)
		(t 
			(let
				(
					(coordenada1 (first (first arcos-nova-representacao)))
					(coordenada2 (second (first arcos-nova-representacao)))
				)
				(aplicar-arcos (rest arcos-nova-representacao) (adicionar-arco-coordenadas coordenada1 coordenada2 estado-antigo))
			)
		)
	)
	
)


(defun converter-estado-novo-para-antigo (estado-nova-representacao)
	"Transforma um conjunto de coordenadas de arcos para uma representação anterior"
	(let
		(
			(tabuleiro-adaptado (criar-tabuleiro-vazio 
										(get-coordenada-x-maxima-tabuleiro estado-nova-representacao)
										(get-coordenada-y-maxima-tabuleiro estado-nova-representacao)
								)
			)
			(arcos-nova-representacao  (get-arcos-nova-representacao estado-nova-representacao))
		)
		(aplicar-arcos arcos-nova-representacao tabuleiro-adaptado)
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adapter - antiga-para-nova
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun converter-arcos-horizontais-antigo-novo-aux-y (lista x y)
	""
	(cond
		((null lista) nil)
		( t 
			(append
				(converter-arcos-horizontais-antigo-novo-aux-y (rest lista) x (1- y))
				(cond 
					( (first lista) (list (list (list x y) (list x (1+ y))) ) )
					( t nil )
				)
			)
		)
	)
)

(defun converter-arcos-horizontais-antigo-novo-aux (horizontais x y)
	""
	(cond
		((null horizontais) nil)
		( t (append (converter-arcos-horizontais-antigo-novo-aux (rest horizontais) (1- x) y) 
					(converter-arcos-horizontais-antigo-novo-aux-y (reverse (first horizontais)) x y)
			) 
		)
	)
)

(defun converter-arcos-horizontais-antigo-novo (horizontais x y)

	(converter-arcos-horizontais-antigo-novo-aux (reverse horizontais) x y)
)



(defun converter-arcos-verticais-antigo-novo-aux-y (lista x y)
	""
	(cond
		((null lista) nil)
		( t 
			(append
				(converter-arcos-verticais-antigo-novo-aux-y (rest lista) x (1- y))
				(cond 
					( (first lista) (list (list (list y x) (list (1+ y) x)) ) )
					( t nil )
				)
			)
		)
	)
)

(defun converter-arcos-verticais-antigo-novo-aux (verticais x y)
	""
	(cond
		((null verticais) nil)
		( t (append (converter-arcos-verticais-antigo-novo-aux (rest verticais) (1- x) y) 
					(converter-arcos-verticais-antigo-novo-aux-y (reverse (first verticais)) x y)
			) 
		)
	)
)

(defun converter-arcos-verticais-antigo-novo (verticais x y)

	(converter-arcos-verticais-antigo-novo-aux (reverse verticais) x y)
)


(defun converter-estado-antigo-para-novo (estado-representacao-antiga)
	"Transforma um estado na representação num estado na representação nova"
	(let 
		(
			(numero-caixas-horizontal (length (first (first estado-representacao-antiga))))
			(numero-caixas-vertical (length (first (second estado-representacao-antiga))))
		)
		(list
			(append (converter-arcos-horizontais-antigo-novo (first estado-representacao-antiga) numero-caixas-vertical (1- numero-caixas-horizontal) )  
					(converter-arcos-verticais-antigo-novo (second estado-representacao-antiga) numero-caixas-horizontal (1- numero-caixas-vertical))
			)
			numero-caixas-vertical
			numero-caixas-horizontal
		)
		
	)	
)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adapter - Redefinição de funções
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Redefinição da função
(defun no-estado (no)
	"Devolve o estado do nó"
	(converter-estado-novo-para-antigo (elemento-por-indice 0 no))
)

;; Redefinição da função
(defun set-no-estado (no estado)
	"Altera o estado de um nó"
	(substituir 0 (converter-estado-antigo-para-novo estado) no)
)

;; Redefinição da função
(defun no-criar (estado &optional (pai nil) (profundidade 0) (controlo nil))
  "Cria um nó"
  (list (converter-estado-antigo-para-novo estado) pai profundidade controlo)
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

(defun numero-caixas-horizontal (tabuleiro)
	"Dá número de caixas na horizontal"
	(length (first (get-arcos-horizontais tabuleiro)))
)

(defun numero-caixas-vertical (tabuleiro)
	"Dá número de caixas na vertical"
	(length (first (get-arcos-verticais tabuleiro)))
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
	"Remove a última coluna da matriz"
	(mapcar ; passa por cada lista da matriz
		(lambda
			(linha)
			(reverse (rest (reverse linha))) ; retira o último elemento da lista
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
;; Ordenação
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun ida-asterisco (abertos sucessores)
	"Função de ordenação e junção da lista de abertos com a lista de sucessores no algoritmo ida*"
	(append sucessores abertos) ; mete a lista de abertos à direita da lista de sucessores
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Heuristica 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun verificar-n-arcos-faltam (caixa n)
 "Verifica o numero de arcos que faltam para completar a caixa é igual ao numero recebido"
	(cond
		( (= n (- 4 (apply '+ caixa))) 1 ) ;; verifica se o numero recebido e igual aos arcos da caixa, se sim devolve 1
		( t 0 );; se não for igual devolve 0
	)
)

;; parte dos numeros de caixas com n linhas a faltar

(defun n-caixas-a-faltar-x-arcos(caixas n)
 "Recebe as caixas e o numero de arcos a faltar e verifica quantas caixas existem com esse numero de arestar por completar"
	(apply
		'+;; soma o numero
		(mapcar ;; mapcar  para cada linha de caixas nas caixas
			(lambda (linha-caixa) ;; função lambda que soma cada verficação de cada caixa da linha
				(apply '+
						(mapcar
							(lambda (caixa)
								(verificar-n-arcos-faltam caixa n);; verifica se os arcos que faltam na caixa coincide com o n recebido
							)
							linha-caixa ;; linha com caixas
						)
				)
			)
			caixas;; lista com caixas (linhas com caixas)
		)
	)
)


;;parte das partilhas
(defun h-numero-partilhas-horizonta-duas-linhas-quadrados(linha1 linha2 n1 n2)
 "função que calcula o numero de partilhas na horizontal recebendo duas linhas e o numero de arcos que deve faltar em cada linha"
	(apply '+
		(mapcar
			(lambda(x y);; por cada caixa da linha1 e linha 2
				(cond
					( (and (= 1 (verificar-n-arcos-faltam x n1)) (= 1 (verificar-n-arcos-faltam y n2)) ) ;;verifica se ambas as posições tem em falta o arco e se tem o mesmo numero de arcos por completar com os n's recebidos
						(cond
							((and (= (second x) (first y)) (= 0 (second x))) 1) ;;verifica se ambos tem na posição certa o arco por completar, se sim então é partilhado
							(T 0);; se não, não é um arco partilhado
						)
					)
					(T 0);; se não faltam os n's arcos
				)
			)
			linha1 linha2 ;; linhas de caixas
		)
	)
)





(defun convert-top-bottom(linha)
	"Função que junta as arestar de baixo e cima de cada caixa conforme a linha"
	(cond
		( (null (second linha)) nil )
		( T
			(cons (mapcar 'list (first linha) (second linha))  (convert-top-bottom (rest linha)));; junta e chama a proxima
		)
	)
)

(defun converter-tabuleiro(tabuleiro)
 "função que converte o tabuleiro em caixas"
	(mapcar 'converter-aux (convert-top-bottom (car tabuleiro)) (matriz2d-transposta (convert-top-bottom (car (rest tabuleiro)))) )
)

(defun converter-aux(tops-bottoms lefts-rights)
 "função que junta os tops-bottoms e os left-rights em toda uma caixa"
	(mapcar
		(lambda (x y)
				(append x (reverse y))
		)
		tops-bottoms
		lefts-rights
	)
)

(defun mapear-para-binario (matriz)
	"Função que recebe uma matriz3D e a tranforma para binario nil-0 e t-1"
	(cond
		((null matriz) nil)
		(T (cons (mapcar 'mapear-bool-binario (first matriz) ) (mapear-para-binario (rest matriz))))
	)
)

(defun calcular-heuristica2 (	n-caixas-objetivo ;;caixas a fazer
								n-caixas-fechadas ;; caixas já feitas
								n-caixas-faltar-1-arcos ;;caixas com 1 arco por acabar
								n-caixas-faltar-2-arcos ;;caixas com 2 arco por acabar
								n-caixas-faltar-3-arcos;;caixas com 3 arco por acabar
								n-caixas-faltar-4-arcos;;caixas com 4 arco por acabar
							
							)

	"função que calcula a segunda heuristica"
	(let
		(
			(n-caixas-faltam (- n-caixas-objetivo n-caixas-fechadas)) ;;constante para calcular numero de caixas em falta
		)
       
		
		
		(1- (+
		(min n-caixas-faltam n-caixas-faltar-1-arcos)
		(*(min (max 0 (- n-caixas-faltam n-caixas-faltar-1-arcos)) n-caixas-faltar-2-arcos) 2)
		(* (min (max 0 (- n-caixas-faltam n-caixas-faltar-1-arcos n-caixas-faltar-2-arcos )) n-caixas-faltar-3-arcos) 3)
		(* (min (max 0 (- n-caixas-faltam n-caixas-faltar-1-arcos n-caixas-faltar-2-arcos n-caixas-faltar-3-arcos )) n-caixas-faltar-4-arcos) 4)
		))
	)
)



(defun heuristica-2 (o)
	"função que devolve o calculo da heuristica para um no"
	(lambda (no)
		(let
			(
				( tabuleiro-convertido (mapear-para-binario (converter-tabuleiro (no-estado no))) );;converte o tabuleiro para bonario
			)
			(calcular-heuristica2
				o
				(n-caixas-a-faltar-x-arcos tabuleiro-convertido 0) ;;numero de caixas completas
				(n-caixas-a-faltar-x-arcos tabuleiro-convertido 1);;numero de caixas onde falta 1 arco
				(n-caixas-a-faltar-x-arcos tabuleiro-convertido 2);;numero de caixas onde falta 2 arcos
				(n-caixas-a-faltar-x-arcos tabuleiro-convertido 3);;numero de caixas onde falta 3 arcos
				(n-caixas-a-faltar-x-arcos tabuleiro-convertido 4);;numero de caixas onde falta 4 arcos
			)
		)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun teste-bfs (o tabuleiro)
	(procura-generica (no-criar tabuleiro) (criar-solucao o) 'sucessores 'bfs (criar-operacoes (numero-caixas-vertical tabuleiro) (numero-caixas-horizontal tabuleiro)))
)

(defun teste-dfs (o p tabuleiro)
	(procura-generica (no-criar tabuleiro) (criar-solucao o) 'sucessores 'dfs (criar-operacoes (numero-caixas-vertical tabuleiro) (numero-caixas-horizontal tabuleiro)) p)
)

(defun teste-a-asterisco (o tabuleiro)
	(procura-generica (no-criar tabuleiro nil 0 '(0 0 0)) (criar-solucao o) 'sucessores 'a-asterisco (criar-operacoes (numero-caixas-vertical tabuleiro) (numero-caixas-horizontal tabuleiro)) nil (heuristica o))
)

(defun teste-a-asterisco-h2 (o tabuleiro)
	(procura-generica (no-criar tabuleiro nil 0 '(0 0 0)) (criar-solucao o) 'sucessores 'a-asterisco (criar-operacoes (numero-caixas-vertical tabuleiro) (numero-caixas-horizontal tabuleiro)) nil (heuristica-2 o))
)

(defun teste-ida-asterisco (o tabuleiro)
	(procura-generica-ida-asterisco (no-criar tabuleiro nil 0 '(0 0 0)) (criar-solucao o) 'sucessores 'ida-asterisco (criar-operacoes (numero-caixas-vertical tabuleiro) (numero-caixas-horizontal tabuleiro)) (heuristica o))
)

(defun teste-ida-asterisco-h2 (o tabuleiro)
	(procura-generica-ida-asterisco (no-criar tabuleiro nil 0 '(0 0 0)) (criar-solucao o) 'sucessores 'ida-asterisco (criar-operacoes (numero-caixas-vertical tabuleiro) (numero-caixas-horizontal tabuleiro)) (heuristica-2 o))
)



