

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

;; TODO: não há necessidade de 2 cond's
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


(defun h-numero-partilhas-vertical (linha n1 n2)
 "função que recebe uma linha de caixas e calcula o numero de partilhas de arestar para as caixas onde falta n1's e n2's arestar por completar"

	(cond
		((null linha) 0) ;; se a linha não existe
		((null (second linha)) 0) ;; se o segundo elemento da linha não existe
		(	(and 	(= 0 (third (first linha)))  ;;se a aresta da direita do primeiro elemento  esta por completar
					(= (third (first linha)) ;; se a aresta da direita do primeiro elemento esta por completar
					(fourth (second linha)));; se a aresta da esquerda do segundo elemento esta por completar
			)
			(cond
				( (and 	(= 1 (verificar-n-arcos-faltam (first linha) n1))  ;; verifica se a primeira caixa falta n1 arestar por completar
						(= 1 (verificar-n-arcos-faltam (second linha) n2))) ;; verifica se a segunda caixa falta n2 arestar por completar
						(+ 1 (h-numero-partilhas-vertical (cdr linha) n1 n2))  ;; soma 1 e faz a chamada recursiva retirando a primeira caixa
				)
				(T (h-numero-partilhas-vertical (cdr linha) n1 n2)) ;; faz a chamada recursiva retirando a primeira caixa
			)
		)
		(T (h-numero-partilhas-vertical (cdr linha) n1 n2)) ;;faz a chamada recursiva retirando a primeira caixa
	)
)


(defun aux-partilhas-vertical(caixas n1 n2)
 "Função auxiliar para a soma todas as partilhas de cada caixa verticalmente"
	(cond
		((null caixas) 0);; se não existir retorna 0
		(T 	(+ ;;soma
				(h-numero-partilhas-vertical (first caixas) n1 n2);;numero de partilhas da primeira linha
				(aux-partilhas-vertical (rest caixas) n1 n2) ;; chamada recursiva do resto das linhas das caixas
			)
		)
	)
)

(defun aux-partilhas-horizontal(caixas n1 n2)
 "Função auxiliar para a soma todas as partilhas de cada caixa horizontalmente"
	(cond
		((null caixas) 0) ;;se não existe caixas
		((null (second caixas)) 0) ;;se não existe a segunda linha
		(T 	(+
				(h-numero-partilhas-horizonta-duas-linhas-quadrados (first caixas) (second caixas) n1 n2 ) ;;calcula o nuemro de partilhas horizontais entre a primeira linha e a sgunda para n1 e n2
				(aux-partilhas-horizontal (rest caixas) n1 n2) ;;chamada recursiva descastando a primeira linha
			)
		)
	)
)

;; a função que faz mesmo o calculo total
(defun calcurar-n-partilhas-n1-n2 (caixas n1 n2)

 "função que calcula o numero de partilhas n1-n2 para as caixas recebidas"

	(cond
		((= n1 n2);; se n1 e n2 são iguas basta chamar uma vez por n2=n1
			(+;;soma
				(aux-partilhas-horizontal caixas n1 n2);; paritlhas horizontais
				(aux-partilhas-vertical caixas n1 n2);; partilhas verticais
			)
		)
		(T ;; caso contrar é preciso somar as partlhas n1-n2 e n2-n1
			(+
				(+
					(aux-partilhas-horizontal caixas n1 n2);;partilhas horizontais n1-n2
					(aux-partilhas-vertical caixas n1 n2);partilhas verticais n1-n2
				)
				(+
					(aux-partilhas-horizontal caixas n2 n1);;partilhas horizontais n2-n1
					(aux-partilhas-vertical caixas n2 n1);;partilhas verticais n2-n1
				)
			)
		)
	)
)


;; helpers


(defun get-helper()
	'(
		((0 0 1 0) (0 0 1 1))
		((0 0 1 0) (0 0 1 1))
	)
)
(defun get-helper2()
	'(
		((1 1 0 0) (1 1 0 0) (1 1 0 0))
		((1 1 0 0) (1 1 0 0) (1 1 0 0))
		((1 1 0 0) (1 1 0 0) (0 0 0 0))
	)
)


(defun get-helper3()
	'(
		( (0 0 1 0) (1 1 1 1) (0 1 0 1))
		( (0 0 1 0) (1 0 0 1) (1 1 0 0))
		( (0 0 1 0) (0 0 0 1) (1 0 0 0))
	)
)


(defun convert-top-bottom(linha)
 "função que junta as arestar de baixo e cima de cada caixa conforme a linha"
	(cond
		( (null (second linha)) nil )
		( T
			(cons (mapcar 'list (first linha) (second linha))  (convert-top-bottom (rest linha)));; junta e chama a proxima
		)
	)
)

(defun tabuleiro-c ()
	'(((nil nil t nil)(t nil t t)(nil nil t t)(nil nil t t)(nil nil t t))
	((nil nil t t)(nil nil t t)(nil nil t t)(t nil t t)(nil t t t)))
)
(defun matriz2d-transposta (m)
	"Faz a transposta da matriz m"
	(apply  #'mapcar (cons #'list m))
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


(defun tabuleiro-e ()
	'(((nil nil nil t nil nil)(nil nil nil t t t)(t t t t t nil)(nil nil nil t t nil)(nil nil nil t t nil)(nil nil t t t t)(nil nil t t t t))
	((nil nil nil t t t)(nil t nil nil t t)(nil t t nil t t)(nil nil t t nil nil)(t nil t nil t nil)(nil nil t t nil nil)(nil t t t t t)))
)


(defun mapear-para-binario (matriz)
 "função que recebe uma matriz 3D e a tranforma binario nil-0 e t-1"
	(cond
		((null matriz) nil)
		(T (cons (mapcar 'mapear-bool-binario (first matriz) ) (mapear-para-binario (rest matriz))))
	)
)


(defun mapear-bool-binario (matriz)
 "função que recebe uma matriz e a converte em binario nil-0 e t-1"
	(mapcar
		(lambda
			(elemento)
			(cond
				(elemento 1)
				(t 0)
			)
		)
		matriz
	)
)


(defun estrutura-test()
	(mapear-para-binario (converter-tabuleiro (tabuleiro-c)))
)


(defun calcular-heuristica2 (	n-caixas-objetivo ;;caixas a fazer
								n-caixas-fechadas ;; caixas já feitas
								n-caixas-faltar-1-arcos ;;caixas com 1 arco por acabar
								n-caixas-faltar-2-arcos ;;caixas com 2 arco por acabar
								n-caixas-faltar-3-arcos;;caixas com 3 arco por acabar
								n-caixas-faltar-4-arcos;;caixas com 4 arco por acabar
								n-partilhas-4-4 ;;numero de partilhas 4-4
								n-partilhas-4-3;;numero de partilhas 4-3
								n-partilhas-4-2;;numero de partilhas 4-2
								n-partilhas-4-1;;numero de partilhas 4-1
								n-partilhas-3-3;;numero de partilhas 3-3
								n-partilhas-3-2;;numero de partilhas 3-2
								n-partilhas-3-1;;numero de partilhas 3-1
								n-partilhas-2-2;;numero de partilhas 2-2
								n-partilhas-2-1;;numero de partilhas 2-1
								n-partilhas-1-1;;numero de partilhas 1-1
							)

 "função que calcula a segunda euristica"

	(let
		(
			(n-caixas-faltam (- n-caixas-objetivo n-caixas-fechadas)) ;;constante para calcular numero de caixas em falta
		)
        (+
			(heuristica2-aux-1-arco n-caixas-faltam n-caixas-faltar-1-arcos n-partilhas-1-1) ;; calcula arcos a usar para caixas onde falta 1 arco
			(heuristica2-aux-2-arco (max 0 (- n-caixas-faltam n-caixas-faltar-1-arcos)) n-caixas-faltar-2-arcos n-partilhas-2-1 n-partilhas-2-2) ;; calcula arcos a usar para caixas onde falta 2 arcos
			(heuristica2-aux-3-arco (max 0 (- n-caixas-faltam n-caixas-faltar-1-arcos n-caixas-faltar-2-arcos )) n-caixas-faltar-3-arcos n-partilhas-3-1 n-partilhas-3-2 n-partilhas-3-3) ;; calcula arcos a usar para caixas onde falta 3 arcos
			(heuristica2-aux-4-arco (max 0 (- n-caixas-faltam n-caixas-faltar-1-arcos n-caixas-faltar-2-arcos n-caixas-faltar-3-arcos )) n-caixas-faltar-4-arcos n-partilhas-4-1 n-partilhas-4-2 n-partilhas-4-3 n-partilhas-4-4) ;; calcula arcos a usar para caixas onde falta 4 arcos
		)
	)
)


(defun heuristica2-aux-1-arco (n-caixas-faltam n-caixas-faltar-1-arco n-partilhas-1-1)

 "função que calcula o numero de arestas a usar para caixas de 1 arco"
	(let*
		(
			(n-caixas-a-usar (min n-caixas-faltam n-caixas-faltar-1-arco)) ;;numero de caixas a usar
			(n-partilhas-a-usar (min n-partilhas-1-1 n-caixas-a-usar)) ;;numero de partilhas a aproveitar
		)
		(- n-caixas-a-usar (floor (/ n-partilhas-a-usar 2))) ;;calculo
    )
 )

(defun heuristica2-aux-2-arco (n-caixas-faltam n-caixas-faltar-2-arco n-partilhas-2-1 n-partilhas-2-2)
  "função que calcula o numero de arestas a usar para caixas de 1 arco"
	(cond
	   ((= 0 n-caixas-faltam) 0)
	   (t
			(let*
				(
					(n-caixas-a-usar (min n-caixas-faltam n-caixas-faltar-2-arco))  ;;numero de caixas a usar
					(n-partilhas-a-usar-2-2 (min n-partilhas-2-2 (- n-caixas-a-usar 1)))  ;;numero de partilhas a aproveitar
				)
				(- (* 2 n-caixas-a-usar) n-partilhas-2-1  n-partilhas-a-usar-2-2 ) ;;calculo
			 )

		)
	)
)

(defun heuristica2-aux-3-arco (n-caixas-faltam n-caixas-faltar-3-arco n-partilhas-3-1 n-partilhas-3-2 n-partilhas-3-3)
  "função que calcula o numero de arestas a usar para caixas de 1 arco"
	(cond
		( (= 0 n-caixas-faltam) 0 )
		(t
			(let*
				(
					(n-caixas-a-usar (min n-caixas-faltam n-caixas-faltar-3-arco))  ;;numero de caixas a usar
					(n-partilhas-a-usar-3-3 (min n-partilhas-3-3 (- n-caixas-a-usar 1))) ;;numero de partilhas a aproveitar
				)
				(- (* 3 n-caixas-a-usar) n-partilhas-3-1  n-partilhas-3-2 n-partilhas-a-usar-3-3 ) ;;calculo
			)
		)
	)
)

(defun heuristica2-aux-4-arco (n-caixas-faltam n-caixas-faltar-4-arco n-partilhas-4-1 n-partilhas-4-2 n-partilhas-4-3 n-partilhas-4-4)
  "função que calcula o numero de arestas a usar para caixas de 1 arco"
	(cond
		( (= 0 n-caixas-faltam) 0 )
		(t 	(let*
				(
					(n-caixas-a-usar (min n-caixas-faltam n-caixas-faltar-4-arco))  ;;numero de caixas a usar
					(n-partilhas-a-usar-4-4 (min n-partilhas-4-4 (- n-caixas-a-usar 1)))  ;;numero de partilhas a aproveitar
				)
				(- (* 4 n-caixas-a-usar) n-partilhas-4-1  n-partilhas-4-2 n-partilhas-4-3 n-partilhas-a-usar-4-4) ;;calculo
			)
		)
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
				(calcurar-n-partilhas-n1-n2 tabuleiro-convertido 4 4) ;;numero de partilhas 4-4
				(calcurar-n-partilhas-n1-n2 tabuleiro-convertido 4 3);;numero de partilhas 4-3
				(calcurar-n-partilhas-n1-n2 tabuleiro-convertido 4 2);;numero de partilhas 4-2
				(calcurar-n-partilhas-n1-n2 tabuleiro-convertido 4 1);;numero de partilhas 4-1
				(calcurar-n-partilhas-n1-n2 tabuleiro-convertido 3 3);;numero de partilhas 3-3
				(calcurar-n-partilhas-n1-n2 tabuleiro-convertido 3 2);;numero de partilhas 3-2
				(calcurar-n-partilhas-n1-n2 tabuleiro-convertido 3 1);;numero de partilhas 3-1
				(calcurar-n-partilhas-n1-n2 tabuleiro-convertido 2 2);;numero de partilhas 2-2
				(calcurar-n-partilhas-n1-n2 tabuleiro-convertido 2 1);;numero de partilhas 2-1
				(calcurar-n-partilhas-n1-n2 tabuleiro-convertido 1 1);;numero de partilhas 1-1
			)
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

(defun teste-ida-asterisco (n m o tabuleiro)
	(procura-generica-ida-asterisco (no-criar tabuleiro nil 0 '(0 0 0)) (criar-solucao o) 'sucessores 'ida-asterisco (criar-operacoes n m) (heuristica o))
)

(defun teste-ida-asterisco-h2 (n m o tabuleiro)
	(procura-generica-ida-asterisco (no-criar tabuleiro nil 0 '(0 0 0)) (criar-solucao o) 'sucessores 'ida-asterisco (criar-operacoes n m) (heuristica-2 o))
)
