

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
    ( (null lista) nil )
    ( t
      (let*
          (
            (is-dfs (eql f-algoritmo 'dfs))
            (proximo-no-lista (first lista))
            (estados-iguais (equal (no-estado no) (no-estado proximo-no-lista)))
            (profundidade-superior (> (no-profundidade no) (no-profundidade proximo-no-lista)) )
            (dfs-exitep (and is-dfs estados-iguais profundidade-superior))
            (else-existep (and (not is-dfs) estados-iguais))
          )

        (cond
          ( dfs-exitep t )
          ( else-existep t )
          ( t (existep no (rest lista) f-algoritmo) )
        )
      )
    )
  )


)

(defun existe-solucao (lista f-solucao f-algoritmo)
	"Verifica se existe uma solucao ao problema numa lista de sucessores para o algoritmo dfs"
	(cond
		((not (eql f-algoritmo 'dfs)) nil)
		((null lista) nil)
		((funcall f-solucao (car lista)) (car lista))
		(T (existe-solucao (cdr lista) f-solucao f-algoritmo))
	)
)

(defun procura-generica (no-inicial f-solucao f-sucessores f-algoritmo lista-operadores &optional (prof-max  nil) (heuristica nil) (abertos (list no-inicial)) (fechados nil) (tempo-inicial (get-universal-time)))
  "Permite procurar a solucao de um problema usando a procura no espaço de estados. A partir de um estado inicial,
 de uma funcao que gera os sucessores e de um dado algoritmo. De acordo com o algoritmo pode ser usada um limite
 de profundidade, uma heuristica e um algoritmo de ordenacao"
 	(cond
		((null abertos) nil); nao existe solucao ao problema
		((funcall f-solucao (car abertos)) (list (car abertos) (- (get-universal-time) tempo-inicial))); se o primeiro dos abertos e solucao este no e devolvido
		((existep (first abertos) fechados f-algoritmo) (procura-generica no-inicial f-solucao f-sucessores f-algoritmo lista-operadores prof-max heuristica (cdr abertos) fechados tempo-inicial) ); se o no ja existe nos fechados e ignorado
		(T 
			(let*
				(
					(lista-sucessores (funcall f-sucessores (first abertos)  lista-operadores f-algoritmo prof-max heuristica));lista dos sucessores do primeiro dos abertos
					(solucao (existe-solucao lista-sucessores f-solucao f-algoritmo));verifica se existe uma solucao nos sucessores para o dfs
				)
				(cond
					(solucao (list solucao (- (get-universal-time) tempo-inicial))); devolve a solucao
					(T (procura-generica no-inicial f-solucao f-sucessores f-algoritmo lista-operadores prof-max heuristica (funcall f-algoritmo (rest abertos) lista-sucessores) (cons (car abertos) fechados)tempo-inicial)); expande a arvore se o primeiro dos abertos nao for solucao
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
		( (null l) nil )
		( (= i 0) (cons valor (rest l)) )
		( t (cons (first l) (substituir (1- i) valor (rest l))) )
	)
)

(defun elemento-por-indice (i l)
	"Devolve o elemento de uma lista correspondente ao índice i"
	(cond
		( (null l) nil )
		( (= i 0) (first l) )
		( t (elemento-por-indice (1- i) (rest l)) )
	)
)

(defun matriz2d-transposta (m)
	"Faz a transposta da matriz m"
	(apply  #'mapcar (cons #'list m))
)

(defun limpar-nils (lista)
	(apply #'append
		(mapcar (lambda (e)
						(cond
							((null e) nil)
							(t (list e))
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
		(make-list (1+ n) :initial-element (make-list m))
		(make-list (1+ m) :initial-element (make-list n))
	)
)

(defun criar-tabuleiro-cheio (n m)
	"Gera um tabuleiro cheio com n linhas e m colunas"
	(list
		(make-list (1+ n) :initial-element (make-list m :initial-element T))
		(make-list (1+ m) :initial-element (make-list n :initial-element T))
	)
)


(defun tabuleiro1 ()
	"retorna um tabuleiro vazio de dimensão 3 x 3"
	(criar-tabuleiro-vazio 3 3)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manipulação de tabuleiros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-arcos-horizontais (tabuleiro)
	"Retorna a lista dos arcos horizontais de um tabuleiro"
	(first tabuleiro)
)


(defun get-arcos-verticais (tabuleiro)
	"Retorna a lista dos arcos verticiais de um tabuleiro"
	(first (rest tabuleiro))
)



(defun arco-na-posicao (i lista)
	"Recebe uma lista de arcos e tenta inserir um arco na posição i"
	(cond
		((eq (elemento-por-indice (1- i) lista) T) nil)
		(t (substituir (1- i) T lista))
	)
	(substituir (1- i) T lista)
)



(defun arco-aux (x y matriz)
	"Recebe uma matriz de arcos e tenta inserir um arco na posição x y"
	(let*
		(
			(x-aux (1- x))
			(lista (elemento-por-indice x-aux matriz))
			(nova-lista (arco-na-posicao y lista))
		)
		(cond
			((null nova-lista) nil)
			(T (substituir x-aux nova-lista matriz))
		)

	)
)


(defun arco-horizontal (x y tabuleiro)
	"Recebe um tabuleiro e tenta inserir um arco na posição x y dos arcos horizontais"
	(let*
		(
			(arcos-horizontais (get-arcos-horizontais tabuleiro))
			(arcos-horizontais-resultado (arco-aux x y arcos-horizontais))
		)
		(cond
			((null arcos-horizontais-resultado) nil)
			(t (substituir 0 arcos-horizontais-resultado tabuleiro))
		)

	)
)


(defun arco-vertical (x y tabuleiro)
	"Recebe um tabuleiro e tenta inserir um arco na posição x y dos arcos verticais"
	(let*
		(
			(arcos-verticais (get-arcos-verticais tabuleiro))
			(arcos-verticais-resultado (arco-aux x y arcos-verticais))
		)
		(cond
			( (null arcos-verticais-resultado) nil)
			( t (substituir 1 arcos-verticais-resultado tabuleiro) )
		)
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operadores
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun criar-operacao (x y funcao)
	"Cria uma função lambda que representa uma operação através de uma operação (arco-horizontal/arco-vertical) e a posição x e y"
	(lambda (no) 
			(no-criar 
				(funcall funcao x y (no-estado no)) 
				no 
				(1+ (no-profundidade no))
			)
	)
)


(defun criar-operacoes-decrementarY (x y funcao)
	"Decrementa o valor de y recursivamente e vai criando operações com o valor de x e y e a função"
	(cond
		( (= y 0) nil )
		( t (cons (criar-operacao x y funcao) (criar-operacoes-decrementarY x (1- y) funcao)) )
	)
)


(defun criar-operacoes-decrementarX (x y funcao)
	"Decrementa o valor de x recursivamente e vai chamando a função 'criar-operacoes-decrementarY' com o valor de x e y e a funcao"
	(cond
		( (= x 0) nil )
		( t (append (criar-operacoes-decrementarY x y funcao) (criar-operacoes-decrementarX (1- x) y funcao)) )
	)
)


(defun criar-operacoes (n m)
	"Gera todos os operadores possíveis para um tabuleiro de n por m"
	(append
		(criar-operacoes-decrementarX (1+ n) m 'arco-horizontal)
		(criar-operacoes-decrementarX (1+ m) n 'arco-vertical)
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

(defun mapear-bool-binario (matriz)
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
	(criar-candidatos-aux 
		(mapcar
			(lambda 
				(linha)
				(maplist 
					(lambda 
						(lista)
						(cond 
							( (< (length lista) 2) nil )
							( t (and (first lista) (second lista)) )
						)
					)
					linha
				)
			)
			(matriz2d-transposta matriz)
		)
	)
)



(defun numero-caixas-fechadas (tabuleiro)
	"Devolve o número fechadas num tabuleiro"
	(let 
		(
			(candidatos1 (alisa (criar-candidatos (get-arcos-horizontais tabuleiro))))
			(candidatos2 (alisa (matriz2d-transposta (criar-candidatos (get-arcos-verticais tabuleiro)))))
		)
		(apply  '+ 	(mapear-bool-binario 
						(mapcar 
							(lambda (&rest lista) 
									(and (first lista) (second lista))
							) 
							candidatos1 
							candidatos2
						)
					)
		)
	)
)


;(NUMERO-CAIXAS-FECHADAS2 '(((T T) (T T) (NIL NIL)) ((T T) (T T) (NIL NIL))))


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



(defun no-alterar-estado (no estado)
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
 "Altera o pai do nó"
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
	""
	(append abertos sucessores)
)

(defun dfs (abertos sucessores)
	""
	(append sucessores abertos)
)

(defun a-asterisco (abertos sucessores)
	""
	(sort (append abertos sucessores) (lambda (no1 no2) (<= (no-controlo-f no1) (no-controlo-f no2))))
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sucessores
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sucessores (no lista-operadores f-algoritmo prof-max &optional (heuristica nil))
	"Gera os sucessores"
	(cond
		( (and (eql f-algoritmo 'dfs) (>= (no-profundidade no) prof-max)) nil)
		( t (let
				(
					(funcao (lambda (op)
									(cond 
											((not (eql f-algoritmo 'a-asterisco)) (funcall op no))
											( t (let*
													(
														(g (1+ (no-controlo-g no)))
														(sucessor (funcall op no))
														(h (funcall heuristica sucessor))
														(f (+ g h))
													)
													(set-no-controlo sucessor (list g h f))
												)
											)
									)
							)
					)			
				)
				(limpar-nils (mapcar funcao lista-operadores))
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
;; Testes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun teste-bfs (n m o)
   (procura-generica (no-criar (criar-tabuleiro-vazio n m)) (criar-solucao o) 'sucessores 'bfs (criar-operacoes n m))
)

(defun teste-dfs (n m o p)
   (procura-generica (no-criar (criar-tabuleiro-vazio n m)) (criar-solucao o) 'sucessores 'dfs (criar-operacoes n m) p)
)

(defun teste-a-asterisco (n m o)
	(procura-generica (no-criar (criar-tabuleiro-vazio n m) nil 0 '(0 0 0)) (criar-solucao o) 'sucessores 'a-asterisco (criar-operacoes n m) nil (heuristica o))
)

