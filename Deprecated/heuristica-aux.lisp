;; tronco comum


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Heuristica 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun verificar-n-arcos-faltam (caixa n)
	(cond
		( (= n (- 4 (apply '+ caixa))) 1 )
		( t 0 )
	)
)

;; parte dos numeros de caixas com n linhas a faltar

(defun n-caixas-a-faltar-x-arcos(caixas n)
	(apply
		'+
		(mapcar
			(lambda (x) ;; TODO dar um nome como deve de ser a este x
				(apply '+
						(mapcar
							(lambda (z)
								(verificar-n-arcos-faltam z n)
							)
							x
						)
				)
			)
			caixas
		)
	)
)


;;parte das partilhas

;; TODO: não há necessidade de 2 cond's
(defun h-numero-partilhas-horizonta-duas-linhas-quadrados(linha1 linha2 n1 n2)
	(apply '+
		(mapcar
			(lambda(x y)
				(cond
					( (and (= 1 (verificar-n-arcos-faltam x n1)) (= 1 (verificar-n-arcos-faltam y n2)) )
						(cond
							((and (= (second x) (first y)) (= 0 (second x))) 1)
							(T 0)
						)
					)
					(T 0)
				)
			)
			linha1 linha2
		)
	)
)


(defun h-numero-partilhas-vertical (linha n1 n2)
	(cond
		((null linha) 0)
		((null (second linha)) 0)
		(	(and 	(= 0 (third (first linha))) 
					(= (third (first linha)) 
					(fourth (second linha)))
			)
			(cond
				( (and 	(= 1 (verificar-n-arcos-faltam (first linha) n1)) 
						(= 1 (verificar-n-arcos-faltam (second linha) n2)))
						(+ 1 (h-numero-partilhas-vertical (cdr linha) n1 n2)) 
				)
				(T (h-numero-partilhas-vertical (cdr linha) n1 n2))
			)
		)
		(T (h-numero-partilhas-vertical (cdr linha) n1 n2))
	)
)


(defun aux-partilhas-vertical(caixas n1 n2)
	(cond
		((null caixas) 0)
		(T 	(+ 	
				(h-numero-partilhas-vertical (first caixas) n1 n2) 
				(aux-partilhas-vertical (rest caixas) n1 n2) 
			)
		)
	)
)

(defun aux-partilhas-horizontal(caixas n1 n2)
	(cond
		((null caixas) 0)
		((null (second caixas)) 0)
		(T 	(+ 
				(h-numero-partilhas-horizonta-duas-linhas-quadrados (first caixas) (second caixas) n1 n2 ) 
				(aux-partilhas-horizontal (rest caixas) n1 n2)
			)
		)
	)
)

;; a função que faz mesmo o calculo total
(defun calcurar-n-partilhas-n1-n2 (caixas n1 n2)
	(cond
		((= n1 n2)
			(+ 
				(aux-partilhas-horizontal caixas n1 n2) 
				(aux-partilhas-vertical caixas n1 n2)
			)
		)
		(T
			(+ 
				(+ 
					(aux-partilhas-horizontal caixas n1 n2) 
					(aux-partilhas-vertical caixas n1 n2)
				) 
				(+ 
					(aux-partilhas-horizontal caixas n2 n1)
					(aux-partilhas-vertical caixas n2 n1)
				)
			)
		)
	)
)


;; helpers


(defun get-helper()

  '( ((0 0 1 0) (0 0 1 1)) ((0 0 1 0)(0 0 1 1)) )
)
(defun get-helper2()
  '( ((1 1 0 0) (1 1 0 0) (1 1 0 0)) ((1 1 0 0)(1 1 0 0) (1 1 0 0)) ((1 1 0 0)(1 1 0 0) (0 0 0 0))  )
)


(defun get-helper3()
  '(
    ( (0 0 1 0) (1 1 1 1) (0 1 0 1))
    ( (0 0 1 0) (1 0 0 1) (1 1 0 0))
    ( (0 0 1 0) (0 0 0 1) (1 0 0 0))
     )
)


(defun convert-top-bottom(linha)
	(cond
		( (null (second linha)) nil )
		( T
			(cons (mapcar 'list (first linha) (second linha))  (convert-top-bottom (rest linha)))
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
	(mapcar 'converter-aux (convert-top-bottom (car tabuleiro)) (matriz2d-transposta (convert-top-bottom (car (rest tabuleiro)))) )
)

(defun converter-aux(tops-bottoms lefts-rights)
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
	(cond
		((null matriz) nil)
		(T (cons (mapcar 'mapear-bool-binario (first matriz) ) (mapear-para-binario (rest matriz))))
	)
)


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


(defun estrutura-test()
	(mapear-para-binario (converter-tabuleiro (tabuleiro-c)))
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
        (+
			(heuristica2-aux-1-arco n-caixas-faltam n-caixas-faltar-1-arcos n-partilhas-1-1)
			(heuristica2-aux-2-arco (max 0 (- n-caixas-faltam n-caixas-faltar-1-arcos)) n-caixas-faltar-2-arcos n-partilhas-2-1 n-partilhas-2-2)
			(heuristica2-aux-3-arco (max 0 (- n-caixas-faltam n-caixas-faltar-1-arcos n-caixas-faltar-2-arcos )) n-caixas-faltar-3-arcos n-partilhas-3-1 n-partilhas-3-2 n-partilhas-3-3)
			(heuristica2-aux-4-arco (max 0 (- n-caixas-faltam n-caixas-faltar-1-arcos n-caixas-faltar-2-arcos n-caixas-faltar-3-arcos )) n-caixas-faltar-4-arcos n-partilhas-4-1 n-partilhas-4-2 n-partilhas-4-3 n-partilhas-4-4)
		)
	)
)


(defun heuristica2-aux-1-arco (n-caixas-faltam n-caixas-faltar-1-arco n-partilhas-1-1)
	(let*
		(
			(n-caixas-a-usar (min n-caixas-faltam n-caixas-faltar-1-arco))
			(n-partilhas-a-usar (min n-partilhas-1-1 n-caixas-a-usar))
		)
		(- n-caixas-a-usar (floor (/ n-partilhas-a-usar 2)))
    )
 )

(defun heuristica2-aux-2-arco (n-caixas-faltam n-caixas-faltar-2-arco n-partilhas-2-1 n-partilhas-2-2)
	(cond
	   ((= 0 n-caixas-faltam) 0)
	   (t
			(let*
				(
					(n-caixas-a-usar (min n-caixas-faltam n-caixas-faltar-2-arco))
					(n-partilhas-a-usar-2-2 (min n-partilhas-2-2 (- n-caixas-a-usar 1)))
				)
				(- (* 2 n-caixas-a-usar) n-partilhas-2-1  n-partilhas-a-usar-2-2 )
			 )

		)
	)
)

(defun heuristica2-aux-3-arco (n-caixas-faltam n-caixas-faltar-3-arco n-partilhas-3-1 n-partilhas-3-2 n-partilhas-3-3)
	(cond
		( (= 0 n-caixas-faltam) 0 )
		(t
			(let*
				(
					(n-caixas-a-usar (min n-caixas-faltam n-caixas-faltar-3-arco))
					(n-partilhas-a-usar-3-3 (min n-partilhas-3-3 (- n-caixas-a-usar 1)))
				)
				(- (* 3 n-caixas-a-usar) n-partilhas-3-1  n-partilhas-3-2 n-partilhas-a-usar-3-3 )
			)
		)
	)
)
  
(defun heuristica2-aux-4-arco (n-caixas-faltam n-caixas-faltar-4-arco n-partilhas-4-1 n-partilhas-4-2 n-partilhas-4-3 n-partilhas-4-4)
	(cond
		( (= 0 n-caixas-faltam) 0 )
		(t 	(let*
				(
					(n-caixas-a-usar (min n-caixas-faltam n-caixas-faltar-4-arco))
					(n-partilhas-a-usar-4-4 (min n-partilhas-4-4 (- n-caixas-a-usar 1)))
				)
				(- (* 4 n-caixas-a-usar) n-partilhas-4-1  n-partilhas-4-2 n-partilhas-4-3 n-partilhas-a-usar-4-4)
			)
		)
	)
)


(defun heuristica-2 (o)
  (lambda (no)
		(let
			( 
				( tabuleiro-convertido (mapear-para-binario (converter-tabuleiro (no-estado no))) ) 
			)
			(calcular-heuristica2
				o
				(n-caixas-a-faltar-x-arcos tabuleiro-convertido 0)
				(n-caixas-a-faltar-x-arcos tabuleiro-convertido 1)
				(n-caixas-a-faltar-x-arcos tabuleiro-convertido 2)
				(n-caixas-a-faltar-x-arcos tabuleiro-convertido 3)
				(n-caixas-a-faltar-x-arcos tabuleiro-convertido 4)
				(calcurar-n-partilhas-n1-n2 tabuleiro-convertido 4 4)
				(calcurar-n-partilhas-n1-n2 tabuleiro-convertido 4 3)
				(calcurar-n-partilhas-n1-n2 tabuleiro-convertido 4 2)
				(calcurar-n-partilhas-n1-n2 tabuleiro-convertido 4 1)
				(calcurar-n-partilhas-n1-n2 tabuleiro-convertido 3 3)
				(calcurar-n-partilhas-n1-n2 tabuleiro-convertido 3 2)
				(calcurar-n-partilhas-n1-n2 tabuleiro-convertido 3 1)
				(calcurar-n-partilhas-n1-n2 tabuleiro-convertido 2 2)
				(calcurar-n-partilhas-n1-n2 tabuleiro-convertido 2 1)
				(calcurar-n-partilhas-n1-n2 tabuleiro-convertido 1 1)
			)
		)
    )
)
