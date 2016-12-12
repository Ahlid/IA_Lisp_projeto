(defun h-numero-partilhas-horizonta-duas-linhas-quadrados(linha1 linha2)


  (mapcar
   (lambda(x y)
     (cond
      ((and (= (second x) (first y)) (= 1 (second x))) 1)
      (T 0)
      )
     )

   '((0 0 0 0) (0 0 0 0)) '((0 1 0 0)(0 0 0 0)) )

  )

(defun h-linhas-feitas-quadrado (quadrado)

(apply '+ quadrado)
)



(defun h-aux-arcos-horizontal(quadrados )


)


(defun n-caixas-a-faltar-x-arcos(caixas n)
  (apply 
   '+
   (mapcar
    (lambda (x)
      (apply '+
             ( mapcar
               (lambda (z)
                 (verificar-n-arcos-faltam z n))
               x )
             )
      )
    caixas)
   )

  )

(defun verificar-n-arcos-faltam (caixa n)
  (cond
   ((= n (- 4 (apply '+ caixa))) 1)
   (t 0)
   )
  )
