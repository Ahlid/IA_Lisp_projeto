(defun h-numero-partilhas-horizonta-duas-linhas-quadrados(linha1 linha2 n1 n2)

  (apply '+

         (mapcar
          (lambda(x y)
            (cond
             ((and (= 1 (verificar-n-arcos-faltam x n1)) (= 1 (verificar-n-arcos-faltam y n2)))
              (cond
               ((and (= (second x) (first y)) (= 1 (second x))) 1)
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
   ((and (= 1 (third (first linha))) (= (third (first linha)) (fourth (second linha)) ) )

    (cond
     ((and (= 1 (verificar-n-arcos-faltam (first linha) n1)) (= 1 (verificar-n-arcos-faltam (second linha) n2)))
      (+ 1 (h-numero-partilhas-vertical (cdr linha) n1 n2)) )
     (T
      (h-numero-partilhas-vertical (cdr linha) n1 n2)))
    )
   (T (h-numero-partilhas-vertical (cdr linha) n1 n2)))

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


(defun calcurar-n-partilhas-m-p (caixas n1 n2)

(+ (aux-partilhas-horizontal caixas n1 n2) (aux-partilhas-vertical caixas n1 n2))

)



(defun get-helper()

'( ((0 0 1 0) (0 0 1 1)) ((0 0 1 0)(0 0 1 1)) )
)
(defun get-helper2()

'( ((1 1 0 0) (1 1 0 0) (1 1 0 0)) ((1 1 0 0)(1 1 0 0) (1 1 0 0)) ((1 1 0 0)(1 1 0 0) (0 0 0 0))  )
)



(defun aux-partilhas-vertical(caixas n1 n2)

  (cond
   ((null caixas) 0)
   (T (+ (h-numero-partilhas-vertical (first caixas) n1 n2) (aux-partilha-vertical (rest caixas) n1 n2) ))
   )
  )

(defun aux-partilhas-horizontal(caixas n1 n2)
  (cond
   ((null caixas) 0)
   ((null (second caixas)) 0)
   (T (+ (h-numero-partilhas-horizonta-duas-linhas-quadrados (first caixas) (second caixas) n1 n2 ) (aux-partilhas-horizontal (rest caixas) n1 n2)))

   )

  )
