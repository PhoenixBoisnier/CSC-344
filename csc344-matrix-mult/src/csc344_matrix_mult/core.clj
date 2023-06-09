(ns csc344-matrix-mult.core)

(defn is[q] ;;checks to see if an item is a variable
  (if (eval (instance? clojure.lang.Symbol q))
    true
    false
    )
  )

(defn cant[q] ;;checks to see if an individual item is a list or is a variable
  (if (eval (instance? clojure.lang.PersistentList q))
    true
    (is q)
    )
  )

(defn has[q] ;;will check to see if the input list has a variable or a list as an input
  (if (cant (first q))
    true
    (if (= () (rest q))
       false
       (has (rest q)))
    )
  )

(defn simpbol[q]
  (if (eval (instance? clojure.lang.PersistentList q))
    (simplify q)
    q
   )
  )

(defn simplify[q]
  (let [s (first q) a (first (rest q)) b (first (rest (rest q)))]
    (cond (= nil b) (list (first (rest a))) ;;the only time b will be nil is if the input list is (- (- x)), therefore, return the (- x)
          (= (cant a) (cant b)) (if (= (cant a) false)
                                  (eval (s a b)) ;;in the case that a and b are numbers, it is evaluated, shouldn't happen probably
                                  (list s a b) ;;in the case that both a and b are variables, it cannot be simplified, return '(s a b)
                                  )
          (= s '+) (if (cant a) ;;if a is the variable
                     (if (= b 0) ;;check to see if b is 0
                       (list a) ;;if b is 0, return a
                       (list s a b) ;;otherwise, cannot be simplified, return '(s a b)
                       )
                     (if (= a 0) ;;a is not the variable, so if a is 0
                       (list b) ;;return b
                       (list s a b) ;;otherwise, it cannot be simplified, return '(s a b)
                       )
                     )
          (= s '*) (if (cant a) ;;if a is the variable 
                     (if (= b 1) ;;if b is 1
                       (list a) ;;return a
                       (if (= b 0) ;;if b is 0
                         0 ;;return 0
                         (list s a b) ;;otherwise, a is the variable and b is neither 1 nor 0, cannot be simplified, return '(s a b)
                         )
                       )
                     (if (= a 1) ;;otherwise, b is the variable, if a is 1
                       (list b) ;;then return b
                       (if (= a 0) ;;otherwise, if a is 0
                         0 ;;then return 0
                         (list s a b) ;;otherwise, b is the variable and a is neither 1 nor 0, cannot be simplified, return '(s a b)
                         )
                       )
                     )
          :else q ;;this is in the case that the other signs occur
          )
    )
  )

(defn transform[abcd xy]
     (let [x (simpbol (first xy))
           y (simpbol (first (rest xy)))
           a (simpbol (first (first abcd)))
           b (simpbol (first (rest (first abcd))))
           c (simpbol (first (first (rest abcd))))
           d (simpbol (first (rest (first (rest abcd)))))
           termAX (if (has (list a x))
                       (simplify (list '* a x))
                       (* a x)
                       )
           termBY (if (has (list b y))
                    (simplify (list ('* b y)))
                    (* b y)
                    )
           termCX (if (has (list c x))
                    (simplify (list '* c x))
                    (* c x)
                    )
           termDY (if (has (list d y))
                    (simplify (list '* d y))
                    (* d y)
                    )
           termAXBY (if (has (list termAX termBY))
                      (simplify (list '+ termAX termBY))
                      (+ termAX termBY)
                      )
           termCXDY (if (has (list termCX termDY))
                      (simplify (list '+ termCX termDY))
                      (+ termCX termDY)
                      )
           ]
       (conj [] termAXBY termCXDY)
      )
     )
