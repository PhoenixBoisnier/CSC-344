(ns csc344-clojuremicro.core)

;(defn apply-all-fns[x y z]
;          (if (first x)
;          (list (eval (list (first x) y z)) (apply-all-fns (rest x) y z))))

(defn real-apply-all-fns[x y z]
          (if (first x)
          (list (eval (list (first x) y z)) (eval (apply-all-fns (rest x) y z)))))

(defn apply-all-fns[x y z]
          (vec (flatten (real-apply-all-fns x y z))))
