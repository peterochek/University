(defn sizes? [obj]
  (apply == (mapv count obj)))

(defn int_v? [obj]
  (and (vector? obj)
       (every? number? obj)))

(defn inverse [f]
  (fn [a b] (f b a)))

(defn dim_sizes? [& obj]
  (and
   (every? vector? obj)
   (apply == (mapv count obj))
   (every? (partial mapv dim_sizes?) obj)))

(defn elw [pred]
  #(fn [& obj]
     {:pre [(pred obj) (sizes? obj)]}
     (apply mapv % obj)))

(def elw_v
  (elw #(every? int_v? %)))

(def v+ (elw_v +))
(def v- (elw_v -))
(def v* (elw_v *))
(def vd (elw_v /))

(defn scalar [& v]
  {:pre [(every? int_v? v)]}
  (reduce + (reduce v* v)))

(defn vect [& v]
  {:pre [(every? int_v? v) (every? (partial = 3) (mapv count v))]}
  (reduce (fn [a b] (vector
                     (- (* (nth a 1) (nth b 2)) (* (nth b 1) (nth a 2)))
                     (- (* (nth b 0) (nth a 2)) (* (nth a 0) (nth b 2)))
                     (- (* (nth a 0) (nth b 1)) (* (nth b 0) (nth a 1))))) v))

(defn v*s [v & s]
  {:pre [(int_v? v) (every? number? s)]}
  (mapv (partial * (reduce * s)) v))

(defn int_m? [obj]
  (and (vector? obj)
       (every? int_v? obj)
       (sizes? obj)))

(def elw_m
  (elw #(every? int_m? %)))

(def m+ (elw_m v+))
(def m- (elw_m v-))
(def m* (elw_m v*))
(def md (elw_m vd))

(defn m*s [m & s]
  {:pre [(int_m? m) (every? number? s)]}
  (mapv (partial (inverse v*s) (reduce * s)) m))

(defn m*v [m v]
  {:pre [(int_m? m) (int_v? v)]}
  (mapv (partial (inverse scalar) v) m))

(defn transpose [m]
  {:pre [(int_m? m)]}
  (apply mapv vector m))

(defn m*m [& m]
  {:pre [(every? int_m? m)]}
  (reduce (fn [a b]
            {:pre [(sizes? [(first a) b])]}
            (transpose (mapv
                        (partial m*v a)
                        (transpose b)))) m))
(defn int_x? [obj]
  (or
   (number? obj)
   (int_v? obj)
   (and
    (= (range (count obj) 0 -1) (mapv count obj))
    (every? int_x? obj))))

(defn op_x [f] (fn [& x]
                 (if (vector? (first x))
                   (apply (partial mapv (op_x f)) x)
                   (apply f x))))

(defn elw_x [f]
  ((elw #(and
          (every? int_x? %)
          (apply dim_sizes? %)))
   (op_x f)))

(def x+ (elw_x +))
(def x- (elw_x -))
(def x* (elw_x *))
(def xd (elw_x /))