(def constant constantly)
(defn variable [name] #(% name))

(defn fold [func]
  (fn [& args]
    (fn [vars]
      (apply func (mapv #(% vars) args)))))

(def add (fold +))
(def subtract (fold -))
(def multiply (fold *))
(defn divideLocal
  ([x] (/ 1.0 x))
  ([x & args] (/ (double x) (apply * args))))
(def divide (fold divideLocal))
(def negate subtract)

(defn meanLocal [& args]
  (/ (apply + args) (count args)))
(def mean (fold meanLocal))

(defn varnLocal [& args]
  (let [meanVal (apply meanLocal args)]
    (/ (apply + (mapv #(* % %) (mapv #(- % meanVal) args)))
       (count args))))
(def varn (fold varnLocal))

(def funcOperators
  {'+      add
   '-      subtract
   '*      multiply
   '/      divide
   'negate negate
   'mean   mean
   'varn   varn})

(defn buildParse [operators constantLocal variableLocal]
  (fn innerParse [string]
    (cond
      (symbol? string) (variableLocal (str string))
      (list? string) (apply (operators (first string)) (map innerParse (rest string)))
      (number? string) (constantLocal string))))

;; fixed parse
(defn parse [operators constantLocal variableLocal]
  (comp (buildParse operators constantLocal variableLocal) read-string))

(def parseFunction (parse funcOperators constant variable))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(definterface IExpression
  (evaluate [vars])
  (diff [diffVar])
  (toString []))

(defn toString [expression] (.toString expression))
(defn diff [expression diffVar] (.diff expression diffVar))
(defn evaluate [expression vars] (.evaluate expression vars))

(declare ZERO)

(deftype Const [num]
  IExpression
  (evaluate [_ vars] num)
  (diff [_ diffVar] ZERO)
  (toString [_] (format "%.1f" (double num))))

(defn Constant [num] (Const. num))

(def ZERO (Constant 0))
(def ONE (Constant 1))
(def TWO (Constant 2))

(deftype Var [name]
  IExpression
  (evaluate [_ vars] (vars name))
  (diff [_ diffVar] (if (= name diffVar) ONE ZERO))
  (toString [_] name))

(defn Variable [name] (Var. name))

(declare Add Subtract Multiply Divide Negate Mean Varn)

(defn diffArgs [func]
  (fn [var & args] (func (mapv #(.diff % var) args) args)))

(defn newName [name]
  (symbol (str "Class" name)))

;; fixed memory
(defmacro AbstractOperation [name func string diff]
  (let [className (newName name)]
    `(do
       (deftype ~className [args#]
         IExpression
         (evaluate [_ vars#] (apply ~func (mapv #(.evaluate % vars#) args#)))
         (diff [_ diffVar#] (apply (diffArgs ~diff) diffVar# (vec args#)))
         (toString [_] (str "(" ~string " " (clojure.string/join " " args#) ")")))
       (defn ~name [& args#] (new ~className args#)))))

(AbstractOperation Add
                   +
                   "+"
                   (fn [diffArgs args] (apply Add diffArgs)))
(AbstractOperation Subtract
                   -
                   "-"
                   (fn [diffArgs args] (apply Subtract diffArgs)))

;; fixed square asymptotics -> linear
; :NOTE: Деление на 0
(defn traverse [diffArgs args]
  (let [nonDiffMultiply (apply Multiply args)]
    (apply Add (map-indexed (fn [idx itm]
                              (Multiply (Divide nonDiffMultiply (nth args idx)) itm)) diffArgs))))

;; removed unused in multiply
(AbstractOperation Multiply
                   *
                   "*"
                   traverse)
(AbstractOperation Negate
                   -
                   "negate"
                   (fn [diffArgs args] (Negate (first diffArgs))))

;; added common subexpressions (possible full binary 2 ^ n tree)
(AbstractOperation Divide
                   divideLocal
                   "/"
                   (fn [diffArgs args] (if
                                        (= (count diffArgs) 1)
                                         (Divide (Negate (first diffArgs))
                                                 (Multiply (first args) (first args)))
                                         (let [denominator (apply Multiply (rest args))]
                                           (Divide (Add (Multiply TWO (first diffArgs) denominator)
                                                        (Negate (traverse diffArgs args)))
                                                   (Multiply denominator denominator))))))

(AbstractOperation Mean
                   meanLocal
                   "mean"
                   (fn [diffArgs args] (Divide
                                        (apply Add diffArgs)
                                        (Constant (count diffArgs)))))
(AbstractOperation Varn
                   varnLocal
                   "varn"
                   (fn [diffArgs args]
                     (Multiply TWO (Subtract
                                    (Multiply (apply Mean (map Multiply args diffArgs)))
                                    (Multiply (apply Mean diffArgs) (apply Mean args))))))

(def objOperators
  {'+ Add
   '- Subtract
   '* Multiply
   '/ Divide
   'negate Negate
   'mean Mean
   'varn Varn})

(def parseObject (parse objOperators Constant Variable))
