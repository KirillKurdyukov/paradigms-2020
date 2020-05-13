(definterface Expression
  (diff [str])
  (evaluate [values]))

(declare zero)

(deftype Const [c]
  Object
  (toString [this] (format "%.1f" c))
  Expression
  (evaluate [this values] c)
  (diff [this str] zero))

(def zero (new Const 0))
(def one (new Const 1))

(deftype V [argName]
  Object
  (toString [this] argName)
  Expression
  (evaluate [this m] (get m argName))
  (diff [this str] (if (= str argName) one zero)))

(defn Constant [val] (new Const val))

(defn Variable [argName] (new V argName))

(deftype Operation [operation ownDiff stringOperation args]
  Object
  (toString [this] (str "(" stringOperation " " (clojure.string/join " " args) ")"))
  Expression
  (evaluate [this m] (apply operation (mapv (fn [x] (.evaluate x m)) args)))
  (diff [this str]  (ownDiff str args)))

(defn Add [a b]
  (new Operation + (fn [str args] (apply Add (mapv (fn [x] (.diff x str)) args))) "+" [a b]))

(defn Subtract [a b]
  (new Operation - (fn [str args] (apply Subtract (mapv (fn [x] (.diff x str)) args))) "-" [a b]))

(defn Multiply [a b]
  (new Operation * (fn [str [f s]] (Add (Multiply (.diff f str) s) (Multiply f (.diff s str)))) "*" [a b]))

(defn Divide [a b]
  (new Operation (fn [f s] (/ (double f) (double s))) (fn [str [f s]] (Divide (Subtract (Multiply (.diff f str) s) (Multiply f (.diff s str))) (Multiply s s))) "/" [a b]))

(defn Negate [one]
  (new Operation - (fn [str args] (apply Negate (mapv (fn [x] (.diff x str)) args))) "negate" [one]))

(defn Negate [one]
  (new Operation - (fn [str args] (apply Negate (mapv (fn [x] (.diff x str)) args))) "negate" [one]))

(defn Exp [one]
  (new Operation (fn [x] (Math/exp x)) (fn [str & args]  (Multiply (Exp args) (.diff args str))) "exp" [one]))

(defn Ln [one]
  (new Operation (fn [x] (Math/log (Math/abs x))) (fn [str & args] (Multiply (Divide (Constant 1) args) (.diff args str))) "ln" [one]))

(defn evaluate [expr m]
  (.evaluate expr m))
(defn diff [expr str]
  (.diff expr str))
(defn toString [expr]
  (.toString expr))

(def isVariable (fn [symbol] (if (or (= symbol 'x) (= symbol 'y) (= symbol 'z)) true false)))

(def mapOperation {
                   "+"      Add
                   "-"      Subtract
                   "/"      Divide
                   "*"      Multiply
                   "negate" Negate
                   "exp" Exp
                   "ln" Ln
                   })
(defn parse [expression] (cond (number? expression) (Constant expression)
                               (isVariable expression) (Variable (name expression))
                               :else (apply (get mapOperation (name (first expression))) (mapv parse (pop expression)))))

(defn parseObject [string] (parse (read-string string)))