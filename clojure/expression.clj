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
  (diff [this str] (ownDiff str args)))

(defn p-operand [oper args str] (apply oper (mapv (fn [x] (.diff x str)) args)))

(defn Add [& args]
  (new Operation + (fn [str args] (p-operand Add args str)) "+" args))

(defn Subtract [& args]
  (new Operation - (fn [str args] (p-operand Subtract args str)) "-" args))

(defn Multiply [& args]
  (new Operation * (fn [str args] (reduce (fn [f s] (Add (Multiply (.diff f str) s) (Multiply f (.diff s str)))) args)) "*" args))

(defn Divide [& args]
  (new Operation (fn [f s] (/ (double f) (double s))) (fn [str [f s]] (Divide (Subtract (Multiply (.diff f str) s) (Multiply f (.diff s str))) (Multiply s s))) "/" args))

(defn Negate [& args]
  (new Operation - (fn [str args] (p-operand Negate args str)) "negate" args))

(defn Exp [& args]
  (new Operation (fn [x] (Math/exp x)) (fn [str args] (Multiply (Exp (first args)) (.diff (first args) str))) "exp" args))

(defn Ln [& args]
  (new Operation (fn [x] (Math/log (Math/abs x))) (fn [str args] (Multiply (Divide one (first args)) (.diff (first args) str))) "ln" args))

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
                   "exp"    Exp
                   "ln"     Ln
                   })
(defn parse [expression] (cond (number? expression) (Constant expression)
                               (isVariable expression) (Variable (name expression))
                               :else (apply (get mapOperation (name (first expression))) (mapv parse (pop expression)))))

(defn parseObject [string] (parse (read-string string)))