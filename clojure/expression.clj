(definterface Expression
  (diff [str])
  (evaluate [values]))

(declare zero Divide Multiply Add Subtract Negate)

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

(deftype A [f s]
  Object
  (toString [this] (str "(" "+ " (.toString f) " " (.toString s) ")"))
  Expression
  (evaluate [this m] (+ (.evaluate f m) (.evaluate s m)))
  (diff [this str] (Add (.diff f str) (.diff s str))))
(deftype S [f s]
  Object
  (toString [this] (str "(" "- " (.toString f) " " (.toString s) ")"))
  Expression
  (evaluate [this m] (- (.evaluate f m) (.evaluate s m)))
  (diff [this str] (Subtract (.diff f str) (.diff s str))))
(deftype M [f s]
  Object
  (toString [this] (str "(" "* " (.toString f) " " (.toString s) ")"))
  Expression
  (evaluate [this m] (* (.evaluate f m) (.evaluate s m)))
  (diff [this str] (Add (Multiply (.diff f str) s) (Multiply f (.diff s str)))))
(deftype D [f s]
  Object
  (toString [this] (str "(" "/ " (.toString f) " " (.toString s) ")"))
  Expression
  (evaluate [this m] (/ (double (.evaluate f m)) (double (.evaluate s m))))
  (diff [this str] (Divide (Subtract (Multiply (.diff f str) s) (Multiply f (.diff s str))) (Multiply s s))))
(deftype N [one]
  Object
  (toString [this] (str "(" "negate " (.toString one) ")"))
  Expression
  (evaluate [this m] (- (.evaluate one m)))
  (diff [this str] (Negate (.diff one str))))
(defn Add [a b] (new A a b))
(defn Subtract [a b] (new S a b))
(defn Multiply [a b] (new M a b))
(defn Divide [a b] (new D a b))
(defn Negate [one] (new N one))
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
                   })
(defn parse [expression] (cond (number? expression) (Constant expression)
                               (isVariable expression) (Variable (name expression))
                               :else (apply (get mapOperation (name (first expression))) (mapv parse (pop expression)))))
(defn parseObject [string] (parse (read-string string)))