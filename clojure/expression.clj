(definterface Expression
  (diff [])
  (evaluate [values]))
(declare zero Divide Multiply Add Subtract)
(deftype Const [c]
  Object
  (toString [this] (str c))
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
  (diff [this str] if (= str argName) one zero))
(defn Constant [val] (new Const val))
(deftype A [f s]
  Object
  (toString [this] (str "( " "+" (.toString f) " " (.toString s) " )"))
  Expression
  (evaluate [this m] (+ (.evaluate f m) (.evaluate s m)))
  (diff [this] (Add (.diff f) (.diff s))))
(deftype S [f s]
  Object
  (toString [this] (str "( " "-" (.toString f) " " (.toString s) " )"))
  Expression
  (evaluate [this m] (- (.evaluate f m) (.evaluate s m)))
  (diff [this] (Subtract (.diff f) (.diff s))))
(deftype M [f s]
  Object
  (toString [this] (str "( " "*" (.toString f) " " (.toString s) " )"))
  Expression
  (evaluate [this m] (* (.evaluate f m) (.evaluate s m)))
  (diff [this] (Add (Multiply (.diff f) s) (Multiply f (.diff s)))))
(deftype D [f s]
  Object
  (toString [this] (str "( " "/" (.toString f) " " (.toString s) " )"))
  Expression
  (evaluate [this m] (/ (.evaluate f m) (.evaluate s m)))
  (diff [this] (Divide (Subtract (Multiply (.diff f) s) (Multiply f (.diff s))) (Multiply s s))))

(defn Variable [argName] (new V argName))
(defn Add [a b] (new A a b))
(defn Subtract [a b] (new S a b))
(defn Multiply [a b] (new M a b))
(defn Divide [a b] (new D a b))
(defn evaluate [expr m]
  (.evaluate expr m))
(defn diff [expr]
  (.diff expr))
(defn toString [expr]
  (.toString expr))
(def isVariable (fn [symbol] (if (or (= symbol 'x) (= symbol 'y) (= symbol 'z)) true false)))
(def mapOperation {
                        "+"      Add
                        "-"      Subtract
                        "/"      Divide
                        "*"      Multiply
                        })
(defn parse [expression] (cond (number? expression) (Constant expression)
                               (isVariable expression) (new Variable (name expression))
                               :else (apply (get mapOperation (name (first expression))) (mapv parse (pop expression)))))
(defn parseObject [string] (parse (read-string string)))