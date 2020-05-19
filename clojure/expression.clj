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

; ADDITION

; Парсер из нескольких слоев снизу вверх.
; base (Базовый слой)
; парсит префикс и возвращает его и хвост
(defn -return [value tail] {:value value :tail tail})       ; вернет пару
(def -valid? boolean)                                       ; корректный парсинг
(def -value :value)                                         ; префикс
(def -tail :tail)                                           ; хвост

; если результат корректный мы покажем его, иначе !
(defn _show [result]
  (if (-valid? result) (str "-> " (pr-str (-value result)) " | " (pr-str (apply str (-tail result))))
                       "!"))

; принимает парсер и некоторое колличество входов, печатаем то что получили и распарсили
(defn tabulate [parser inputs]
  (run! (fn [input] (printf "    %-10s %s\n" (pr-str input) (_show (parser input)))) inputs))

; парсер который парсит пустой префикс
(defn _empty [value] (partial -return value))

; парсит символы, p - предикат хороший ли символ
(defn _char [p]
  (fn [[c & cs]]
    (if (and c (p c)) (-return c cs))))

; преобразовывает результат и возвращает
(defn _map [f result]
  (if (-valid? result)
    (-return (f (-value result)) (-tail result))))

; комбинирует два парсера a и b приминяет один за другим, использовав ф-ю f к результату парсера b
(defn _combine [f a b]
  (fn [str]
    (let [ar ((force a) str)]
      (if (-valid? ar)
        (_map (partial f (-value ar))
              ((force b) (-tail ar)))))))

; пытается запустить первый парсер, если не удается, запускает второй
(defn _either [a b]
  (fn [str]
    (let [ar ((force a) str)]
      (if (-valid? ar) ar ((force b) str)))))

; парсит все целиком
(defn _parser [p]
  (fn [input]
    (-value ((_combine (fn [v _] v) p (_char #{\u0000})) (str input \u0000)))))

; combinators

; удобные версии, простейших ф-ий
; удобная записть например (+char "abc") парсит только a b c.
(defn +char [chars] (_char (set chars)))

; отрицание +char
(defn +char-not [chars] (_char (comp not (set chars))))

; мапит целиком парсер, то есть взять целиком, что выдал парсер и приминить какую - то функцию
(defn +map [f parser] (comp (partial _map f) parser))

(def +parser _parser)

; любой результат парсера заменяет на "ignore", но сначала парсер должен отработать!
; хорошее сочитание с +map отдельное удовольствие
(def +ignore (partial +map (constantly 'ignore)))

; игнорирует ignore, а в противном случае добавит в коллекцию coll значение value
(defn iconj [coll value]
  (if (= value 'ignore) coll (conj coll value)))

; построение последовательности из произвольного колличества парсеров
; здесь прикольная идея заключается в том, что парсер _empty вернет пустой вектор
; как раз для функции iconj, reduce c начала скушает _empty и понеслась...
; на каждом шаге в коллекции будет появляться новый распаршенный префикс
(defn +seq [& ps]
  (reduce (partial _combine iconj) (_empty []) ps))

; последовательно запустит, и функция f получит аргументы и применит функцию хорошенько
; ко всем элементам коллекции которая вернет +seq
(defn +seqf [f & ps] (+map (partial apply f) (apply +seq ps)))

; берет n - ую позицию из набора.
(defn +seqn [n & ps] (apply +seqf (fn [& vs] (nth vs n)) ps))

; приминяет первый из парсеров
(defn +or [p & ps]
  (reduce _either p ps))

; необязательный парсер: если премнился хорошо, иначе вернет ничего
(defn +opt [p]
  (+or p (_empty nil)))

; парсер, который повторяет столько раз сколько сможет - рекурсивный
; трюк в том, что в конце рекурсии мы получим список и будем в него пихать наши элементы и вернем его по итогу
(defn +star [p]
  (letfn [(rec [] (+or (+seqf cons p (delay (rec))) (_empty ())))] (rec)))

; 1 или более число раз в отличии от +star
(defn +plus [p] (+seqf cons p (+star p)))

; просто будет принимать один парсер и приминять к нему str
(defn +str [p] (+map (partial apply str) p))
; живые будут завидовать мертвым, едем дальше...

; json

(def *digit (+char "0123456789"))
(def *number (+map read-string (+str (+plus *digit))))
(def *string
  (+seqn 1 (+char "\"") (+str (+star (+char-not "\""))) (+char "\"")))
(def *space (+char " \t\n\r"))
(def *ws (+ignore (+star *space)))
(def *null (+seqf (constantly 'null) (+char "n") (+char "u") (+char "l") (+char "l")))
(def *all-chars (mapv char (range 32 128)))
(apply str *all-chars)
(def *letter (+char (apply str (filter #(Character/isLetter %) *all-chars))))
(def *identifier (+str (+seqf cons *letter (+star (+or *letter *digit)))))
(defn *seq [begin p end]
  (+seqn 1 (+char begin) (+opt (+seqf cons *ws p (+star (+seqn 1 *ws (+char ",") *ws p)))) *ws (+char end)))
(defn *array [p] (*seq "[" p "]"))
(defn *member [p] (+seq *identifier *ws (+ignore (+char ":")) *ws p))
(defn *object [p] (+map (partial reduce #(apply assoc %1 %2) {}) (*seq "{" (*member p) "}")))
(def json
  (let
    [*null (+seqf (constantly 'null) (+char "n") (+char "u") (+char "l") (+char "l"))
     *all-chars (mapv char (range 0 128))
     *letter (+char (apply str (filter #(Character/isLetter %) *all-chars)))
     *digit (+char (apply str (filter #(Character/isDigit %) *all-chars)))
     *space (+char (apply str (filter #(Character/isWhitespace %) *all-chars)))
     *ws (+ignore (+star *space))
     *number (+map read-string (+str (+plus *digit)))
     *identifier (+str (+seqf cons *letter (+star (+or *letter *digit))))
     *string (+seqn 1 (+char "\"") (+str (+star (+char-not "\""))) (+char "\""))]
    (letfn [(*seq [begin p end]
              (+seqn 1 (+char begin) (+opt (+seqf cons *ws p (+star (+seqn 1 *ws (+char ",") *ws p)))) *ws (+char end)))
            (*array [] (*seq "[" (delay (*value)) "]"))
            (*member [] (+seq *identifier *ws (+ignore (+char ":")) *ws (delay (*value))))
            (*object [] (+map (partial reduce #(apply assoc %1 %2) {}) (*seq "{" (*member) "}")))
            (*value [] (+or *null *number *string (*object) (*array)))]
      (+parser (+seqn 0 *ws (*value))))))

(defn parseObjectSuffix [expression]
  ())