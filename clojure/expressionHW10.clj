(defn constant [value] (fn [m] value))
(defn variable [varName] (fn [m] (get m varName)))
(defn operation [f] (fn [& args] (fn [m] (apply f (mapv (fn [x] (x m)) args)))))
(def negate (operation -))
(def multiply (operation *))
(def add (operation +))
(def exp (operation (fn [x] (Math/exp x))))
(def ln (operation (fn [x] (Math/log (Math/abs x)))))
(def subtract (operation -))
(def divide (operation (fn [a, b] (/ (double a) (double b)))))
(def isVariable (fn [symbol] (if (or (= symbol 'x) (= symbol 'y) (= symbol 'z)) true false)))
(defn parseFunction [string]
  (letfn [(parse [expression]
            (let [mapOperation {
                                "+"      add
                                "-"      subtract
                                "/"      divide
                                "*"      multiply
                                "negate" negate
                                "exp"    exp
                                "ln"     ln
                                }] (cond (number? expression) (constant expression)
                                         (isVariable expression) (variable (name expression))
                                         :else (apply (get mapOperation (name (first expression))) (mapv parse (pop expression))))))]
    (parse (read-string string))))