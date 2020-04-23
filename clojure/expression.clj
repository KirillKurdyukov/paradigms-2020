(defn constant [value] (fn [m] value))
(defn variable [varName] (fn [m] (get m varName)))
(defn operation [f] (fn [& args] (fn [m] (apply f (mapv (fn [x] (x m)) args)))))
(def negate (operation -))
(def multiply (operation *))
(def add (operation +))
(def subtract (operation -))
(def divide (operation (fn [a, b] (/ (double a) (double b)))))
(def isVariable (fn [symbol] (if (or (= symbol 'x) (= symbol 'y) (= symbol 'z)) true false)))
(def mapOperation {
                   "+"      add
                   "-"      subtract
                   "/"      divide
                   "*"      multiply
                   "negate" negate
                   })
(defn parse [expression] (cond (number? expression) (constant expression)
                                 (isVariable expression) (variable (name expression))
                               :else (apply (get mapOperation (name (first expression))) (mapv parse (pop expression)))))
(defn parseFunction [string]
  (let [expression (read-string string)]
    (parse expression)))