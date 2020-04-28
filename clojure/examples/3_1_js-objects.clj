(chapter "JavaScript-like objects")

(example "Maps as objects"
         (def point {:x 10 :y 20})
         point
         (point :x))

(section "Prototypes")
(example "Object with prototype"
         (def shifted-point {:prototype point :dx 1 :dy 2 :y 100}))
(example "Get with prototype"
         (defn proto-get [obj key]
           (cond
             (contains? obj key) (obj key)
             (contains? obj :prototype) (proto-get (obj :prototype) key)
             :else nil)))
(example "Own property"
         (proto-get shifted-point :dx))
(example "Inherited property"
         (proto-get shifted-point :x))
(example "Overridden property"
         (proto-get shifted-point :y))
(example "Missing property"
         (proto-get shifted-point :z))

(section "Methods ")
(example "Points with methods"
         (def point
           {:x 10
            :y 20
            :getX (fn [this] (proto-get this :x))
            :getY (fn [this] (proto-get this :y))
            :add (fn [this a b] (+ a b))
            })
         (def shifted-point
           {:prototype point
            :dx 1
            :dy 2
            :getX (fn [this] (+ (proto-get this :x) (proto-get this :dx)))
            :getY (fn [this] (+ (proto-get this :y) (proto-get this :dy)))
            }))
(example "Call method"
         (defn proto-call [this key & args]
           (apply (proto-get this key) this args)))

(example "Own method"
         (proto-call point :getX))
(example "Overridden method"
         (proto-call shifted-point :getX))
(example "Multi-argument method"
         (proto-call point :add 2 3)
         (proto-call shifted-point :add 2 3))

(section "Syntactic sugar ")
(example "Field declaration"
         (defn field [key]
           (fn [this] (proto-get this key))))
(example "Method declaration"
         (defn method [key]
           (fn [this & args] (apply proto-call this key args))))
(example "Fields"
         (def _x (field :x))
         (def _y (field :y))
         (def _dx (field :dx))
         (def _dy (field :dy)))
(example "Methods"
         (def _getX (method :getX))
         (def _getY (method :getY))
         (def _add (method :add)))
(example "Points"
         (def point
           {:x 10
            :y 20
            :getX _x
            :getY _y
            :add (fn [this a b] (+ a b))
            })
         (def shifted-point
           {:prototype point
            :dx 1
            :dy 2
            :getX (fn [this] (+ (_x this) (_dx this)))
            :getY (fn [this] (+ (_y this) (_dy this)))
            }))
(example "Fields usage"
         (_x point)
         (_x shifted-point)
         (_dx shifted-point))
(example "Methods usage"
         (_getX point)
         (_getX shifted-point)
         (_add shifted-point 2 3))

(section "Constructors")
(example "Constructor declaration"
         (defn constructor [ctor prototype]
           (fn [& args] (apply ctor {:prototype prototype} args))))
(example "Supertype"
         (def _Point)
         (def _distance (method :distance))
         (def _length (method :length))
         (def _sub (method :sub))
         (def PointPrototype
           {:getX _x
            :getY _y
            :sub (fn [this that] (_Point (- (_getX this) (_getX that))
                                         (- (_getY this) (_getY that))))
            :length (fn [this] (let [square #(* % %)] (Math/sqrt (+ (square (_getX this)) (square (_getY this))))))
            :distance (fn [this that] (_length (_sub this that)))
            })
         (defn Point [this x y]
           (assoc this
             :x x
             :y y))
         (def _Point (constructor Point PointPrototype)))
(example "Subtype"
         (def ShiftedPointPrototype
           (assoc PointPrototype
             :getX (fn [this] (+ (_x this) (_dx this)))
             :getY (fn [this] (+ (_y this) (_dy this)))))
         (defn ShiftedPoint [this x y dx dy]
           (assoc (Point this x y)
             :dx dx
             :dy dy
             ))
         (def _ShiftedPoint (constructor ShiftedPoint ShiftedPointPrototype)))

(example "Instances"
         (def point (_Point 10 20))
         (def shifted-point (_ShiftedPoint 10 20 1 2))
         (_getX point)
         (_getX shifted-point)
         (_x point)
         (_x shifted-point)
         (_dx shifted-point)
         (_length (_Point 4 3))
         (_sub (_Point -1 -2) (_Point 2 2))
         (_distance (_Point -1 -2) (_Point 2 2)))
