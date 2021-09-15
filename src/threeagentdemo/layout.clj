(ns threeagentdemo.layout)

(defmacro set-node [nd prop & args]
  (let [prop (symbol (str ".set" prop))]
    `(doto ~nd (~prop ~@(map (fn [arg] `(threeagentdemo.layout/as-param ~arg)) args)))))

(defmacro get-node [nd prop & args]
  (let [prop (symbol (str ".get" prop))]
    `(doto ~nd (~prop ~@(map (fn [arg] `(threeagentdemo.layout/as-param ~arg)) args)))))

(defmacro defproperty [pname & args]
  (let [getter (str "get" (name pname))
        prop   (symbol (name pname))
        getsym (symbol getter)
        setter (str "set" (name pname))
        setsym (symbol setter)
        nd (gensym "node")]
    `(do (defn ~getsym [~nd ~@args]
           (get-node ~nd ~prop ~@args))
         (defn ~setsym [~nd ~@args]
           (set-node ~nd ~prop ~@args))
         (swap! threeagentdemo.layout/node-properties assoc ~pname ~setsym)
         nil)))

(defmacro defproperties [kvps]
  `(do ~@(for [[k v] kvps]
           `(defproperty ~k ~@v))))

(defmacro do-node [nd & args]
  `(doto ~nd
     ~@(for [[m & xs] args]
         `(~m ~@(map (fn [arg] `(threeagentdemo.layout/as-param ~arg)) xs)))))
