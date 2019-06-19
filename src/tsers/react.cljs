(ns tsers.react
  (:require ["react" :as react]
            ["react-dom" :as react-dom]
            [clojure.string :as string]))

(def ^:private fn-cache-prop
  "$TsersComponentCache")

(defn- throw-ex [msg]
  (throw (js/Error. msg)))

(def ^:private react-create-element
  react/createElement)

(def ^:private react-render
  react-dom/render)

(def ^:private react-memo
  react/memo)

(def ^:private react-use-state
  react/useState)

(declare as-elem)
(declare as-js-props)

(def ^:private tag-cache
  (js/Map.))

(defn- create-element [type props children]
  (let [n (count children)]
    (cond
      (identical? 0 n)
      (react-create-element type props)
      (identical? 1 n)
      (let [[c1 c2] children]
        (react-create-element type props c1))
      (identical? 2 n)
      (let [[c1 c2] children]
        (react-create-element type props c1 c2))
      (identical? 3 n)
      (let [[c1 c2 c3] children]
        (react-create-element type props c1 c2 c3))
      (identical? 4 n)
      (let [[c1 c2 c3 c4] children]
        (react-create-element type props c1 c2 c3 c4))
      (identical? 5 n)
      (let [[c1 c2 c3 c4 c5] children]
        (react-create-element type props c1 c2 c3 c4 c5))
      (identical? 6 n)
      (let [[c1 c2 c3 c4 c5 c6] children]
        (react-create-element type props c1 c2 c3 c4 c5 c6))
      (identical? 7 n)
      (let [[c1 c2 c3 c4 c5 c6 c7] children]
        (react-create-element type props c1 c2 c3 c4 c5 c6 c7))
      (identical? 8 n)
      (let [[c1 c2 c3 c4 c5 c6 c7 c8] children]
        (react-create-element type props c1 c2 c3 c4 c5 c6 c7 c8))
      (identical? 9 n)
      (let [[c1 c2 c3 c4 c5 c6 c7 c8 c9] children]
        (react-create-element type props c1 c2 c3 c4 c5 c6 c7 c8 c9))
      (identical? 10 n)
      (let [[c1 c2 c3 c4 c5 c6 c7 c8 c9 c10] children]
        (react-create-element type props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10))
      (identical? 11 n)
      (let [[c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11] children]
        (react-create-element type props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11))
      (identical? 12 n)
      (let [[c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12] children]
        (react-create-element type props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12))
      true (apply create-element (cons type (cons props children))))))

(defn- fq-name [kw]
  (str (if-let [ns (namespace kw)]
         (str ns "/"))
       (name kw)))

(defn- as-class-list [classes]
  (if-not (nil? classes)
    (string/join " " classes)))

(defn- assert-tag-valid-at-mode-break [tag start current]
  (if (identical? start current)
    (throw-ex (str "Invalid hiccup tag - no tag name specified: " tag))))

(defn- assert-no-prev-id [tag prev-id]
  (if-not (nil? prev-id)
    (throw-ex (str "Invalid hiccup tag - id declared twice: " tag))))

(defn- parse-tag [tag]
  (loop [length  (.-length tag)
         start   0
         index   0
         name    nil
         classes nil
         id      nil
         mode    :name]
    (if (identical? index length)
      (case mode
        :name [tag nil nil]
        :id [name (subs tag start) (as-class-list classes)]
        :class [name id (as-class-list (cons (subs tag start) classes))]
        nil)
      (let [ch (unchecked-get tag index)
            i' (inc index)]
        (if (identical? ch ".")
          (do (assert-tag-valid-at-mode-break tag start index)
              (case mode
                :name (recur length i' i' (subs tag 0 index) classes id :class)
                :id (recur length i' i' name classes (subs tag start index) :class)
                :class (recur length i' i' name (cons (subs tag start index) classes) id :class)
                nil))
          (if (identical? ch "#")
            (do (assert-no-prev-id tag id)
                (assert-tag-valid-at-mode-break tag start index)
                (case mode
                  :name (recur length i' i' (subs tag 0 index) classes id :id)
                  :class (recur length i' i' name (cons (subs tag start index) classes) id :id)
                  :id (assert-no-prev-id tag tag)))
            (recur length start i' name classes id mode)))))))

(defn- parse-tag-cached [tag]
  (if-let [cached-result (.get tag-cache tag)]
    cached-result
    (let [res (parse-tag tag)]
      (.set tag-cache res)
      res)))

(defn- camelize [s]
  (let [res (.split s "-")
        n   (.-length res)
        buf (js/Array. n)]
    (unchecked-set buf 0 (unchecked-get res 0))
    (loop [i 1]
      (when (< i n)
        (let [part      (unchecked-get res i)
              camelized (str (.toUpperCase (.charAt part 0)) (subs part 1))]
          (unchecked-set buf i camelized))))
    (.join buf "")))

(declare as-js-props)

(defn- as-js-prop-value [x]
  (cond
    (or (fn? x)
        (boolean? x)
        (string? x)
        (number? x)
        (nil? x)
        (undefined? x)) x
    (keyword? x) (fq-name x)
    (map? x) (as-js-props x)
    (coll? x) (let [val #js []]
                (doseq [v x]
                  (.push val (as-js-prop-value v)))
                val)
    (ifn? x) (fn [& args] (apply x args))
    :else x))

(defn- as-js-props [props]
  (if (nil? props)
    #js {}
    (let [js-props #js {}]
      (doseq [[k v] props]
        (let [p (cond
                  (= :class k) "className"
                  (keyword? k) (camelize (name k))
                  (string? k) k
                  :else (throw-ex (str "Invalid intrinsic property name" (pr-str k))))]
          (unchecked-set js-props p (as-js-prop-value v))))
      js-props)))

(comment
  (parse-tag-cached (name :div.foobar))

  (def x #js {:foo 123 :bar 123})

  (doseq [[k v] x]
    (println k v))

  (as-js-props {:foo 123 :bar 123 :lol [:bal {:foo "123"}]})
  (camelize "on-close" 123)

  (as-elem [:div.foo])

  '-)

(defn- assert-no-id-from-props [js-props]
  (if (some? (unchecked-get js-props "id"))
    (throw-ex "Intrinsic element id declared in both tag name and props")))

(defn- as-intrinsic-elem [tag props children]
  (let [[tag-name id class-names] (parse-tag-cached (name tag))
        js-props (as-js-props props)]
    (when (some? id)
      (assert-no-id-from-props props)
      (unchecked-set js-props "id" id))
    (when (some? class-names)
      (if-let [class-names-from-props (unchecked-get js-props "className")]
        (unchecked-set js-props "className" (str class-names-from-props " " class-names))
        (unchecked-set js-props "className" class-names)))
    (create-element tag-name js-props (map as-elem children))))

(defn- memo-eq [prev next]
  (and (= (unchecked-get prev "p")
          (unchecked-get next "p"))
       (= (unchecked-get prev "c")
          (unchecked-get next "c"))))

(defn- as-component-elem [render-fn props children]
  (let [comp (or (unchecked-get render-fn fn-cache-prop)
                 (let [new-comp  (fn [react-props]
                                   (let [props (unchecked-get react-props "p")
                                         ch    (unchecked-get react-props "c")]
                                     (-> (render-fn (assoc (or props {}) :children ch))
                                         (as-elem))))
                       memo-comp (react-memo new-comp memo-eq)]
                   (unchecked-set memo-comp "displayName" (.-name render-fn))
                   (unchecked-set new-comp "displayName" "fn")
                   (unchecked-set render-fn fn-cache-prop memo-comp)
                   memo-comp))]
    (create-element comp #js {:p props :c children} [])))

(defn- vec-as-elem [[tag & [props & children :as props+children]]]
  (let [props    (if (map? props)
                   props)
        children (if props children props+children)]
    (cond
      (keyword? tag) (as-intrinsic-elem tag props children)
      (fn? tag) (as-component-elem tag props children)
      (ifn? tag) (as-component-elem tag props children))))

(defn- expand-to-array [sequence]
  (let [a #js []]
    (doseq [x sequence]
      (.push a (as-elem x)))
    a))

(defn- as-elem [x]
  (cond
    (vector? x) (vec-as-elem x)
    (or (boolean? x)
        (string? x)
        (number? x)
        (nil? x)
        (undefined? x)) x
    (seq? x) (expand-to-array x)
    (keyword? x) (fq-name x)
    :else (throw-ex (str "Invalid hiccup" (pr-str x)))))





(defn render [app node]
  (-> (as-elem app)
      (react-render node)))

(defn use-state [initial]
  (vec (react-use-state initial)))
