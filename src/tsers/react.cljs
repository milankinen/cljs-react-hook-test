(ns tsers.react
  (:require ["react" :as react]
            ["react-dom" :as react-dom]
            [clojure.string :as string]))

(def ^:private component-cache-prop
  (js/Symbol "CachedComponent"))

; cache for parsed keyword tags
(def ^:private tag-cache
  (js/Map.))

; cache for camelized keyword props
(def ^:private prop-cache
  (js/Map.))

(defn- throw-ex [msg]
  (throw (js/Error. msg)))

(def ^:private react-render react-dom/render)
(def ^:private react-create-element react/createElement)
(def ^:private react-memo react/memo)
(def ^:private react-use-state react/useState)

(declare as-elem)
(declare as-js-props)

; Flattens children to variadic form expected by React
; and optimize some common cases
(defn- $ [js-type js-props js-children]
  (let [t js-type
        p js-props
        c js-children]
    (case (alength c)
      0 (react-create-element t p)
      1 (react-create-element t p (aget c 0))
      2 (react-create-element t p (aget c 0) (aget c 1))
      3 (react-create-element t p (aget c 0) (aget c 1) (aget c 2))
      4 (react-create-element t p (aget c 0) (aget c 1) (aget c 2) (aget c 3))
      5 (react-create-element t p (aget c 0) (aget c 1) (aget c 2) (aget c 3) (aget c 4))
      (.apply react-create-element nil (.concat #js [t p] c)))))

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
  (let [res #js {:name nil :id nil :classes nil}]
    (loop [si   0
           i    0
           mode nil]
      (let [ch (.charAt tag i)]
        (if (or (identical? ch ".")
                (identical? ch "#")
                (identical? ch ""))
          (do (assert (not= si i) (str "Invalid hiccup tag: " tag))
              (case mode
                "." (if-let [cls (.-classes res)]
                      (.push cls (subs tag si i))
                      (set! res -classes #js [(subs tag si i)]))
                "#" (do (assert (nil? (.-id res)) (str "Invalid hiccup tag: " tag))
                        (set! res -id (subs tag si i)))
                (do (assert (nil? (.-name res)) (str "Invalid hiccup tag: " tag))
                    (set! res -name (subs tag si i))))
              (if (identical? ch "")
                ['todo]
                (recur (inc i) (inc i) ch)))
          (recur si (inc i) mode))))
    [(.-name res)
     (.-id res)
     (some-> (.-classes res) (.join " "))]))

(defn- parse-tag-cached [tag-kw]
  (let [tag-s (name tag-kw)]
    (if-let [cached-result (.get tag-cache tag-s)]
      cached-result
      (let [res (parse-tag tag-s)]
        (.set tag-cache tag-s res)
        res))))

(defn- camelize [s]
  (let [res (.split s "-")
        n   (.-length res)
        buf (make-array n)]
    (unchecked-set buf 0 (unchecked-get res 0))
    (loop [i 1]
      (when (< i n)
        (let [part      (unchecked-get res i)
              camelized (str (.toUpperCase (.charAt part 0)) (subs part 1))]
          (unchecked-set buf i camelized))))
    (.join buf "")))

(defn- camelize-cached [prop-kw]
  (let [prop-s (name prop-kw)]
    (if-let [cached-result (.get prop-cache prop-s)]
      cached-result
      (let [res (camelize prop-s)]
        (.set prop-cache prop-s res)
        res))))

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
                  (keyword? k) (camelize-cached k)
                  (string? k) k
                  :else (throw-ex (str "Invalid intrinsic property name" (pr-str k))))]
          (unchecked-set js-props p (as-js-prop-value v))))
      js-props)))

(defn- as-elem-array [sequence]
  (let [a #js []]
    (doseq [x sequence]
      (.push a (as-elem x)))
    a))

; TODO CHECK SOME SANE KEYS?
(defn- as-js-key [key]
  (cond
    (or (string? key)
        (number? key)
        (boolean? key)
        (undefined? key)) key
    (keyword? key) (fq-name key)
    (satisfies? IPrintWithWriter key) (pr-str key)
    true key))

(defn- as-intrinsic-elem [[tag-name id class-names] key props children]
  (let [js-props (as-js-props props)]
    (when (some? id)
      (assert (nil? (unchecked-get js-props "id")) (str "Id defined twice for tag " tag-name))
      (unchecked-set js-props "id" id))
    (when (some? class-names)
      (if-let [class-names-from-props (unchecked-get js-props "className")]
        (unchecked-set js-props "className" (str class-names-from-props " " class-names))
        (unchecked-set js-props "className" class-names)))
    (when (some? key)
      (unchecked-set js-props "key" (as-js-key key)))
    ($ tag-name js-props (as-elem-array children))))

(defn as-clj-props [react-props]
  (let [children (unchecked-get react-props "c")
        props    (or (unchecked-get react-props "p") {})]
    (assoc props :children children)))

(defn- wrap-memo [comp eq]
  {:pre [(or (false? eq)
             (fn? eq))]}
  (if (fn? eq)
    (react-memo comp #(eq (as-clj-props %1) (as-clj-props %2)))
    comp))

(defn- with-display-name! [comp name]
  (unchecked-set comp "displayName" name)
  comp)

(defn- as-component-elem [render-fn key props children]
  (let [comp     (or (unchecked-get render-fn component-cache-prop)
                     (let [fn-name  (.-name render-fn)
                           wrapper  (-> (fn [react-props]
                                          (-> (as-clj-props react-props)
                                              (render-fn)
                                              (as-elem)))
                                        (with-display-name! fn-name))
                           eq       (if-some [meta-eq (:memo (meta render-fn))]
                                      meta-eq
                                      =)
                           new-comp (-> (wrap-memo wrapper eq)
                                        (with-display-name! (str "memo(" fn-name ")")))]
                       (unchecked-set render-fn component-cache-prop new-comp)
                       new-comp))
        js-props (if (some? key)
                   #js {:p props :c children :key (as-js-key key)}
                   #js {:p props :c children})
        js-ch    #js []]
    ($ comp js-props js-ch)))

(defn- vec-as-elem [[tag & [props & children :as props+children] :as elem]]
  (let [props    (if (map? props)
                   props)
        children (if props children props+children)
        key      (meta elem)]
    (cond
      (keyword? tag) (as-intrinsic-elem (parse-tag-cached tag) key props children)
      (or (fn? tag)
          (ifn? tag)) (as-component-elem tag key props children)
      (string? tag) (as-intrinsic-elem (parse-tag tag) key props children)
      true (throw-ex (str "Invalid hiccup tag type: " (type tag))))))

(defn- as-elem [x]
  (cond
    (vector? x) (vec-as-elem x)
    (or (boolean? x)
        (string? x)
        (number? x)
        (nil? x)
        (undefined? x)) x
    (seq? x) (as-elem-array x)
    (keyword? x) (fq-name x)
    true x))


;;
;; Public APIs
;;

(defn render [hiccup dom-node]
  (-> (as-elem hiccup)
      (react-render dom-node)))

(defn use-state [initial]
  (vec (react-use-state initial)))
