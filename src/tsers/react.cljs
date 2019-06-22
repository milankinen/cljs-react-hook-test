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
(def ^:private react-create-context react/createContext)
(def ^:private react-fragment react/Fragment)
(def ^:private react-memo react/memo)
(def ^:private react-use-state react/useState)
(def ^:private react-use-effect react/useEffect)
(def ^:private react-use-context react/useContext)
(def ^:private react-use-layout-effect react/useLayoutEffect)
(def ^:private react-use-memo react/useMemo)
(def ^:private react-use-callback react/useCallback)
(def ^:private react-use-reducer react/useReducer)
(def ^:private react-use-ref react/useRef)

(defonce ^:private default-memo-fn (atom =))

(defn- native-react-type? [x]
  (or (string? x)
      (fn? x)
      (instance? react/Component x)))

(def ^:private obj-proto-to-string
  (.. js/Object -prototype -toString))

(defn- plain-object? [x]
  (and (goog/isObject x)
       (identical? (.-constructor x) js/Object)
       (identical? (.call obj-proto-to-string x) "[object Object]")))

(declare as-element)
(declare as-js-props)

(defn- ref-atom [initial-value]
  (let [a (atom initial-value)]
    (assert (identical? js/undefined (.-current a)))
    ; for React ref interrop
    (js/Object.defineProperty
      a
      "current"
      #js {:get (fn [] @a)
           :set (fn [val] (reset! a val))})
    a))

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

(defn- display-name [comp]
  (or (.-displayName comp)
      (.-name comp)))

(defn- with-display-name! [comp name]
  (set! comp -displayName name)
  comp)

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
    (satisfies? IPrintWithWriter key) (pr-str key)
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
      (.push a (as-element x)))
    a))

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
  (let [js-props (as-js-props (if (some? key) (assoc props :key key) props))]
    (when (some? id)
      (assert (nil? (unchecked-get js-props "id")) (str "Id defined twice for tag " tag-name))
      (unchecked-set js-props "id" id))
    (when (some? class-names)
      (if-let [class-names-from-props (unchecked-get js-props "className")]
        (unchecked-set js-props "className" (str class-names-from-props " " class-names))
        (unchecked-set js-props "className" class-names)))
    ($ tag-name js-props (as-elem-array children))))

(defn- unwrap-props [wrapped-props]
  (let [children (unchecked-get wrapped-props "c")
        props    (or (unchecked-get wrapped-props "p") {})]
    (assoc props :children children)))

(defn- wrap-props [key props children]
  (if (some? key)
    #js {:p props :c children :key (as-js-key key)}
    #js {:p props :c children}))

(defn- wrap-component [render-fn]
  (let [wrapper (-> (fn [react-props]
                      (-> (unwrap-props react-props)
                          (render-fn)
                          (as-element)))
                    (with-display-name! (display-name render-fn)))
        memo    (:memo (meta render-fn))
        eq      (or memo @default-memo-fn)]
    (if-not (false? memo)
      (-> (react-memo wrapper #(eq (unwrap-props %1) (unwrap-props %2)))
          (with-display-name! (str "memo(" (display-name render-fn) ")")))
      wrapper)))

(defn- as-component-elem [render-fn key props children]
  (let [comp (or (unchecked-get render-fn component-cache-prop)
                 (let [new-comp (wrap-component render-fn)]
                   (unchecked-set render-fn component-cache-prop new-comp)
                   new-comp))]
    ($ comp (wrap-props key props children) #js [])))

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
      (some? (.-$$typeof tag)) (as-component-elem tag key props children)
      true (throw-ex (str "Invalid hiccup tag type: " (type tag))))))

(defn- as-react-deps [deps]
  (->> (map #(cond
               (or (string? %)
                   (number? %)
                   (boolean? %)
                   (fn? %)
                   (undefined? %)
                   (nil? %)) %
               (keyword? %) (hash %)
               (implements? IHash %) (hash %)
               true %)
            deps)
       (into-array)))

(defn- wrap-effect-fn [eff]
  (fn []
    (let [res (eff)]
      (cond
        (fn? res) res
        (ifn? res) #(res)
        true js/undefined))))

;;
;; Public APIs
;;

(defn disable-memo!
  "Globally disables component memoization for all components. You can
   still memoize individual components by using `with-memo`."
  []
  (reset! default-memo-fn false))

(defn render
  "Renders the given hiccup using the given dom node as a root
   for the rendered tree"
  [hiccup dom-node]
  (-> (as-element hiccup)
      (react-render dom-node)))

(defn as-element
  "Turns the given hiccup into React element(s) recursively, for example:

   ```cljs
   (def app-el
     (as-element [:div.app
                   [sidebar {:version 123 :title \"tsers\"]
                   [main {}]]))
   ```
  "
  [hiccup]
  (cond
    (vector? hiccup) (vec-as-elem hiccup)
    (or (boolean? hiccup)
        (string? hiccup)
        (number? hiccup)
        (nil? hiccup)
        (undefined? hiccup)) hiccup
    (seq? hiccup) (as-elem-array hiccup)
    (keyword? hiccup) (fq-name hiccup)
    true hiccup))

(defn create-element
  "Creates a native React element by calling React.createElement directly.

   No cljs->js conversions are applied to the props and children so this means
   that props (second argument) must be either plain JS object or nil and children
   must be native React elements as well. Use `as-element` to convert children into
   native elements.

   This function is mainly needed for JS library interrop.

   An example:
   ```cljs
   (require '[\"react-select\" :as react-select])

   (defn select [{:keys [value on-change options]}]
     (let [type  react-select/default
           props #js {:value    (clj->js value)
                      :onChange (if on-change #(on-change (js->clj % :keywordize-keys true)))
                      :options  (clj->js options)}]
       (create-element type props)))

   (defn app [_]
     (let [[selection set-selection!] (use-state nil)]
       [:div
        [:h1 \"Best programming language is: \" (or (:label selection) \"N/A\")]
        [select {:value     (:value selection)
                 :on-change set-selection!
                 :options   [{:value \"js\"
                              :label \"JavaScript\"}
                             {:value \"cljs\"
                              :label \"ClojureScript\"}]}]]))
   ```
   "
  [type props & children]
  {:pre [(native-react-type? type)
         (or (nil? props)
             (plain-object? props))]}
  ($ type props (into-array children)))

(defn <>
  "Creates a React Fragment with the given children. Children can be either
   native React elements or hiccup elements."
  [& children]
  ($ react-fragment #js {} (as-elem-array children)))

(defn create-context
  "Creates a new React Context with the given default value. See
   https://reactjs.org/docs/context.html for more details."
  [default-value]
  (react-create-context default-value))

(defn context-provider
  "Provides context to the rendered sub-tree, see https://reactjs.org/docs/context.html#contextprovider
   for more details.

   The usage of `context-provider` differs from React's `Context.Provider`
   in requiring the used context to be given as a `context` prop the the
   provider along with context's `value`.

   An example:
   ```cljs
   (def my-context (create-context 'some-default-value)

   (defn my-app [_]
     [context-provider {:context my-context :value '...}
       [sidebar]
       [main]
       [footer]])
   ```
  "
  [{:keys [context value children]}]
  {:pre [(some? context)
         (.-Provider context)]}
  (let [provider (.-Provider context)]
    ($ provider #js {:value value} (as-elem-array children))))

(defn with-memo
  "By default, all components are wrapped with `React.memo` using `=` as an
   equality operator.

   You can override the equality operator or disable memoization by using
   this function and providing a custom equality function or `false` as
   a second argument.

   You can also disable memoization by using `disable-memo!`.
  "
  ([component memo]
   {:pre [(or (fn? memo)
              (false? memo))]}
   (with-meta component {:memo memo})))

(defn use-state
  "Wrapper for useState hook, see https://reactjs.org/docs/hooks-reference.html#usestate
   for more details.

   Returned value is a vector or `[state set-state]`
  "
  [initial-state]
  (vec (react-use-state initial-state)))

(defn use-effect
  "Wrapper for useEffect hook, see https://reactjs.org/docs/hooks-reference.html#useeffect
   for more details.

   Dependencies must be either a vector or `nil`. Use `[]` to run effect function
   only once and `nil` to run it after every commit.
  "
  [eff deps]
  {:pre [(or (nil? deps)
             (vector? deps))
         (or (fn? eff)
             (ifn? eff))]}
  (if (some? deps)
    (react-use-effect (wrap-effect-fn eff) (as-react-deps deps))
    (react-use-effect (wrap-effect-fn eff))))

(defn use-context
  "Wrapper for useContext, see https://reactjs.org/docs/hooks-reference.html#usecontext
   for more details."
  [context]
  {:pre [(some? context)
         (.-Provider context)]}
  (react-use-context context))

(defn use-layout-effect
  "Wrapper for useLayoutEffect hook, see https://reactjs.org/docs/hooks-reference.html#uselayouteffect
   for more details.

   Dependencies must be either a vector or `nil`. Use `[]` to run effect function
   only once and `nil` to run it after every commit."
  [eff deps]
  {:pre [(or (nil? deps)
             (vector? deps))
         (or (fn? eff)
             (ifn? eff))]}
  (if (some? deps)
    (react-use-layout-effect (wrap-effect-fn eff) (as-react-deps deps))
    (react-use-layout-effect (wrap-effect-fn eff))))

(defn use-memo
  "Wrapper for useMemo hook, see https://reactjs.org/docs/hooks-reference.html#usememo
   for more details."
  [value-fn deps]
  {:pre [(fn? value-fn)
         (vector? deps)]}
  (react-use-memo value-fn (as-react-deps deps)))

(defn use-callback
  "Wrapper for useCallback hook, see https://reactjs.org/docs/hooks-reference.html#usecallback
   for more details."
  [callback-fn deps]
  {:pre [(fn? callback-fn)
         (vector? deps)]}
  (react-use-callback callback-fn (as-react-deps deps)))

(defn use-ref
  "Wrapper for useRef hook, see https://reactjs.org/docs/hooks-reference.html#useref
   for more details.

   However, instead of plain JS object with `current` attribute, this function
   returns an atom containing the current value of the ref.
   "
  [initial-value]
  (let [ref (react-use-ref nil)]
    (or (.-current ref)
        (let [a (ref-atom initial-value)]
          (set! (.-current ref) a)
          a))))

(defn use-reducer
  "Wrapper for useReducer, see https://reactjs.org/docs/hooks-reference.html#usereducer
   for more details.

   Returned value is a vector or `[state dispatch]`
  "
  ([reducer initial-arg init]
   {:pre [(fn? reducer)
          (or (fn? init)
              (nil? init))]}
   (-> (if (fn? init)
         (react-use-reducer reducer initial-arg init)
         (react-use-reducer reducer initial-arg))
       (vec)))
  ([reducer initial-arg]
   (use-reducer reducer initial-arg nil)))
