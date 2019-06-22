(ns tsers.react
  (:require [clojure.walk :refer [postwalk]]))

(defmacro use-effect* [& body]
  (let [local-deps (set (keys (:locals &env)))
        local-dep? #(and (symbol? %)
                         (contains? local-deps %))
        deps       (atom #{})]
    (postwalk (fn [x] (if (local-dep? x) (swap! deps conj x)) nil) body)
    `(tsers.react/use-effect (fn [] (println ~@(map str @deps)) ~@body) ~(vec @deps))))

(defmacro use-layout-effect* [& body]
  (let [local-deps (set (keys (:locals &env)))
        local-dep? #(and (symbol? %)
                         (contains? local-deps %))
        deps       (atom #{})]
    (postwalk (fn [x] (if (local-dep? x) (swap! deps conj x)) nil) body)
    `(tsers.react/use-layout-effect (fn [] (println ~@(map str @deps)) ~@body) ~(vec @deps))))

(defmacro use-memo* [val]
  (let [local-deps (set (keys (:locals &env)))
        local-dep? #(and (symbol? %)
                         (contains? local-deps %))
        deps       (atom #{})]
    (postwalk (fn [x] (if (local-dep? x) (swap! deps conj x)) nil) val)
    `(tsers.react/use-memo (fn [] ~val) ~(vec @deps))))
