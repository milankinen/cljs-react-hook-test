(ns tsers.app
  (:require [tsers.react :refer [use-state use-effect use-ref use-context <>] :as react]
            ["react-select" :as react-select]))


(defn select [{:keys [value on-change options]}]
  (let [type  react-select/default
        props #js {:value    (clj->js value)
                   :onChange (if on-change #(on-change (js->clj % :keywordize-keys true)))
                   :options  (clj->js options)}]
    (react/create-element type props)))

(def ctx (react/create-context nil))

(def s (atom false))
(def p (js/Promise. (fn [resolve] (js/setTimeout #(do (reset! s true) (resolve)) 1000))))

(defn inner [{:keys [ref]}]
  (if-not @s (throw p))
  (let [value (use-context ctx)]
    [:p {:ref ref}
     "..." value "..."]))

(defn failing-btn [_]
  (let [[fail? set-fail!] (use-state false)]
    (if fail? (throw (js/Error. ":-/")))
    [:button {:on-click #(set-fail! true)}
     "Click to fail"]))

(defn error-test [_]
  (let [[error set-error!] (use-state nil)]
    [react/error-boundary {:on-error #(set-error! %)}
     (if-not error
       (<> [:p "OK!"]
           [failing-btn])
       [:strong "OOOPS"])]))

(defn- app [_]
  (let [[selection set-selection!] (use-state nil)
        [count set-count!] (use-state 0)
        dom-node (use-ref nil)
        _        (use-effect (fn []
                               (js/console.log @dom-node))
                             [])]
    (<>
      [react/suspense {:fallback [:div "Laddar..."]}
       [react/context-provider {:context ctx :value "tsers"}
        [:div
         [:h1 "Best programming language is: " (or (:label selection) "N/A")]
         [select {:value     selection
                  :on-change set-selection!
                  :options   [{:value "js"
                               :label "JavaScript"}
                              {:value "cljs"
                               :label "ClojureScript"}]}]
         [:button {:on-click #(set-count! inc)}
          "INCREMENT (" count ")"]]
        [inner {:ref dom-node :title "asd"}]]]
      [error-test])))

(defn main []
  (let [root (.getElementById js/document "root")]
    (react/render [app] root)))
