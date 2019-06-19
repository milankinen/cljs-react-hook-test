(ns tsers.app
  (:require [tsers.react :as react]))

(defn- lolbal [{:keys [title]}]
  (let [[count set-count!] (react/use-state 0)]
    [:div
     [:h1 title "(" count ")" "??"]
     [:button {:on-click #(set-count! inc)}
      "++"]]))

(defn main []
  (let [root (.getElementById js/document "root")]
    (react/render
      [:div.foobar
       [lolbal {:title "tsers"}]
       [:div "bal"]]
      root)))
