(ns fdp.core
  (:require [hicada.compiler]))

(defmacro html
  [body]
  (hicada.compiler/compile body {:create-element 'js/React.createElement
                                 :transform-fn (comp)
                                 :array-children? false}
                           {} &env))
