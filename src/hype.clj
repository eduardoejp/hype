(ns hype
  (:use clojure.template))

;; [Interface]
(do-template [<sym> <proc>]
  (defmacro <sym> [item args & body]
    {:pre [(and (keyword? item) (namespace item))
           (vector? args)
           (not (empty? body))]}
    `(~'<proc> ~item (fn ~args ~@body)))
  
  deftag  hype/register-tag!
  defattr hype/register-attr!)
