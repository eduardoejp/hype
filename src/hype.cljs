(ns hype
  (:require [clojure.string :as string]
            [goog.dom :as gdom]
            [goog.events :as g-events])
  (:require-macros [clojure.template :refer [do-template]]
                   [hype :refer [defattr]]))

(declare vector->html*
         node)

;; [Constants]
(def ^:private +tag-regex+ #"^([\w-_]+)?(#([\w-_]+))?((\.[\w-_]+)*)")

;; [State]
(def ^:dynamic *tag-fns*  {})
(def ^:dynamic *attr-fns* {})

;; [Utils]
(defn ^:private tag-classes [ns tag]
  (if ns
    (str ns
         (if (and ns tag)
           (str " " ns "_" tag)))))

(defn ^:private normal [[ns+tag attrs & body]]
  (let [ns (namespace ns+tag)
        ns (if ns (string/replace ns #"\." "_"))
        [tag _ id classes] (next (re-find +tag-regex+ (name ns+tag)))
        classes (if (not (empty? classes))
                  (string/replace (.substring classes 1) #"\." " "))
        extracted-classes (if (and ns classes)
                            (str (tag-classes ns tag) " " classes)
                            (or (tag-classes ns tag) classes))
        attrs-is-map? (and (map? attrs)
                           (not (gdom/isNodeLike attrs)))
        attrs* (-> (if attrs-is-map? attrs {})
                   (update-in [:id] #(or % id))
                   (update-in [:class] #(if (and % extracted-classes)
                                          (str % " " extracted-classes)
                                          (or % extracted-classes))))
        body* (if attrs-is-map?
                body
                (cons attrs body))]
    [ns+tag ns tag attrs* body*]))

(defn ^:private attrs<= [node attrs]
  (doseq [[k v] attrs
          :when v]
    (if (namespace k)
      (if-let [attr-fn (get *attr-fns* k)]
        (attr-fn node v)
        (throw (ex-info "There isn't a registered handler for the attribute." {:attr k})))
      (assoc node k v)))
  node)

(let [regex #"^:[\w-_]+/[\w-_]+"]
  (def ^:private ->tag-string
    (memoize #(re-find regex (pr-str %)))))

(defn ^:private expansion [ns+tag attrs body]
  (if-let [tag-fn (get *tag-fns* (->tag-string ns+tag))]
    (apply tag-fn attrs body)
    (throw (ex-info "There isn't a registered handler for the tag." {:tag ns+tag}))))

(defn ^:private append-sub! [parent sub]
  (cond (vector? sub) (let [[_node _attrs] (vector->html* sub)]
                        (gdom/appendChild parent _node)
                        (attrs<= _node _attrs))
        (string? sub) (gdom/appendChild parent (gdom/createTextNode sub))
        (seq? sub)    (doseq [child sub]
                        (append-sub! parent child))
        (nil? sub)    nil
        :else         (gdom/appendChild parent (node sub))))

(defn ^:private vector->html* [vector]
  (let [[ns+tag ns tag attrs body] (normal vector)]
    (if ns
      (let [expanded (expansion ns+tag attrs body)]
        (if (vector? expanded)
          (recur expanded)
          [expanded attrs]))
      (let [_node (gdom/createDom tag)]
        (doseq [child body]
          (append-sub! _node child))
        [_node attrs]))))

(defn ^:private vector->html [vector]
  (let [[_node _attrs] (vector->html* vector)]
    (attrs<= _node _attrs)))

;; [Procedures]
(do-template [<sym> <var> <pre> <msg>]
  (defn <sym> [item f]
    (let [item* (<pre> item)]
      (if-let [tag-fn (get <var> item*)]
        (throw <msg>)
        (set! <var> (assoc <var> item* f))))
    nil)

  register-tag!  *tag-fns*  ->tag-string
  (str "Can't re-define an existing tag (" (pr-str item) ")")
  
  register-attr! *attr-fns* identity
  (str "Can't re-define an existing attribute (" (pr-str item) ")"))

;; [Installation]
(defprotocol DOM_Node
  (node [self]))

(extend-protocol DOM_Node
  PersistentVector
  (node [self]
    (vector->html self))

  js/HTMLElement
  (node [self]
    self)

  js/SVGSVGElement
  (node [self]
    self))

(defattr :$/events [node events]
  (doseq [[k v] events]
    (if (coll? v)
      (doseq [v* v]
        (g-events/listen node k v*))
      (g-events/listen node k v))))

(defattr :$/hooks [node hooks]
  (set! (.-_hooks_ node) hooks))
