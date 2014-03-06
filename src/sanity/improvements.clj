(ns sanity.improvements
 "Various improvements to clojure"
 (:require [clojure.tools.macro :refer [name-with-attributes]]
           [clojure.walk :refer [prewalk]]
           [net.n01se.clojure-jna :as jna]
           [potemkin.namespaces :as p]
           [me.raynes.fs :as fs]
           [clojure.string :as s]
           [taoensso.timbre :as log]
           [clansi.core :as color]))

(import java.io.File)

;;; Bracketing

(defn dynamic-symbol? [^clojure.lang.Symbol s]
 (and (.startsWith (.getName s) "*")
      (.endsWith (.getName s) "*")
      (> (count (.getName s)) 2)))

(defmacro define
 "Similar to Scheme's define. Rest arguments must be marked by '&' not '.'
  Automatically generates def vs defn (with or without dynamic) as needed."
 [head & body]
 `(~(if (seq? head) 'defn 'def)
   ~@(if (seq? head)
      (list (first head) (vec (replace {'. '&} (rest head))))
      (if (dynamic-symbol? head)
       (list (with-meta head {:dynamic true}))
       (list head)))
   ~@ body))

(defmacro unless
 "Evaluates test. If not true, evaluates body. The opposite of when."
 [test & body]
 (list 'if-not test (cons 'do body)))

(defmacro let
 "binding => binding-form init-expr
  Evaluates the exprs in a lexical context in which the symbols in
  the binding-forms are bound to their respective init-exprs or parts
  therein. Augmented to support sane (lisp/scheme) bracketing."
 [bindings & body]
 `(clojure.core/let
    ~(if (seq? bindings) (into [] (reduce concat bindings)) bindings)
   ~@body))

(defmacro loop
 "Evaluates the exprs in a lexical context in which the symbols in
  the binding-forms are bound to their respective init-exprs or parts
  therein. Acts as a recur target. Augmented to support sane (lisp/scheme)
  bracketing."
 [bindings & body]
 `(clojure.core/loop
    ~(if (seq? bindings) (into [] (reduce concat bindings)) bindings)
   ~@body))

(defmacro conds
 "A variant of cond with sane (lisp/scheme) bracketing. Unfortunately there's
  no way to detect if we want the clojure cond or the sane cond so we have to
  explicitly pick this version when desired."
 [& args]
 (if (empty? args)
  nil
  (let ((as (into [] (reduce concat (map (fn [[h & ys]] (list h (cons 'do ys)))
                                         args))))
        (r (if (= (nth as (- (count as) 2)) 'else)
            (assoc as (- (count as) 2) ':else)
            as)))
   `(cond ~@r))))

(defmacro cases
 "A variant of case with sane (lisp/scheme) bracketing. Unfortunately there's
  no way to detect if we want the clojure case or the sane case so we have to
  explicitly pick this version when desired."
 [obj & args]
 (if (empty? args)
  nil
  (let ((as (into [] (reduce concat (map (fn [[h & ys]] (list h (cons 'do ys)))
                                         args))))
        (r (if (= (nth as (- (count as) 2)) 'else)
            (concat (take (- (count as) 2) as) (list (last as))) 
            as)))
   `(case ~obj ~@r))))

(defmacro lambda [args & body] "A variant of fn that uses lisp/scheme bracket."
 `(~'fn ~(vec (replace {'. '&} args)) ~@body))

;;; Misc

(defn count
 "Replaces clojure.core/count. Accepts either a collection or a
  predicate and a collection."
 ([l] (clojure.core/count l))
 ([p l] (loop ((l l) (c 0))
         (conds ((empty? l) c)
           ((p (first l)) (recur (rest l) (+ c 1)))
          (else (recur (rest l) c))))))
