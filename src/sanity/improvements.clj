(ns sanity.improvements
 "Various improvements to clojure"
 (:refer-clojure :exclude [let loop count])
 (:require [clojure.tools.macro :refer [name-with-attributes]]
           [clojure.walk :refer [prewalk]]
           [potemkin.namespaces :as p]
           [me.raynes.fs :as fs]
           [clojure.string :as s]
           [taoensso.timbre :as log]
           [clansi.core :as color]))

(import java.io.File)

;;; Bracketing

;; Things from core.clj
;; TODO It would be nice if I could just get a handle on these rather than redefine them; surely RT can do this
(defn core-count
  "Returns the number of items in the collection. (count nil) returns
  0.  Also works on strings, arrays, and Java Collections and Maps"
  {
   :inline (fn  [x] `(. clojure.lang.RT (count ~x)))
   :added "1.0"}
  [coll] (clojure.lang.RT/count coll))

(defmacro ^{:private true} assert-args
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                  (str (first ~'&form) " requires " ~(second pairs) " in " ~'*ns* ":" (:line (meta ~'&form))))))
     ~(clojure.core/let [more (nnext pairs)]
        (when more
          (list* `assert-args more)))))

(defmacro core-let
  "binding => binding-form init-expr

  Evaluates the exprs in a lexical context in which the symbols in
  the binding-forms are bound to their respective init-exprs or parts
  therein."
  {:added "1.0", :special-form true, :forms '[(let [bindings*] exprs*)]}
  [bindings & body]
  (assert-args
     (vector? bindings) "a vector for its binding"
     (even? (core-count bindings)) "an even number of forms in binding vector")
  `(let* ~(destructure bindings) ~@body))

(defn ^:private ^:static
  reduce1
       ([f coll]
             (core-let [s (seq coll)]
               (if s
         (reduce1 f (first s) (next s))
                 (f))))
       ([f val coll]
          (core-let [s (seq coll)]
            (if s
              (if (chunked-seq? s)
                (recur f
                       (.reduce (chunk-first s) f val)
                       (chunk-next s))
                (recur f (f val (first s)) (next s)))
         val))))

(defmacro core-loop
  "Evaluates the exprs in a lexical context in which the symbols in
  the binding-forms are bound to their respective init-exprs or parts
  therein. Acts as a recur target."
  {:added "1.0", :special-form true, :forms '[(loop [bindings*] exprs*)]}
  [bindings & body]
    (assert-args
      (vector? bindings) "a vector for its binding"
      (even? (core-count bindings)) "an even number of forms in binding vector")
    (core-let [db (destructure bindings)]
      (if (= db bindings)
        `(loop* ~bindings ~@body)
        (core-let [vs (take-nth 2 (drop 1 bindings))
              bs (take-nth 2 bindings)
              gs (map (fn [b] (if (symbol? b) b (gensym))) bs)
              bfs (reduce1 (fn [ret [b v g]]
                            (if (symbol? b)
                              (conj ret g v)
                              (conj ret g v b g)))
                          [] (map vector bs vs gs))]
          `(core-let ~bfs
             (loop* ~(vec (interleave gs gs))
               (core-let ~(vec (interleave bs gs))
                 ~@body)))))))

(defn dynamic-symbol? [^clojure.lang.Symbol s]
 (and (.startsWith (.getName s) "*")
      (.endsWith (.getName s) "*")
      (> (core-count (.getName s)) 2)))

(defmacro define
 "Similar to Scheme's define. Automatically generates def vs defn (with or
  without dynamic) as needed. Docstrings come after the head of the define, as
  in (define (a) docstring? b)"
 [head docstring? & body]
 `(~(if (seq? head) 'defn 'def)
   ~@(if (seq? head)
      `(~(first head)
        ~@(if (and (string? docstring?) (not (empty? body)))
           (list docstring?)
           '())
        ~(vec (replace {'. '&} (rest head))))
      (if (sanity.improvements/dynamic-symbol? head)
       (list (with-meta head {:dynamic true}))
       (list head)))
   ~@(if (and (seq? head) (string? docstring?))
      (if (empty? body) (list docstring?) body)
      (cons docstring? body))))

(defmacro conds
 "A variant of cond with sane (lisp/scheme) bracketing. Unfortunately there's
  no way to detect if we want the clojure cond or the sane cond so we have to
  explicitly pick this version when desired."
 [& args]
 (if (empty? args)
  nil
  (core-let [as (into [] (reduce concat (map (fn [[h & ys]] (list h (cons 'do ys)))
                                             args)))
             r (if (= (nth as (- (core-count as) 2)) 'else)
                (assoc as (- (core-count as) 2) ':else)
                as)]
            `(cond ~@r))))

(defmacro unless
 "Evaluates test. If not true, evaluates body. The opposite of when."
 [test & body]
 (list 'if-not test (cons 'do body)))

(defmacro cases
 "A variant of case with sane (lisp/scheme) bracketing. Unfortunately there's
  no way to detect if we want the clojure case or the sane case so we have to
  explicitly pick this version when desired."
 [obj & args]
 (if (empty? args)
  nil
  (core-let [as (into [] (reduce concat (map (fn [[h & ys]] (list h (cons 'do ys)))
                                             args)))
             r (if (= (nth as (- (core-count as) 2)) 'else)
                (concat (take (- (core-count as) 2) as) (list (last as)))
                as)]
            `(case ~obj ~@r))))

(defmacro lambda "A variant of fn that uses lisp/scheme bracket." [args & body]
 `(~'fn ~(vec (replace {'. '&} args)) ~@body))

;; Now we evaluate in clojure.core. We can't just replace these macros
;; because warnOrFailOnReplace has a really nasty hack that checks for
;; clojure.core.
(core-let
 [current *ns*]
 (binding [*ns* (create-ns 'clojure.core)]
  (eval '(do

          (defmacro let
           "binding => binding-form init-expr
  Evaluates the exprs in a lexical context in which the symbols in
  the binding-forms are bound to their respective init-exprs or parts
  therein. Augmented to support sane (lisp/scheme) bracketing."
           [bindings & body]
           `(core-let
             ~(if (seq? bindings) (into [] (reduce concat bindings)) bindings)
             ~@body))


          (defmacro loop
           "Evaluates the exprs in a lexical context in which the symbols in
  the binding-forms are bound to their respective init-exprs or parts
  therein. Acts as a recur target. Augmented to support sane (lisp/scheme)
  bracketing."
           [bindings & body]
           `(core-loop
             ~(if (seq? bindings) (into [] (reduce concat bindings)) bindings)
             ~@body))

;;; Misc, non-bracketing

          (defn count
           "Replaces clojure.core/count. Accepts either a collection or a
  predicate and a collection."
           ([l] (sanity.improvements/core-count l))
           ([p l] (sanity.improvements/core-loop [l l c 0]
                   (sanity.improvements/conds
                    ((empty? l) c)
                    ((p (first l)) (recur (rest l) (+ c 1)))
                    (else (recur (rest l) c))))))

          (defn every?
           "Is p true of every element of the collection(s) l(s)?"
           [p l & ls]
           (sanity.improvements/core-loop [l l ls ls]
            (or (or (empty? l) (nil? l))
                (and (apply p (first l) (map first ls))
                     (recur (rest l) (map rest ls))))))
          
          (defn some?
           "Is p true of every element of the collection(s) l(s)?"
           [p l & ls]
           (loop [l l ls ls]
            (and (not (or (empty? l) (nil? l)))
               (or (apply p (first l) (map first ls))
                  (recur (rest l) (map rest ls))))))))))
