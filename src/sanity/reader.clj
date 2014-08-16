(ns sanity.reader
 "Various improvements to clojure"
 (:require [clojure.tools.macro :refer [name-with-attributes]]
           [clojure.walk :refer [prewalk]]
           [potemkin.namespaces :as p]
           [me.raynes.fs :as fs]
           [clojure.string :as s]
           [taoensso.timbre :as log]
           [clansi.core :as color]))

(use 'sanity.core)
(use 'sanity.improvements)

;;; Reader macros

(import '(clojure.lang
          RT LispReader Var Compiler ISeq
          IPersistentMap Util AFn PersistentVector Symbol IPersistentCollection IRecord
          IPersistentList IPersistentSet IPersistentVector Keyword IObj PersistentHashMap))
(import '(java.io PushbackReader Reader))

(defn get-field [class field]
 "Get a handle to a potentially protected/private field"
 (.get (doto (.getDeclaredField class field) (.setAccessible true)) nil))

(defn get-method' [class name & type-args]
 "Get a handle to a potentially protected/private method"
 (let [f (doto (.getDeclaredMethod class name (into-array java.lang.Class type-args)) (.setAccessible true))]
  (fn [object & args] (.invoke f object (into-array args)))))

;; http://briancarper.net/blog/449/
(defn dispatch-reader-macro [ch fun]
 "Call fun with a Reader and the last character read when encountering
  # followed by the character ch."
 (let [dm (.get (doto (.getDeclaredField clojure.lang.LispReader "dispatchMacros")
                 (.setAccessible true))
                nil)]
  (aset dm (int ch) fun)))

(defn string-reader [^Reader r doublequote]
 "A reader macro for reading a literal string"
 (let ((sb (StringBuilder.)))
  (let ((ch (char (LispReader/read1 r))))
   (unless (= ch \") (error (str "Strings start with '\"' not '" ch "'" ))))
  (loop (())
   (let ((ch (char (LispReader/read1 r))))
    (if (= ch \")
     (.toString sb)
     (do (.append sb ch) (recur)))))))

(defn balanced-string-reader [^Reader r start-token end-token]
 "A reader macro for reading a literal string which must start with
  start-token and end with end-token"
 (let ((sb (StringBuilder.)))
  (let ((ch (char (LispReader/read1 r))))
   (unless (= ch start-token) (error (str "Must start with '" start-token "' not '" ch "'" ))))
  (loop ((level 0))
   (let ((ch (char (LispReader/read1 r))))
    (conds ((= ch start-token) (.append sb ch) (recur (+ level 1)))
           ((= ch end-token) (if (= level 0)
                                (.toString sb)
                                (do (.append sb ch) (recur (- level 1)))))
           (else (.append sb ch) (recur level)))))))

;;; Workaround for broken quasiquote
;; Much of this is transliterated from LispReader.java

(defn isUnquote [form]
 (and (instance? ISeq form)
      (Util/equals (RT/first form)
                   (get-field clojure.lang.LispReader "UNQUOTE"))))
(defn isUnquoteSplicing [form]
 (and (instance? ISeq form)
      (Util/equals (RT/first form)
                   (get-field clojure.lang.LispReader "UNQUOTE_SPLICING"))))

(def compiler-quote (get-field Compiler "QUOTE"))

(def lisp-apply (get-field LispReader "APPLY"))
(def lisp-concat (get-field LispReader "CONCAT"))
(def lisp-gensym-env (get-field LispReader "GENSYM_ENV"))
(def lisp-hashmap (get-field LispReader "HASHMAP"))
(def lisp-hashset (get-field LispReader "HASHSET"))
(def lisp-list (get-field LispReader "LIST"))
(def lisp-seq (get-field LispReader "SEQ"))
(def lisp-vector (get-field LispReader "VECTOR"))
(def lisp-with-meta (get-field LispReader "WITH_META"))
(def compiler-resolve-symbol (get-method' Compiler "resolveSymbol" Symbol))

(defn seq' [^clojure.lang.ISeq coll] (if (empty? coll) coll (seq coll)))
(def sane-seq (Symbol/intern "sanity.reader" "seq'"))

(declare syntaxQuote)

(defn sqExpandList [^ISeq seq]
 (loop [ret PersistentVector/EMPTY seq seq]
  (if (nil? seq)
   (.seq ret)
   (recur (let [item (.first seq)]
           (conds ((isUnquote item) (.cons ret (RT/list lisp-list (RT/second item))))
                  ((isUnquoteSplicing item) (.cons ret (RT/second item)))
                  (else (.cons ret (RT/list lisp-list (syntaxQuote item))))))
          (.next seq)))))

(defn syntax-quote [reader backquote]
 (let [r (cast PushbackReader reader)]
  (try (Var/pushThreadBindings (clojure.lang.RT/map (into-array Object [lisp-gensym-env PersistentHashMap/EMPTY])))
       (syntaxQuote (cast Object (read r true nil true)))
       (finally (Var/popThreadBindings)))))

(defn flattenMap [form]
 (loop [keyvals PersistentVector/EMPTY s (RT/seq form)]
  (if (nil? s)
   (.seq keyvals)
   (recur (let [e (.first s)]
           (.cons (.cons keyvals (key e)) (val e)))
          (.next seq)))))

(defn syntaxQuote [form]
 (let
   [ret
    (conds ((.containsKey Compiler/specials form) (RT/list compiler-quote form))
      ((instance? Symbol form)
       (do
        (RT/list
         compiler-quote
         (let [sym (cast Symbol form)]
          (conds
            ((and (nil? (.getNamespace sym)) (.endsWith (.getName sym) "#"))
             (let [gmap (.deref lisp-gensym-env)]
              (when (nil? gmap)
               (throw (IllegalStateException. "Gensym literal not in syntax-quote")))
              (let [gs (cast Symbol (.valAt gmap sym))]
               (if (nil? gs)
                (let ((gs (Symbol/intern
                           nil
                           (str
                            (.substring (.getName sym)
                                        0
                                        (- (.length (.getName sym)) 1))
                            "__"
                            (clojure.lang.RT/nextID)
                            "__auto__"))))
                 (.set lisp-gensym-env (.assoc gmap sym gs))
                 gs)
                gs))))
           ((and (nil? (.getNamespace sym)) (.endsWith (.getName sym) "."))
            (Symbol/intern
             nil
             (str
              (.getName
               (compiler-resolve-symbol
                nil (Symbol/intern
                     nil
                     (.substring (.getName sym)
                                 0
                                 (- (.length (.getName sym)) 1)))))
              ".")))
           ((and (nil? (.getNamespace sym)) (.startsWith (.getName sym) "."))
            ;; Simply quote method names
            sym)
           (else ;; This has been modified to not qualify the symbol
            sym))))))
     ((isUnquote form) (RT/second form))
     ((isUnquoteSplicing form) (throw (IllegalStateException. "splice not in a list")))
     ((instance? IPersistentCollection form)
      (conds ((instance? IRecord form) form)
        ((instance? IPersistentMap form)
         (RT/list lisp-apply lisp-hashmap (RT/list lisp-seq (RT/cons lisp-concat (sqExpandList (.seq (cast IPersistentVector (flattenMap form))))))))
       ((instance? IPersistentVector form)
        (RT/list lisp-apply lisp-vector (RT/list lisp-seq (RT/cons lisp-concat (sqExpandList (.seq (cast IPersistentVector form)))))))
       ((instance? IPersistentSet form)
        (RT/list lisp-apply lisp-hashset (RT/list lisp-seq (RT/cons lisp-concat (sqExpandList (.seq (cast IPersistentSet form)))))))
       ((or (instance? ISeq form) (instance? IPersistentList form))
        (let [seq (RT/seq form)]
         (if (nil? seq)
          (RT/cons lisp-list nil)
          (RT/list sane-seq (RT/cons lisp-concat (sqExpandList seq))))))
       (else (throw UnsupportedOperationException "Unknown Collection type"))))
     ((or (instance? Keyword form) (instance? Number form) (instance? Character form) (instance? String form)) form)
     (else (RT/list compiler-quote form)))]
  (if (and (instance? IObj form) (not (nil? (RT/meta form))))
   (let [newMeta (.without (.without (.meta (cast IObj form)) (get-field RT "LINE_KEY")) (get-field RT "COLUMN_KEY"))]
    (if (> (.count newMeta) 0)
     (RT/list lisp-with-meta ret (syntaxQuote (.meta (cast IObj form))))
     ret))
   ret)))

;; Adds syntax for #`. This is a quasiquote and works just like `
;; except that it does not qualify symbols with the current namespace.
;; This default behaviour leads to a lot of strange and broken code.
;; Try out `a vs #`a (you would need to do `~'a to get this behaviour
;; otherwise.
(dispatch-reader-macro \` syntax-quote)
