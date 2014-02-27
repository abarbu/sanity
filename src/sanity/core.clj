(ns sanity.core
 "Various improvements to clojure"
 (:require [clojure.tools.macro :refer [name-with-attributes]]
           [clojure.walk :refer [prewalk]]
           [net.n01se.clojure-jna :as jna]
           [potemkin.namespaces :as p]
           [me.raynes.fs :as fs]
           [clojure.string :as s]
           [taoensso.timbre :as log]
           [clansi.core :as color]))

(use 'sanity.improvements)

(import java.io.File)

;;; Namespaces

(defmacro import-all
 "Imports a list of variables from a namespace"
 [namespace]
 (let [syms (map #(symbol (str namespace "/" (first %)))
                 (ns-publics (the-ns namespace)))]
  `(do
    ~@(map
       (fn [sym]
        (when (and (resolve (symbol (name (symbol sym))))
                   (not (= (str (:ns (meta (resolve (symbol (name (symbol sym)))))))
                           (str namespace))))
         (print (str "WARNING: symbol already refers to: " (resolve (symbol (name (symbol sym))))
                     " in namespace: " (str (:ns (resolve (symbol (name (symbol sym))))))
                     ", being replaced by:" (str sym)))
         (newline))
        (let [vr (resolve sym)
              m (meta vr)]
         (cond
          (:macro m) `(p/import-macro ~sym)
          (:arglists m) `(p/import-fn ~sym)
          :else `(p/import-def ~sym))))
       syms))))

(define (eval-in-namespace e n)
 "Eval the expression in a namespace. The namespace is automatically
  created if it does not exist."
 (let ((current *ns*))
  (binding [*ns* (create-ns n)]
   (eval e))))

(defmacro combine-namespaces
 "Combines the given namespaces under a single name"
 [id & namespaces]
 `(do ~@(map (fn [x] `(require '~x)) namespaces)
      (eval-in-namespace
       '(do ~@(map (fn [x] `(sanity.core/import-all ~x)) namespaces))
       '~id)))

;;; Features

;; defnk by Meikel Brandmeyer:
;; TODO Provide access to the argument map
;; TODO Error on unknown argument?
;; TODO Error on incorrect number of non-keyword arguments
(defmacro defnk
 "Define a function accepting keyword arguments. Symbols up to the first
 keyword in the parameter list are taken as positional arguments.  Then
 an alternating sequence of keywords and defaults values is expected. The
 values of the keyword arguments are available in the function body by
 virtue of the symbol corresponding to the keyword (cf. :keys destructuring).
 defnk accepts an optional docstring as well as an optional metadata map."
 [fn-name & fn-tail]
 (let [[fn-name [args & body]] (name-with-attributes fn-name fn-tail)
       [pos kw-vals]           (split-with symbol? args)
       syms                    (map #(-> % name symbol) (take-nth 2 kw-vals))
       values                  (take-nth 2 (rest kw-vals))
       sym-vals                (apply hash-map (interleave syms values))
       de-map                  {:keys (vec syms)
                                :or   sym-vals}]
   `(defn ~fn-name
      [~@pos & options#]
      (let [~de-map (apply hash-map options#)]
        ~@body))))

(defmacro letrec [bindings & body]
 (let [bcnt (quot (count bindings) 2)
       arrs (gensym "bindings_array")
       arrv `(make-array Object ~bcnt)
       bprs (partition 2 bindings)
       bssl (map first bprs)
       bsss (set bssl)
       bexs (map second bprs)
       arrm (zipmap bssl (range bcnt))
       btes (map #(prewalk (fn [f]
                            (if (bsss f)
                             `(aget ~arrs ~(arrm f))
                             f))
                           %)
                 bexs)]
  `(let [~arrs ~arrv]
    ~@(map (fn [s e]
            `(aset ~arrs ~(arrm s) ~e))
           bssl
           btes)
    (let [~@(mapcat (fn [s]
                     [s `(aget ~arrs ~(arrm s))])
                    bssl)]
     ~@body))))

(defmacro letrec-trampoline [bindings & body]
 (let [bcnt (quot (count bindings) 2)
       arrs (gensym "bindings_array")
       arrv `(make-array Object ~bcnt)
       bprs (partition 2 bindings)
       bssl (map first bprs)
       bsss (set bssl)
       bexs (map second bprs)
       arrm (zipmap bssl (range bcnt))
       btes (map #(prewalk (fn [f]
                            (if (bsss f)
                             `(aget ~arrs ~(arrm f))
                             f))
                           %)
                 bexs)]
  `(let [~arrs ~arrv]
    ~@(map (fn [s e]
            `(aset ~arrs ~(arrm s) ~e))
           bssl
           btes)
    (let [~@(mapcat (fn [s]
                     [s `(aget ~arrs ~(arrm s))])
                    bssl)]
     ~@body))))

(defn chdir [directory]
 "Change the directory of the JVM. This is not something the JVM
  supports out of the box, and your mileage may vary."
 (jna/invoke Integer c/chdir directory)
 (System/setProperty "user.dir" directory))

;;; Numbers

(def Infinity Double/POSITIVE_INFINITY)
(def -Infinity Double/NEGATIVE_INFINITY)
(def +infinity Double/POSITIVE_INFINITY)
(def -infinity Double/NEGATIVE_INFINITY)

(defn sqrt [x] (Math/sqrt x))
(defn exp [x] (Math/exp x))
(defn expt [x y] (Math/pow x y))
(defn sin [x] (Math/sin x))
(defn log [x] (Math/log x))
(defn cos [x] (Math/cos x))
(defn tan [x] (Math/tan x))
(defn atan [x y] (Math/atan2 x y))
(defn abs [x] (Math/abs x))
(defn round [^double x] (Math/round x))
(defn floor [x] (Math/floor x))
(defn ceil [x] (Math/ceil x))

(defn sqr [x] "Square the given number" (* x x))

(def pi Math/PI)
(def half-pi (/ pi 2.0))
(def two-pi (* 2.0 pi))
(def minus-pi (- pi))
(def two-pi-360 (/ two-pi 360.0))
(def three-sixty-two-pi (/ 360.0 two-pi))

(defn degrees->radians [angle] (* two-pi-360 angle))
(defn radians->degrees [angle] (* three-sixty-two-pi angle))

(defn normalize-rotation [rotation]
 "Normalize a rotation in radians to be in the internal [π,-π)"
 (conds ((> rotation pi) (normalize-rotation (- rotation two-pi)))
   ((<= rotation minus-pi) (normalize-rotation (+ rotation two-pi)))
  (else rotation)))
(defn rotation+ [x y] "Add two angles and normalize the result" (normalize-rotation (+ x y)))
(defn rotation- [x y] "Subtract two angles and normalize the result" (normalize-rotation (- x y)))
(defn angle-separation [x y] "Compute the separation between two angles"
 (min (abs (rotation- x y)) (abs (rotation- y x))))

(defn orientation [v] "Computes the orientation of a vector in radians."
 (atan (nth v 1) (nth v 0)))

;;; Workarounds for strange semantics and missing basic functionality

(define (null? c)
 "Is this collection empty? nil is considered empty because
 ((fn [& a] a)) returns nil and not an empty collection"
 (or (empty? c) (nil? c)))

(define (sequence? c)
 "Is this a sequence? nil is considred a sequence because
 ((fn [& a] a)) returns nil and not an empty sequence"
 (or (seq? c)
     ;; This is required because ((fn [& a] a)) returns nil, not an empty collection
     (nil? c))) 

(defn re-seq' [^java.util.regex.Pattern re s]
 "Returns a lazy sequence of successive matches of pattern in string.
  Similar to clojure.core/re-seq but produces the match groups as well as start and
  end indices. Returns a list of maps {:groups ... :start ... :end ...}"
 (let [m (re-matcher (if (string? re) (re-pattern re) re) s)]
  ((fn step []
    (when (. m find)
     (cons {:groups (re-groups m) :start (. m start) :end (. m end)}
           (lazy-seq (step))))))))

(defn re-seq:overlapping' [^java.util.regex.Pattern re s]
 "Returns a lazy sequence of successive matches of pattern in string.
  Similar to clojure.core/re-seq but produces the match groups as well as start and
  end indices. Returns a list of maps {:groups ... :start ... :end ...}.
  It also handles overlapping matches correctly."
 (map #(update-in % [:groups] rest) (re-seq' (str "(?=(" re "))") s)))

(defn seqable?
 "Returns true if (seq x) will succeed, false otherwise."
 [x]
 (or (seq? x)
     (instance? clojure.lang.Seqable x)
     (nil? x)
     (instance? Iterable x)
     (-> x .getClass .isArray)
     (string? x)
     (instance? java.util.Map x)))

(defn substring? [sub ^String str] "Is the first argument a substring of the second?"
 (.contains str sub))
(def substring
 "Alias for clojure.core/subs. ([str start-index] [str start-index end-index])"
 subs)

(defn third [x] (nth x 2))
(defn fourth [x] (nth x 3))
(defn fifth [x] (nth x 4))
(defn sixth [x] (nth x 5))
(defn seventh [x] (nth x 6))
(defn eighth [x] (nth x 7))
(defn ninth [x] (nth x 8))

(defn string-prefix? [^String p ^String s] "Is the first argument a prefix of the second?" (.startsWith s p))
(defn string-suffix? [^String p ^String s] "Is the first argument a suffix of the second?" (.endsWith s p))

;; TODO This needs a better implementation
(defn for-each [f l] "map for side effects. Forces and discards the result." (dorun (map f l)))
(defn for-each-indexed [f l]
 "map for side effects with an index (second argument to f). Forces and discards the result."
 (dorun (map f l (range 0 (count l)))))

(defn map-vector [f v & vs]
 "Map over collections and produce a vector"
 (loop [r [] i 0]
  (if (= i (count v))
   r
   (recur (conj r (apply f (nth v i) (map (fn [v] (nth v i)) vs))) (+ i 1)))))

;; TODO Document me
(defn map-m-n [f m n] (loop ((i m) (c [])) (if (> i n) c (recur (+ i 1) (conj c (f i))))))
(defn for-each-m-n [f m n] (loop ((i m)) (if (> i n) nil (do (f i) (recur (+ i 1))))))
(defn for-each-m-n-dec [f m n] (loop ((i m)) (if (< i n) nil (do (f i) (recur (- i 1))))))
(defn map-n [f n] (map f (range 0 n)))
(defn map-n-vector [f n] (map-vector f (range 0 n)))

(defn map-reduce
 "Map f over the collection and reduce (foldl) with the binary function g."
 ([g f l] (reduce g (map f l)))
 ([g i f l & ls] (reduce g i (apply map f l ls))))

(defn read-object-from-file [file]
 "Read a clojure object from the given file. Note that this uses the
 builtin reader and may be unsafe to run on untrusted
 files!"
 (read-string (slurp file)))

(defn write-object-to-file [object file]
 "Write an object to a file. Sadly the object will not be
  pretty-printed because pprint is far too slow."
 ;; TODO Geeze.. this is slow.. pprint is painful
 ;; (let [w (StringWriter.)]
 ;;  (pprint object w)
 ;;  (spit file (.toString w)))
 (spit file object))

(defn zip [a b & cs]
 "Zip two or more lists into ne"
 (map reverse (reduce (lambda (a b) (map cons b a)) (map list a) (cons b cs))))

(defn unzip [l]
 "Unzip a list into a list of lists"
 (if (empty? l)
  '()
  (map-n (lambda (i) (map (lambda (e) (nth e i)) l)) (count (first l)))))

(defn read-lines [filename]
 "Read the file and split at newlines"
 (s/split (slurp filename) #"\n"))
(defn read-text-file [filename]
 "Alias for slurp"
 (slurp filename))

(defn write-lines [lines filename]
 "Write a list of strings to a file inserting newlines"
 (spit filename (s/join "\n" lines)))
(defn write-text-file [s filename]
 "Alias for spit"
 (spit filename s))

(defn sum
 "One of several operations depending on the arguments. Sum a single
  collection. If given a functiona and a collection apply the function
  to the collection and sum the results. If given a function a
  number sum_i=0^number f(i)."
 ([l] (reduce + 0 l))
 ([a l & ls]
    (cond (and (fn? a) (number? l) (empty? ls))
          (let [f a n l]
           (loop [n (- n 1) c 0]
            (if (< n 0)
             c
             (recur (- n 1) (+ c (f n))))))
          (fn? a) (apply map-reduce + 0 a l ls))))

(defn product
 "One of several operations depending on the arguments. Take the
  product of all values in a single collection. If given a functiona
  and a collection apply the function to the collection and take the
  product of the results. If given a function a number
  product_i=0^number f(i)."
 ([l] (reduce * 1 l))
 ([a l & ls]
    (cond (and (fn? a) (number? l) (empty? ls))
          (let [f a n l]
           (loop [n (- n 1) c 1]
            (if (< n 0)
             c
             (recur (- n 1) (* c (f n))))))
          (fn? a) (apply map-reduce * 1 a l ls))))

(defn map-pairs [f l] (map (fn [b a] (f a b)) (rest l) l))

(defn every? [p l & ls]
 "Is p true of every element of the collection(s) l(s)?"
 (loop [l l ls ls]
  (or (null? l)
      (and (apply p (first l) (map first ls))
           (recur (rest l) (map rest ls))))))

(define (every-other list)
 "Take every other element of a list."
 (conds ((null? list) '())
   ((null? (rest list)) list)
  (else (cons (first list) (every-other (rest (rest list)))))))

(defn update-values [f m]
 "Map f over the values in the map m. A very inefficient operation at
  the moment."
 (reduce (fn [r [k v]] (assoc r k (f v))) {} m))

(defn map-linear [f s e n]
 "Interpolate linearly between s and e with n steps and call f on each value."
 (map-n (lambda (v) (f (+ s (* v (/ (- e s) n))))) (+ 1 n)))

(defn strip-extension [^String path]
 "Remove the extension from a path."
 (let [i (.lastIndexOf path ".")]
  (if (pos? i) (subs path 0 i) path)))

(defn strip-directory [^String path]
 "Remove the directory from a path."
 (fs/base-name path))

(defn extension [^String path]
 "Return the extension of a path."
 (s/join (rest (fs/extension path))))

(defn replace-extension [^String path extension]
 "Replace the extension (add one if one does not exist) of a path."
 (let [i (.lastIndexOf path ".")]
  (if (pos? i)
   (str (subs path 0 i) "." extension)
   (str path "." extension))))

(defn directory [^String path]
 "Get the directory portion of a path"
 (let ((p ^File (fs/parent path))) (.getPath p)))

(defn file-change-time [^String filename]
 "Return the last modified time of a file in seconds since epoch"
 (/ (.lastModified (java.io.File. filename)) 1000))

(define (with-temporary-file filename f)
 ;; TODO Pad file name to three characters
 "Create a temporary file and call f with its name. The filename will
  be used as a template, and must be at least three characters. It
  will be deleted once f returns"
 (let ((file ^File (File/createTempFile
                    (strip-extension (strip-directory filename))
                    (if (= (extension filename) "")
                     ""
                     (str "." (extension filename)))
                    (fs/file (directory filename))))
       (result (f (.getPath file))))
  (.delete file)
  result))

;;; Timbre

(defn setup-timbre-format []
 "Replace the current timbre fmt-output-fn with one that is more
  verbse, provides colored output depending on the log level, and
  determines the class and function-name of the source of the log
  message."
 (log/set-config!
  [:fmt-output-fn]
  (fn [{:keys [level throwable message timestamp hostname ns]}
       ;; Any extra appender-specific opts:
       & [{:keys [nofonts?] :as appender-fmt-output-opts}]]
   (format "%s %s -- %s%s"
           (color/style (-> level name s/upper-case)
                        (case level
                         :trace  :blue
                         :debug  :cyan
                         :info   :green
                         :warn   :magenta
                         :error  :red
                         :fatal  :bg-red
                         :report :yellow))
           (s/replace
            (third (drop-while #(not (= "taoensso.timbre$send_to_appenders_BANG_" %))
                               (map #(.getClassName %) (.getStackTrace (Throwable.)))))
            #"^([^$]+)\$" "$1/")
           (or message "")
           (or (log/stacktrace throwable "\n" (when nofonts? {})) "")))))

;;; Scheme compatibility, makes porting code over easier

(def string-join "Alias for s/join" s/join)

(defn const [a] "Create a constant function" (fn [& _] a))

(def o "Alias for clojure.core/comp. Function composition." comp)

(def eq? "Alias for clojure.core/identical?" identical?)

(def foldl "Left fold. Alias for clojure.core/reduce. ([f coll] [f val coll])" reduce)
(defn foldr "Right fold. Rather inefficient at the moment and
implemented in terms of a left fold and reduce."
 ([f coll] (reduce #(f %2 %1) (reverse coll)))
 ([f val coll] (reduce #(f %2 %1) val (reverse coll))))

(defn position-if [p l]
 "Return the index of the first position where predicate p is true in collection l."
 (loop ((l l) (i 0))
  (conds ((null? l) false)
    ((p (first l)) i)
   (else (recur (rest l) (+ i 1))))))
(defn position-if-not [p l]
 "Return the index of the first position where predicate p is false in collection l."
 (loop ((l l) (i 0))
  (conds ((null? l) false)
    ((p (first l)) (recur (rest l) (+ i 1)))
   (else i))))
(defn find-if [pred coll]
 "Return a value from collection coll where predicate pred is true."
 (when (seq coll)
  (if (pred (first coll))
   (first coll)
   (recur pred (rest coll)))))

(def length clojure.core/count)
(def vector-length clojure.core/count)
(def string-length clojure.core/count)

(def vector-ref "Alias for clojure.core/nth" nth)
(def list-ref "Alias for clojure.core/nth" nth)
(def string-ref "Alias for clojure.core/nth" nth)

(def list->string "Alias for s/join" s/join)
(def string->list "Alias for clojure.core/seq" seq)

(defn string->number [s]
 "Parse the string as a Double. Convert to an exact if possible. If
  the parse fails return false."
 (try (let [x (Double/parseDouble s)]
       (if (== x (round x))
        (round x)
        x))
      (catch Exception e false)))
(def number->string "Alias for clojure.core/str" str)

(def list->vector vec)
(def vector->list seq)

(defn remove-if [f l] "Remove every element of l where f is true." (remove f l))
(defn remove-if-not [f l] "Remove every element of l where f is true." (filter f l))

(def string->list "Alias for clojure.core/seq" seq)

(def equal? "Alias for clojure.core/=" =)

(defn position-if [p l]
 (loop [l l i 0]
  (conds ((null? l) false)
    ((p (first l)) i)
   (else (recur (rest l) (+ i 1))))))

(def append "Append two collections, alias for clojure.core/concat" concat)
(def string-append str)

(defn join* [l] "Takes a collection of collections and flattens one level." (reduce append '() l))

(defn remove-duplicates
 ([x] (remove-duplicates = x))
 ([p x]
    "Remove duplicates (as determined by the binary predicate p) from collection x"
    (loop ((x x) (c '()))
     (conds ((null? x) (reverse c))
      ((some #(p (first x) %) c) (recur (rest x) c))
      (else (recur (rest x) (cons (first x) c)))))))

(def car first)
(def cdr rest)

(defn format* [& more] "format and print" (print (apply format more)))

(defn replace-ith-vector [x i xi] (assoc x i xi))

(defn enumerate [n] (map-n identity n))

(def function? fn?)
(def procedure? fn?)

(defn maximum-with-position
 "Find the largest value and return (list value position). Takes
  either a collection, or an objective function and a collection."
 ([l'] (maximum-with-position identity l'))
 ([f l']
    (when (not (null? l'))
     (loop [l (cdr l') x (car l') i 0 j 0]
      (if (null? l)
       (list x j)
       (if (> (f (car l)) (f x))
        (recur (cdr l) (car l) (+ i 1) (+ i 1))
        (recur (cdr l) x (+ i 1) j)))))))

(defn minimum-with-position
 "Find the smallest value and return (list value position). Takes
  either a collection, or an objective function and a collection."
 ([l'] (minimum-with-position identity l'))
 ([f l'] (maximum-with-position (fn [x] (- (f x))) l')))

(defn maximum
 "Find the largest value and return it. Takes
  either a collection, or an objective function and a collection."
 ([l'] (first (maximum-with-position identity l')))
 ([f l'] (first (maximum-with-position f l'))))

(defn minimum
 "Find the smallest value and return it. Takes
  either a collection, or an objective function and a collection."
 ([l'] (minimum identity l'))
 ([f l'] (maximum (fn [x] (- (f x))) l')))

(defn maximum-position
 "Find the largest value and return its position. Takes
  either a collection, or an objective function and a collection."
 ([l'] (second (maximum-with-position identity l')))
 ([f l'] (second (maximum-with-position f l'))))

(defn minimum-position
 "Find the smallest value and return its position. Takes
  either a collection, or an objective function and a collection."
 ([l'] (minimum-position identity l'))
 ([f l'] (maximum-position (fn [x] (- (f x))) l')))
