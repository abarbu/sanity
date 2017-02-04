(defproject com._0xab/sanity "1.12.0"
 :description "Fixes a number of problems with Clojure semantics and syntax and fills in some missing functionality"
 :url "https://github.com/abarbu/sanity"
 :license {:name "Eclipse Public License"
           :url "http://www.eclipse.org/legal/epl-v10.html"}
 :dependencies [[org.clojure/clojure "1.8.0"]
                [potemkin "0.4.3"]
                [org.clojure/tools.macro "0.1.5"]
                [me.raynes/fs "1.4.6"]
                [com.taoensso/timbre "4.8.0"]
                [clj-native "0.9.5"]
                ;; non-canonical fork because upstream hasn't been updated since 1.0
                ;; but this is the original dev
                [radicalzephyr/clansi "1.2.0"]]
 :codox {:src-dir-uri "http://github.com/abarbu/sanity/blob/master/"
         :src-linenum-anchor-prefix "L"}
 :scm {:name "git"
       :url "https://github.com/abarbu/sanity"})
