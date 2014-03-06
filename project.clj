(defproject com._0xab/sanity "1.2.0"
 :description "Fixes a number of problems with Clojure semantics and syntax and fills in some missing functionality"
 :url "https://github.com/abarbu/sanity"
 :license {:name "Eclipse Public License"
           :url "http://www.eclipse.org/legal/epl-v10.html"}
 :dependencies [[org.clojure/clojure "1.5.1"]
                [potemkin "0.3.4"]
                [org.clojure/tools.macro "0.1.5"]
                [me.raynes/fs "1.4.5"]
                [net.n01se/clojure-jna "1.0.0"]
                [com.taoensso/timbre "3.0.0-SNAPSHOT"]
                ;; non-canonical fork because upstream hasn't been updated since 1.0
                ;; but this is the original dev
                [org.clojars.rosejn/clansi "1.2.0-SNAPSHOT"]]
 :codox {:src-dir-uri "http://github.com/abarbu/sanity/blob/master/"
         :src-linenum-anchor-prefix "L"}
 :scm {:name "git"
       :url "https://github.com/abarbu/sanity"})
