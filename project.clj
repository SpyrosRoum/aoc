(defproject aoc2024 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/tools.trace "0.8.0"]
                 [org.clojure/core.memoize "1.1.266"]
                 [org.clojure/math.combinatorics "0.3.0"]
                 [com.clojure-goes-fast/clj-async-profiler "1.5.1"]]
  :main ^:skip-aot aoc2024.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}
  :jvm-opts ["-Djdk.attach.allowAttachSelf"])
