(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'com.github.stepbeekio/sudoku-solver)
(def version "0.1.0-SNAPSHOT")
(def class-dir "target/classes")
(def uber-file (format "target/%s-%s.jar" (name lib) version))

(defn uber
  "Build an uberjar. It has no :main yet because the project has no -main."
  [_]
  (let [basis (b/create-basis {:project "deps.edn"})]
    (b/delete {:path "target"})
    (b/copy-dir {:src-dirs ["src" "resources"] :target-dir class-dir})
    (b/compile-clj {:basis basis :src-dirs ["src"] :class-dir class-dir})
    (b/uber {:basis basis :class-dir class-dir :uber-file uber-file})
    (println "Built" uber-file)))
