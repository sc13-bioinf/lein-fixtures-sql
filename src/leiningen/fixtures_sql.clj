(ns leiningen.fixtures-sql
  (:require [clojure.string :as str]
            [clojure.tools.logging :refer [info error]]
            [clojure.tools.cli :refer [cli]]
            [clojure.java.io :as io]
            [leiningen.help :as help])
  (:import [java.lang System]
           [java.io StringWriter PrintWriter]))

(defn not-nil? [maybe-nil] (not (nil? maybe-nil)))

(defn properties->map
  "Convert the properties list to a map, transforming the value
  whenever a transformer is defined for that key."
  ([properties]
     (properties->map properties {}))
  ([properties transformer-for]
     (letfn [(keyfor [s] (mapv keyword (str/split (key s) #"\.")))]
       (reduce (fn [accum entry]
                 (let [k (keyfor entry)]
                   (if-let [f (transformer-for k)]
                     (assoc-in accum k (f (val entry)))
                     (assoc-in accum k (val entry)))))
               {} properties))))

(defn load-properties
  "Load named properties file from the classpath, return a map with
  values optionally transformed by `transform-map`."
  ([resource-name]
     (load-properties resource-name {}))
  ([resource-name transform-map]
     (when-let [resource (io/resource resource-name)]
       (let [properties (doto (java.util.Properties.) (.load (io/input-stream resource)))]
         (properties->map properties transform-map)))))

(def transform-map {[:e-mail :exception-handler :recipient] #(str/split % #",")})

(defn read-config
  "Read `config-file`, apply `config-transformers`"
  [config-file]
  (load-properties config-file transform-map))

(defn cfg-get-in
  ([config ks]
    (cfg-get-in ks nil))
  ([config ks default]
    (clojure.core/get-in config ks default)))

(defn- stack-trace-to-string
  "Returns a string containing the output of .printStackTrace"
  [t]
  (let [sw (StringWriter.)
        pw (PrintWriter. sw)
        _ (.printStackTrace t pw)]
    (.toString sw)))

(defn- exception-to-string
  "Convert and exception to a string"
  [e]
  (let [date-format (java.text.SimpleDateFormat. "yy-MM-dd HH:mm")
        date-string (.format date-format (java.util.Date.))]
    (str/join "" ["The application caught the following exception at "
                  date-string
                  (System/getProperty "line.separator")
                  (System/getProperty "line.separator")
                  "Exception caught:"
                  (System/getProperty "line.separator")
                  (.getMessage e)
                  (System/getProperty "line.separator")
                  (System/getProperty "line.separator")
                  "Stack trace:"
                  (System/getProperty "line.separator")
                  (stack-trace-to-string e)])))

(defn- exit [status msg]
  (info "called exit")
  (println msg)
  (System/exit status))

(defn extract-resource-paths
  [project accumulator item]
  (conj accumulator (get-in project item)))

(defn search-resource-paths
  [resource-paths config-file-name]
  (when-let [config-file (first (filter #(.exists %) (map (fn [rp] (java.io.File. (str/join (System/getProperty "file.separator") [rp config-file-name]))) resource-paths)))]
    (.getPath config-file)))

(defn resolve-config-file
  "Locate the config file by looking in the projects resources dirs"
  [project config-file-path]
  (let [resource-paths (flatten (filter not-nil? (reduce (partial extract-resource-paths project) [] [[:profiles :test :resource-paths] [:resource-paths]])))
        _ (info "Searching resource-paths: " resource-paths " for " config-file-path)
        fn-parts (str/split config-file-path (re-pattern (System/getProperty "file.separator")))]
    (cond
     (= (first fn-parts) "resources")
     (search-resource-paths resource-paths (str/join (System/getProperty "file.separator") (rest fn-parts)))
     (= (count fn-parts) 1)
     (search-resource-paths (conj resource-paths "") (first fn-parts))
     :else config-file-path)))

(defn- create-fixtures
  "Create fixtures files based on the configuration in the properties file"
  [project options]
  (let [resolved-config-file-path (resolve-config-file project (:config options))
        _ (info (str/join "" ["Reading config from '" resolved-config-file-path "'"]))]
  (if-let [config (read-config resolved-config-file-path)]
    (let [databases ""
          _ (info "databases: " databases)]
      )
    (println "No properties file found!" (System/getProperty "line.separator") (System/getProperty "line.separator") (help/help-for "fixtures-sql")))))

(defn fixtures-sql
  "Create fixtures from your sql databases. Fixtures are configured in a properties file:
      resources/fixtures-sql.properties

  Usage:
      lein fixtures-sql
      lein fixtures-sql --config fixtures.properties"
  [project & args]
  (try
    (let [[options arguments summary] (cli args ["-c" "--config" "Properties file configuring datasources from which to generate fixtures" :default "resources/fixtures-sql.properties"])]
      (create-fixtures project options))
    (catch Exception e
      (println (exception-to-string e)))))
