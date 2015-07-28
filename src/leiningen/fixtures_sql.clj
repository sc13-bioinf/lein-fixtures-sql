(ns leiningen.fixtures-sql
  :require [clojure.tools.cli :refer [parse-opts]])

(def cli-options
  [["--config CONFIG" "Properties file configuring datasources from which to generate fixtures"]
  ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["This is my program. There are many like it, but this one is mine."
        ""
        "Usage: program-name [options] action"
        ""
        "Options:"
        options-summary
        (string/join \newline)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn fixtures-sql
  "I don't do a lot."
  [project & args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond (:help options) (exit 0 (usage summary))
          (exit 1 (usage summary)))))
