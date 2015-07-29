(defproject lein-fixtures-sql "0.1.0-SNAPSHOT"
  :description "Generate sql fixtures from your production databases"
  :url "https://github.com/sc13-bioinf/lein-fixtures-sql"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/tools.cli "0.3.1"]
                 [org.clojure/tools.logging "0.3.1"]
                 [lein-light-nrepl "0.0.12"]]
  :repl-options {:nrepl-middleware [lighttable.nrepl.handler/lighttable-ops]}
  :eval-in-leiningen true)
