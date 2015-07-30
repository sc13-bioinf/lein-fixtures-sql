(defproject org.clojars.sc13-bioinf/lein-fixtures-sql "0.0.3"
  :description "Generate sql fixtures from your production databases"
  :url "https://github.com/sc13-bioinf/lein-fixtures-sql"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/tools.cli "0.3.1"]
                 [org.clojure/tools.logging "0.3.1"]
                 [me.raynes/conch "0.8.0"]
                 [lein-light-nrepl "0.0.12"]]
  :repl-options {:nrepl-middleware [lighttable.nrepl.handler/lighttable-ops]}
  :eval-in-leiningen true)
