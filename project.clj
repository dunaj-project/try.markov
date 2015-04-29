(defproject try-dunaj/markov "0.1.0-SNAPSHOT"
  :description "Dunaj tutorial: Markov chain"
  :url "https://github.com/dunaj-project/try.markov"
  :scm {:name "git"
        :url "https://github.com/dunaj-project/try.markov"}
  :source-paths ["src"]
  :resource-paths ["res"]
  :java-source-paths ["src/jvm"]
;;  :global-vars {*warn-on-reflection* true}
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "same as Clojure"}
  :dependencies
  [[org.dunaj/dunaj "0.4.0"]
   [rhizome "0.2.5"]
   [criterium "0.4.3" :exclusions [[org.clojure/clojure]]]
   [incanter "1.5.6" :exclusions [[org.clojure/clojure]]]]
  :jvm-opts ^:replace ["-Xms1G" "-Xmx1G"
                       "-XX:-UseConcMarkSweepGC" "-server"])
