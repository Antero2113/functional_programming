(defproject clojure-lab1 "0.1.0-SNAPSHOT"
  :description "Project Euler Problems"
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :main core
  :java-source-paths ["java"]
  :source-paths ["src"]
  :test-paths ["test"]
  :aot :all
  :prep-tasks [["javac"] "compile"])