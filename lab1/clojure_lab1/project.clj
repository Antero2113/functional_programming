(defproject clojure-lab1 "0.1.0-SNAPSHOT"
  :description "Project Euler Problem 5"
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :main core
  :java-source-paths ["java"]
  :source-paths ["src"]
  :aot :all
  :prep-tasks [["javac"] "compile"])