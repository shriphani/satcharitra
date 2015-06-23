(ns satcharitra.core
  (:require [clj-http.client :as client]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [net.cgrand.enlive-html :as html]
            [org.bovinegenius.exploding-fish :as uri])
  (:import [java.io StringReader]))

(defn grab-chapter-links
  [root-link]
  (let [chapter-tags (-> root-link
                         (java.net.URL.)
                         html/html-resource
                         (html/select [:table :tbody :tr :td :a]))
        links
        (map
         (fn [t]
           (->> t :attrs :href (uri/resolve-uri root-link)))
         chapter-tags)

        text
        (map html/text chapter-tags)]
    (map vector links text)))

(defn download-chapter
  [chapter-link]
  (-> chapter-link
      client/get
      :body))

(defn grab-dataset
  [root-link corpus-file]
  (let [links-text (grab-chapter-links root-link)]
    (with-open [wrtr (io/writer corpus-file)]
      (pprint
       (reduce
        (fn [acc [link text]]
          (merge acc {text (download-chapter link)}))
        {}
        links-text)
       wrtr))))

(defn html->body-map
  [page-src]
  (let [pg-html (->> page-src
                     java.io.StringReader.
                     html/html-resource
                     (filter (fn [x] (:tag x)))
                     first)]
    (first
     (filter
      (fn [descendant]
        (= (:tag descendant)
           :body))
      (:content pg-html)))))

(defn leaf-paths
  "Return a path from root to leaf"
  ([a-map]
   (leaf-paths a-map
               :tag))
  ([a-map leaf-op]
   (let [descendants (filter map? (:content a-map))
         txt-content (filter #(-> % map? not)
                             (:content a-map))]

     (if (empty? descendants)
       [[(leaf-op a-map)]]
       (reduce
        concat
        []
        (map (fn [a-child]
               (let [all-paths (leaf-paths a-child)]
                 (map
                  (fn [a-path]
                    (cons (:tag a-map) a-path))
                  all-paths)))
             descendants))))))

(defn produce-histogram
  [data-file]
  (let [corpus (-> data-file
                   slurp
                   read-string)
        chapter-paths  (map
                        (fn [[chapter body]]
                          (-> body html->body-map leaf-paths set))
                        corpus)

        path-dfs
        (reduce
         (fn [acc paths]
           (let [path-freq (into
                            {}
                            (map
                             (fn [path]
                               [path 1])
                             paths))]
             (merge-with + acc path-freq)))
         {}
         chapter-paths)]
    (sort-by second path-dfs)))

