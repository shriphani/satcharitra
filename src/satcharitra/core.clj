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

(defn process-chapter
  [chapter-body]
  ; selectors for portions.
  )

(defn process-dataset
  [data-file]
  (let [corpus (-> data-file
                   slurp
                   read-string)
        test-chapter (get corpus "CHAPTER I")]
    (process-chapter test-chapter)))
