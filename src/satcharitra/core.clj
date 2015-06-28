(ns satcharitra.core
  (:require [clj-http.client :as client]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [net.cgrand.enlive-html :as html]
            [org.bovinegenius.exploding-fish :as uri])
  (:import [java.io StringReader]
           [java.util.regex Matcher]
           [org.apache.commons.lang3 StringEscapeUtils]))

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

(defn get-chapter-names
  [root-link]
  (let [chapter-tags (-> root-link
                         (java.net.URL.)
                         html/html-resource
                         (html/select [:table :tbody :tr :td :a]))]
    (map html/text chapter-tags)))

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
         txt-content (filter
                      (fn [blob]
                        (and blob
                             (-> blob string/blank? not)))
                      (filter #(-> % map? not)
                              (:content a-map)))
         descendant-paths (reduce
                           concat
                           []
                           (map (fn [a-child]
                                  (let [all-paths (leaf-paths a-child)]
                                    (map
                                     (fn [a-path]
                                       (cons (leaf-op a-map)
                                             a-path))
                                     all-paths)))
                                descendants))]

     (cond (empty? descendants)
           [[(leaf-op a-map)]]

           (-> txt-content empty? not)
           (do (println txt-content)
               (conj descendant-paths [(leaf-op a-map)]))

           :else
           descendant-paths))))

(defn produce-histogram
  [data-file]
  (let [corpus (-> data-file
                   slurp
                   read-string)
        chapter-paths  (map
                        (fn [[chapter body]]
                          (-> body html->body-map leaf-paths set))
                        (take 1
                              corpus))

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

(def rewrite-rules
  {[[:body] [:p {:align "CENTER"}]]
   (fn [text]
     nil)

   [[:body] [:p {:align "CENTER"}] [:b nil]]
   (fn [text]
     (if (re-find #"Chapter" text)
       (str
        "\\newpage"
        "\\newgeometry{left=0cm,bottom=0cm,top=0cm,right=0cm}
        \\begingroup
        \\begin{tikzpicture}[remember picture, overlay]
        \\node[anchor=north] at (current page.north){\\pgfornament[width=6cm,symmetry=h]{46}};
        \\node[anchor=south] at (current page.south){\\pgfornament[width=6cm]{46}};
        \\node[anchor=north,rotate=90] at (current page.west){\\pgfornament[width=6cm,symmetry=h]{46}};
        \\node[anchor=north,rotate=-90] at (current page.east){\\pgfornament[width=6cm,symmetry=h]{46}};
        \\node[inner sep=6pt] (chapter) at (current page.center)"
        "{\\Huge "
        text
        " };"
        "\\node[inner sep=12pt, below of=chapter, text width=10cm, align=center, outer sep=12pt] (title1) { };"

        )
       (str
        "
        \\node[inner sep=12pt, below of=title1, text width=10cm, align=center, outer sep=12pt] (title) { "
        text
        "};
        \\node[anchor=north] at (title.south){\\pgfornament[width=5cm]{60}};
        \\node[anchor=south] at (chapter.north){\\pgfornament[width=5cm,symmetry=h]{49}};
        \\end{tikzpicture}
        \\endgroup
      \\newpage
      \\restoregeometry
"
        )))

   [[:body] [:p nil]]
   identity

   [[:body] [:p nil] [:font {:size 5}]]
   identity

   [[:body] [:p nil] [:b nil]]
   (fn [text]
     (str "\\section*{" text "}"))

   [[:body] [:p nil] [:b nil] [:font {:color "#0000FF"}]]
   (fn [text]
     (str "\\section*{" text "}"))

   [[:body] [:p {:align "CENTER"}] [:b nil] [:i nil]]
   (fn [text] nil)

   [[:body] [:p nil] [:a {:href "../saisatc.html"}]]
   (fn [text] nil)

   [[:body] [:center nil] [:p nil] [:b nil] [:font {:color "#FF0000"}]]
   (fn [text]
     (if (re-find #"Chapter" text)
      (str
       "\\newpage"
       "\\newgeometry{left=0cm,bottom=0cm,top=0cm,right=0cm}
        \\begingroup
        \\begin{tikzpicture}[remember picture, overlay]
        \\node[anchor=north] at (current page.north){\\pgfornament[width=6cm,symmetry=h]{46}};
        \\node[anchor=south] at (current page.south){\\pgfornament[width=6cm]{46}};
        \\node[anchor=north,rotate=90] at (current page.west){\\pgfornament[width=6cm,symmetry=h]{46}};
        \\node[anchor=north,rotate=-90] at (current page.east){\\pgfornament[width=6cm,symmetry=h]{46}};
        \\node[inner sep=6pt] (chapter) at (current page.center)"
       "{\\Huge "
       text
       " };
\\node[inner sep=12pt, below of=chapter, text width=10cm, align=center, outer sep=12pt] (title1) { };"

       )
      (str
       "
        \\node[inner sep=12pt, below of=title1, text width=10cm, align=center, outer sep=12pt] (title) { "
       text
       "};
        \\node[anchor=north] at (title.south){\\pgfornament[width=5cm]{60}};
        \\node[anchor=south] at (chapter.north){\\pgfornament[width=5cm,symmetry=h]{49}};
        \\end{tikzpicture}
        \\endgroup
      \\newpage
      \\restoregeometry
"
       )))

   [[:body] [:center nil] [:p nil] [:font {:color "#FF0000"}] [:b nil]]
   (fn [text]
     (if (re-find #"Chapter" text)
      (str
       "\\newpage"
       "\\newgeometry{left=0cm,bottom=0cm,top=0cm,right=0cm}
        \\begingroup
        \\begin{tikzpicture}[remember picture, overlay]
        \\node[anchor=north] at (current page.north){\\pgfornament[width=6cm,symmetry=h]{46}};
        \\node[anchor=south] at (current page.south){\\pgfornament[width=6cm]{46}};
        \\node[anchor=north,rotate=90] at (current page.west){\\pgfornament[width=6cm,symmetry=h]{46}};
        \\node[anchor=north,rotate=-90] at (current page.east){\\pgfornament[width=6cm,symmetry=h]{46}};
        \\node[inner sep=6pt] (chapter) at (current page.center)"
       "{\\Huge "
       text
       " };
\\node[inner sep=12pt, below of=chapter, text width=10cm, align=center, outer sep=12pt] (title1) { };"

       )
      (str
       "
        \\node[inner sep=12pt, below of=title1, text width=10cm, align=center, outer sep=12pt] (title) { "
       text
       "};
        \\node[anchor=north] at (title.south){\\pgfornament[width=5cm]{60}};
        \\node[anchor=south] at (chapter.north){\\pgfornament[width=5cm,symmetry=h]{49}};
        \\end{tikzpicture}
        \\endgroup
      \\newpage
      \\restoregeometry
"
       )))

   :all
   identity})

(defn get-rewrite-rule
  [item-path]
  (or (get rewrite-rules item-path)
      (get rewrite-rules :all)))

(defn format-content
  "Args:
   item: a string"
  [item item-path]
  (let [transformation (get-rewrite-rule item-path)]
    (transformation item)))

(defn convert-chapter-parse
  ([a-map]
   (->> (convert-chapter-parse a-map [])
        flatten
        (apply str)))

  ([a-map parent-path]
   (let [current-node-path (conj parent-path
                                 (if (= (:tag a-map)
                                        :body)
                                   [(:tag a-map)]
                                   [(:tag a-map) (:attrs a-map)]))
         node-contents (:content a-map)]

     (map
      (fn [an-item]
        (if (map? an-item)
          (convert-chapter-parse an-item
                                current-node-path)
          (let [fixed-item (-> an-item
                               (StringEscapeUtils/unescapeHtml3)
                               (string/replace #"\&"
                                               (Matcher/quoteReplacement "\\&")))]
            (format-content fixed-item
                            current-node-path))))
      node-contents))))

(defn convert-chapter
  [a-chapter]
  (let [chapter-map (html->body-map a-chapter)]
    (convert-chapter-parse chapter-map)))

(defn convert-book
  [book-link data-file out-book]
  (let [chapters (get-chapter-names book-link)
        data     (-> data-file slurp read-string)]
    (with-open [wrtr (io/writer out-book)]
      (doall
       (doseq [chapter chapters]
         (let [ch-text (->> chapter
                            (get data)
                            convert-chapter)

               fixed-text (if (not (re-find #".end.tikzpicture."
                                            ch-text))
                            (string/replace ch-text
                                            #".node.inner sep=12pt, below of=chapter, text width=10cm, align=center, outer sep=12pt. .title1. . .;"
                                            (Matcher/quoteReplacement
                                             (str "\\node[inner sep=12pt, below of=chapter, text width=10cm, align=center, outer sep=12pt] (title1) { };"
                                                  "
"
                                                  "
        \\node[anchor=north] at (title1.south){\\pgfornament[width=5cm]{60}};
        \\node[anchor=south] at (chapter.north){\\pgfornament[width=5cm,symmetry=h]{49}};
        \\end{tikzpicture}
        \\endgroup
      \\newpage
      \\restoregeometry
"
                                                  )))
                            ch-text)

               ch-first-char
               (last
                (re-find #"restoregeometry\s+(.)" fixed-text))
               _ (println (= ch-first-char "\\"))
               fixed-first-char (if (not= ch-first-char "\\")
                                  (string/replace fixed-text
                                                  #"restoregeometry\s+(.)"
                                                  (Matcher/quoteReplacement
                                                   (str "restoregeometry"
                                                        "\n"
                                                        "\\lettrine{"
                                                        ch-first-char
                                                        "}")))
                                  fixed-text)

               fixed-apostrophes (string/replace fixed-first-char
                                                 "’"
                                                 "'")]

           (binding [*out* wrtr]
             (println fixed-apostrophes))))))))
