(ns com.phronemophobic.reveal-plugin.reveal-exception
  (:require [membrane.ui :as ui]
            [clj-stacktrace.core :as clj-stacktrace]
            [membrane.basic-components :as basic]
            [membrane.component :refer
             [defui defeffect]]
            [clojure.string :as str]
            [vlaaad.reveal.ext :as rx]
            [clojure.repl :refer [source demunge]
             :as repl]
            [membrane.cljfx :as mfx
             :refer [membrane-component]]
            [cljfx.api :as fx]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]])
  (:import javafx.geometry.BoundingBox
           (java.io LineNumberReader InputStreamReader PushbackReader)
           (java.net URL)
           clojure.lang.RT)
  (:gen-class))


(defn foo []
  (/ 1 0))

;; Adapted from:
;; https://github.com/clojure-emacs/orchard/blob/9f0c7d53eafd656b46638ed322992faa70862070/src/orchard/namespace.clj#L29
(defn canonical-source
  "Returns the URL of the source file for the namespace object or symbol,
  according to the canonical naming convention, if present on the classpath."
  ^URL [ns]
  (let [prefix (-> (str ns)
                 (str/replace "-" "_")
                 (str/replace "." "/"))]
    (some
     (fn [suffix]
       (let [path (str prefix suffix)]
         (when (io/resource path)
           path)))
     [".clj"
      ".cljc"
      ".cljs"])))


(defn parse-stacktrace-element
  "Returns a (possibly unmunged) string representation of a StackTraceElement"
  {:added "1.3"}
  [^StackTraceElement el]
  (let [file (.getFileName el)
        clojure-fn? (and file (or (.endsWith file ".clj")
                                  (.endsWith file ".cljc")
                                  (= file "NO_SOURCE_FILE")))]
    (merge
     (when clojure-fn?
       {:sym (symbol (demunge (.getClassName el)))})
     {:class (.getClassName el)
      :stacktrace-element el
      :method (.getMethodName el)
      :filename (.getFileName el)
      :lineno (.getLineNumber el)})))

(defn source-path-via-meta* [elem]
  (when-let [sym (:sym elem)]
    (when-let [v (resolve sym)]
      (when-let [m (meta v)]
        {:file (:file m)
         :line (:line m)}))))

(defn source-path-via-ns* [elem]
  (when-let [sym (:sym elem)]
    (when-let [ns-name (namespace sym)]
      (when-let [ns-sym (symbol ns-name)]
        (when (the-ns ns-sym)
          (when-let [line (:lineno elem)]
            {:file (canonical-source ns-sym)
             :line line})))))
  )

(defn find-source-path [elem]
  (or (source-path-via-meta* elem)
      (source-path-via-ns* elem)))

(defn source-fn
  "Returns a string of the source code for the given symbol, if it can
  find it.  This requires that the symbol resolve to a Var defined in
  a namespace for which the .clj is in the classpath.  Returns nil if
  it can't find the source.  For most REPL usage, 'source' is more
  convenient.

  Example: (source-fn {:sym 'filter})"
  [elem]
  (when-let [{filepath :file
              line :line} (find-source-path elem)]
    (when-let [strm (or (.getResourceAsStream (RT/baseLoader) filepath)
                        (io/input-stream filepath))]
      (with-open [rdr (LineNumberReader. (InputStreamReader. strm))]
        (dotimes [_ (dec line)] (.readLine rdr))
        (let [text (StringBuilder.)
              pbr (proxy [PushbackReader] [rdr]
                    (read [] (let [i (proxy-super read)]
                               (.append text (char i))
                               i)))
              read-opts (if (.endsWith ^String filepath "cljc") {:read-cond :allow} {})]
          (if (= :unknown *read-eval*)
            (throw (IllegalStateException. "Unable to read source while *read-eval* is :unknown."))
            (try
              (read read-opts (PushbackReader. pbr))
              (catch Exception e
                nil)))
          (str text))))))

(defeffect ::open-in-editor [elem]
;; (cider--find-var "membrane.ui/vertical-layout" 120)
  (when-let [sym (:sym elem)]
    (if-let [v (resolve sym)]
      (sh "emacsclient"  "--no-wait" "-e" (str "(cider--find-var " "\"" (pr-str sym) "\"" ")"))
      (when-let [ns-name (namespace sym)]
        (when-let [ns-sym (symbol ns-name)]
          (when (the-ns ns-sym)
            (when-let [line (:lineno elem)]
              (sh "emacsclient"  "--no-wait" "-e" (str "(cider--find-var " "\"" (pr-str ns-sym) "\"" line")")))))))))



(def source-fn-memo (memoize source-fn))

(defui exception-ui [{:keys [exception hover-source selected-source selected-elem]}]
  (let [st (:stacktrace exception)]
    (ui/padding
     5 5
     (ui/horizontal-layout
      (apply
       ui/vertical-layout
       (for [[i {:keys [sym class method filename lineno stacktrace-element]
                 :as elem}] (map-indexed vector st)
             :when sym]
         (let [hover? (get extra [:hover? i])]
           [(basic/on-mouse-out
             {:hover? hover?
              :mouse-out
              (fn []
                [[:set $hover-source nil]])
              :body
              (ui/on
               :mouse-move
               (fn [pos]
                 (when-let [source (source-fn-memo elem)]
                   [[:set $hover-source source]]))
               :mouse-down
               (fn [pos]
                 (when-let [source (source-fn-memo elem)]
                   [[:set $selected-source source]
                    [:set $selected-elem elem]]))
               (ui/label sym))})])))
      (when-let [source (or hover-source selected-source)]
        (ui/vertical-layout
         (when (and selected-elem
                    (find-source-path selected-elem))
           (basic/button {:text "open"
                          :on-click
                          (fn []
                            [[::open-in-editor selected-elem]])}))
         (ui/label source)))))))


(rx/defaction ::exception [obj]
  (when (instance? Throwable obj)
   (fn []
     (let [app-state (atom {:exception {:stacktrace
                                        (map #(parse-stacktrace-element %)
                                             (.getStackTrace obj))}})]
       {:fx/type rx/observable-view
        :ref app-state
        :fn (fn [state]
              (let [view (membrane-component #'exception-ui
                                             state
                                             #(swap! app-state %))]
                {:fx/type rx/popup-view
                 :select (fn [e]

                           (condp instance? e
                             #_#_javafx.scene.input.KeyEvent
                             (when-let [rect (-> state :select-rect :rect)]
                               (let [node (.getTarget e)
                                     local-bounds (BoundingBox. (:x rect)
                                                                (:y rect)
                                                                (:w rect)
                                                                (:h rect))
                                     bounds (.localToScreen node local-bounds)]
                                 (when rect
                                   {:bounds bounds
                                    :value (:obj rect)})))

                             javafx.scene.input.ContextMenuEvent
                             (let [node (.getTarget e)
                                   x (.getX e)
                                   y (.getY e)
                                   intents (->> (ui/mouse-down (exception-ui @app-state) [x y]))
                                   local-bounds (BoundingBox. x y 10 10)
                                   bounds (.localToScreen node local-bounds)]
                               (when-let [selected-elem (->> intents
                                                             (filter (fn [[intent & args]]
                                                                       (= intent :set)))
                                                             (some (fn [[_set $ref v]]
                                                                     (when (= $ref '[(keypath :selected-elem)])
                                                                       v))))]
                                 {:value selected-elem
                                  :bounds bounds}))))
                 :desc {:fx/type :scroll-pane
                        :fit-to-width true
                        :content view}}))}))))




