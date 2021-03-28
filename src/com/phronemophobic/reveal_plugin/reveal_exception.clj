(ns com.phronemophobic.reveal-plugin.reveal-exception
  (:require [membrane.ui :as ui]
            [clj-stacktrace.core :as clj-stacktrace]
            [membrane.basic-components :as basic]
            [membrane.component :refer
             [defui defeffect]]
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
           clojure.lang.RT)
  (:gen-class))


;; (clj-stacktrace/parse-exception)

(defn foo []
  (/ 1 0))

;; (foo)

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

(defn source-fn
  "Returns a string of the source code for the given symbol, if it can
  find it.  This requires that the symbol resolve to a Var defined in
  a namespace for which the .clj is in the classpath.  Returns nil if
  it can't find the source.  For most REPL usage, 'source' is more
  convenient.

  Example: (source-fn 'filter)"
  [x]
  (when-let [v (resolve x)]
    (when-let [filepath (:file (meta v))]
      (when-let [strm (or (.getResourceAsStream (RT/baseLoader) filepath)
                          (io/input-stream filepath))]
        (with-open [rdr (LineNumberReader. (InputStreamReader. strm))]
          (dotimes [_ (dec (:line (meta v)))] (.readLine rdr))
          (let [text (StringBuilder.)
                pbr (proxy [PushbackReader] [rdr]
                      (read [] (let [i (proxy-super read)]
                                 (.append text (char i))
                                 i)))
                read-opts (if (.endsWith ^String filepath "cljc") {:read-cond :allow} {})]
            (if (= :unknown *read-eval*)
              (throw (IllegalStateException. "Unable to read source while *read-eval* is :unknown."))
              (read read-opts (PushbackReader. pbr)))
            (str text)))))))

(defeffect ::open-in-editor [elem]
  (when-let [sym (:sym elem)]
    (when-let [v (resolve sym)]
      (when-let [filepath (:file (meta v))]
        (let [f (io/file filepath)]
          (when (.exists f)
            (when-let [line (:line (meta v))]
              (sh "emacsclient"  "--no-wait" (str "+" line) (.getCanonicalPath f)))))))))


(defui exception-ui [{:keys [exception hover-source selected-source selected-elem]}]
  (let [st (:stacktrace exception)]
    (ui/horizontal-layout
     (apply
      ui/vertical-layout
      (for [{:keys [sym class method filename lineno stacktrace-element]
             :as elem} st
            :when sym]
        (let [hover? (get extra [:hover? stacktrace-element])]
          [(basic/on-mouse-out
            {:hover? hover?
             :mouse-out
             (fn []
               [[:set $hover-source nil]])
             :body
             (ui/on
              :mouse-move
              (fn [pos]
                [[:set $hover-source (source-fn sym)]])
              :mouse-down
              (fn [pos]
                [[:set $selected-source (source-fn sym)]
                 [:set $selected-elem elem]])
              (ui/label sym))})])))
     (when-let [source (or hover-source selected-source)]
       (ui/vertical-layout
        (when selected-elem
          (basic/button {:text "open"
                         :on-click
                         (fn []
                           [[::open-in-editor selected-elem]])}))
        (ui/label source))))))


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
                 #_#_:select (fn [e]
                           (condp instance? e
                             javafx.scene.input.KeyEvent
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
                                   rect (->> (ui/mouse-down (apply treemap-clj.view/treemap-explore
                                                                   (sequence cat state))
                                                            [x y])
                                             (some (fn [[type & args :as intent]]
                                                     (when (= type :treemap-clj.view/select-rect)
                                                       (nth intent 2)))))
                                   local-bounds (BoundingBox. x y 10 10)
                                   bounds (.localToScreen node local-bounds)]
                               (when rect
                                 {:bounds bounds
                                  :value (:obj rect)}))))
                 :desc {:fx/type :scroll-pane
                        :fit-to-width true
                        :content view}}))}))))





(defn bar []
  (foo))



(bar)
