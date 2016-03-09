(require '[figwheel-sidecar.repl :as r]
         '[figwheel-sidecar.repl-api :as ra])

(ra/start-figwheel!
  {:figwheel-options {:css-dirs ["resources/public"]
                      :server-port 5001}
   :build-ids ["dev"]
   :all-builds
                     [{:id "dev"
                       :figwheel true
                       :source-paths ["src"]
                       :compiler {:main 'masonry-layout.core
                                  :asset-path "js/out"
                                  :output-to "resources/public/js/out/main.js"
                                  :output-dir "resources/public/js/out"
                                  :verbose true}}]})

(ra/cljs-repl)
