(ns zone.lambda.game.main
  (:require
   [zone.lambda.game.chess.core :as chess]
   [zone.lambda.game.connect4.core :as connect4]
   [clojure.core.reducers :as r]))


(defn helper []
  (println "use one of the following parameters:
\tchess
\tconnect4"
                 ))

(defn -main
  ([]
     (helper))
  ([app]
     (case app
       "chess" (chess/-main)
       "connect4" (connect4/-main)
       (helper))))
