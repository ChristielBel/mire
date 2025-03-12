(ns mire.server
  (:require [clojure.java.io :as io]
            [server.socket :as socket]
            [mire.player :as player]
            [mire.commands :as commands]
            [mire.rooms :as rooms]))

(defn- cleanup []
  "Drop all inventory and remove player from room and player list."
  (dosync
   (doseq [item @player/*inventory*]
     (commands/discard item))
   (commute player/streams dissoc player/*name*)
   (commute (:inhabitants @player/*current-room*)
            disj player/*name*)))

(defn- get-unique-player-name [name]
  (if (@player/streams name)
    (do (print "That name is in use; try again: ")
        (flush)
        (recur (read-line)))
    name))

(defn- mire-handle-client [in out]
  (binding [*in* (io/reader in)
            *out* (io/writer out)
            *err* (io/writer System/err)]

    (print "\nWhat is your name? ") (flush)
    (let [name (get-unique-player-name (read-line))
          current-room (ref (@rooms/rooms :start))
          inventory (ref #{})]
      
      (print "\nCount points? ") (flush)
      (let [point-count (read-line)]
        
        (print "\nWhat is your power? ") (flush)
        (let [power (read-line)]
          
          (print "\nWhat is your agility? ") (flush)
          (let [agility (read-line)]
            
            (print "\nWhat is your luck? ") (flush)
            (let [luck (read-line)]
              
              (binding [player/*name* name
                        player/*current-room* current-room
                        player/*inventory* inventory
                        player/*point_count* (Integer. point-count)
                        player/*power* (Integer. power)
                        player/*agility* (Integer. agility)
                        player/*luck* (Integer. luck)]

              (println "Player Characteristics:")
              (println "Name:" player/*name*)
              (println "Point Count:" player/*point_count*)
              (println "Power:" player/*power*)
              (println "Agility:" player/*agility*)
              (println "Luck:" player/*luck*)

              (dosync
              (commute (:inhabitants @player/*current-room*) conj player/*name*)
              (commute player/streams assoc player/*name* *out*))

              (println (commands/look)) (print player/prompt) (flush)

              (try (loop [input (read-line)]
                    (when input
                      (println (commands/execute input))
                      (.flush *err*)
                      (print player/prompt) (flush)
                      (recur (read-line))))
                  (finally (cleanup)))))))))))

(defn -main
  ([port dir]
     (rooms/add-rooms dir)
     (defonce server (socket/create-server (Integer. port) mire-handle-client))
     (println "Launching Mire server on port" port))
  ([port] (-main port "resources/rooms"))
  ([] (-main 3333)))
