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

(defn- distribute-points [remaining-points characteristic-name]
  (println (str "\nYou have " remaining-points " points left."))
  (print
    (str "How many points do you want to assign to " characteristic-name "? "))
  (flush)
  (let [input  (read-line)
        points (try (Integer/parseInt input)
                 (catch Exception _ nil))]
    (cond
     (nil? points)               (do (println "Invalid input. Please enter a number.")
                                   (recur remaining-points characteristic-name))
     (< points 1)                (do (println "Points must be at least 1.")
                                   (recur remaining-points characteristic-name))
     (> points remaining-points) (do (println "You don't have enough points.")
                                   (recur remaining-points characteristic-name))
     :else                       points)))

(defn- mire-handle-client [in out]
  (binding
    [*in*
     (io/reader in)
     *out*
     (io/writer out)
     *err*
     (io/writer System/err)]

    (print "\nWhat is your name? ") (flush)
    (let [name            (get-unique-player-name (read-line))
          current-room    (ref (@rooms/rooms :start))
          inventory       (ref #{})
          initial-points  10
          default-power   1
          default-agility 1
          default-luck    1]

      (let [power-points     (distribute-points initial-points "power")
            remaining-points (- initial-points power-points)
            agility-points   (distribute-points remaining-points "agility")
            remaining-points (- remaining-points agility-points)
            luck-points      (distribute-points remaining-points "luck")
            remaining-points (- remaining-points luck-points)]

        (binding
          [player/*name*
           name
           player/*current-room*
           current-room
           player/*inventory*
           inventory
           player/*point_count*
           remaining-points
           player/*power*
           (+ default-power power-points)
           player/*agility*
           (+ default-agility agility-points)
           player/*luck*
           (+ default-luck luck-points)]

          (println "\nPlayer Characteristics:")
          (println "Name:" player/*name*)
          (println "Remaining Points:" player/*point_count*)
          (println "Power:" player/*power*)
          (println "Agility:" player/*agility*)
          (println "Luck:" player/*luck*)

          (dosync
           (commute (:inhabitants @player/*current-room*) conj player/*name*)
           (commute player/streams assoc player/*name* *out*))

          (println (commands/look)) (print player/prompt) (flush)

          (try
            (loop [input (read-line)]
              (when input
                    (println (commands/execute input))
                    (.flush *err*)
                    (print player/prompt) (flush)
                    (recur (read-line))))
            (finally (cleanup))))))))

(defn -main
  ([port dir]
   (rooms/add-rooms dir)
   (defonce server (socket/create-server (Integer. port) mire-handle-client))
   (println "Launching Mire server on port" port))
  ([port] (-main port "resources/rooms"))
  ([] (-main 3333)))