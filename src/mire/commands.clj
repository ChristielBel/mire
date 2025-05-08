(ns mire.commands
    (:require [clojure.string :as str]
              [mire.rooms :as rooms]
              [mire.player :as player]
              [mire.challenges :as challenges]))

(defn- move-between-refs
  "Move one instance of obj between from and to. Must call in a transaction."
  [obj from to]
  (alter from disj obj)
  (alter to conj obj))

;; Command functions

(defn look
  "Get a description of the surrounding environs and its contents."
  []
  (str (:desc @player/*current-room*)
       "\nExits: " (keys @(:exits @player/*current-room*)) "\n"
       "\nPlayers in the room: "
       (str/join ", " (seq @(:inhabitants @player/*current-room*)))))

(defn move
  "\"♬ We gotta get out of this place... ♪\" Give a direction."
  [direction]
  (dosync
   (let [target-name ((:exits @player/*current-room*) (keyword direction))
         target      (@rooms/rooms target-name)]
     (if target
       (do
         (move-between-refs player/*name*
                            (:inhabitants @player/*current-room*)
                            (:inhabitants target))
         (ref-set player/*current-room* target)
         (look))
       "You can't go that way."))))

(defn grab
  "Pick something up."
  [thing]
  (dosync
   (if (rooms/room-contains? @player/*current-room* thing)
     (do
       (move-between-refs (keyword thing)
                          (:items @player/*current-room*)
                          player/*inventory*)
       (str "You picked up the " thing "."))
     (str "There isn't any " thing " here."))))

(defn discard
  "Put something down that you're carrying."
  [thing]
  (dosync
   (if (player/carrying? thing)
     (do
       (move-between-refs (keyword thing)
                          player/*inventory*
                          (:items @player/*current-room*))
       (str "You dropped the " thing "."))
     (str "You're not carrying a " thing "."))))

(defn inventory
  "See what you've got."
  []
  (str "You are carrying:\n"
       (str/join "\n" (seq @player/*inventory*))))

(defn detect
  "If you have the detector, you can see which room an item is in."
  [item]
  (if (@player/*inventory* :detector)
    (if-let [room (first
                    (filter #((:items %) (keyword item))
                            (vals @rooms/rooms)))]
      (str item " is in " (:name room))
      (str item " is not in any room."))
    "You need to be carrying the detector for that."))

(defn say
  "Say something out loud so everyone in the room can hear."
  [& words]
  (let [message (str/join " " words)]
    (doseq [inhabitant (disj @(:inhabitants @player/*current-room*)
                             player/*name*)]
      (binding [*out* (player/streams inhabitant)]
               (println (str player/*name* ", " message))
               (println player/prompt)))
    (str "You said " message)))

(defn create-challenge
  "Create a new challenge. Usage: create-challenge <type> <name>"
  [challenge-type challenge-name]
  (if (challenges/create-challenge player/*name* challenge-type challenge-name)
    (str "Challenge " challenge-name " created! Others can join with 'join-challenge " challenge-name "'.")
    "Challenge with this name already exists."))

(defn join-challenge
  "Join a challenge. Usage: join-challenge <name>"
  [challenge-name]
  (if (challenges/join-challenge challenge-name player/*name*)
    (str "Joined challenge " challenge-name ".")
    "Cannot join challenge. It may have started or doesn't exist, or you're already joined it."))

(defn start-challenge
  "Start a challenge. Usage: start-challenge <name>"
  [challenge-name]
  (if (challenges/start-challenge challenge-name player/*name*)
    (dosync ; <-- Add dosync to handle ref transactions
      (let [challenge-ref (get @challenges/challenges challenge-name)
            participants (:participants @challenge-ref)]
        ;; Notify participants
        (doseq [p participants]
          (when-let [out (get @player/streams p)]
            (binding [*out* out]
              (println "Challenge started! Type 'forge' to earn points in the next 30 seconds!")
              (println player/prompt)
              (flush))))

        ;; Start timer and process
        (future
          (Thread/sleep 30000)
          (let [challenge @challenge-ref
                scores (:scores challenge)
                winner (if (empty? scores)
                         "No one"
                         (key (apply max-key val scores)))]
            (doseq [p participants]
              (when-let [out (get @player/streams p)]
                (binding [*out* out]
                  (println (str "Winner: " winner " with " (get scores winner 0) " points!"))
                  (println player/prompt)
                  (flush))))
            (dosync (alter challenge-ref assoc :status :completed)))))
      "Challenge started!")
    "Failed to start challenge."))

(defn forge
  "Gather points during a challenge. Usage: forge"
  []
  (if-let [challenge-ref (challenges/find-active-challenge player/*name*)]
    (let [points (+ (rand-int player/*power*)
                    (rand-int player/*agility*)
                    (rand-int player/*luck*))]
      (challenges/add-score challenge-ref player/*name* points)
      (str "Forged +" points " points!"))
    "You're not in an active challenge."))

(defn help
  "Show available commands and what they do."
  []
  (str/join "\n"
            (map #(str (key %) ": " (:doc (meta (val %))))
                 (dissoc (ns-publics 'mire.commands)
                         'execute 'commands))))

(defn stats
  "Display player's name, inventory, and characteristics."
  []
  (str "Name: " player/*name* "\n"
       "Free points: " @player/*point_count* "\n"
       "Power: " @player/*power* "\n"
       "Agility: " @player/*agility* "\n"
       "Luck: " @player/*luck* "\n"
       "Inventory: " (str/join ", " (seq @player/*inventory*))))

(defn distribute
  "Distribute free points to a characteristic (power, agility, luck)."
  [characteristic]
  (let [characteristic (keyword characteristic)]
    (if (contains? #{:power :agility :luck} characteristic)
      (do
        (println (str "You have " @player/*point_count* " free points."))
        (print (str "How many points do you want to assign to " characteristic "? "))
        (flush)
        (let [input  (read-line)
              points (try (Integer/parseInt input)
                       (catch Exception _ nil))]
          (cond
           (nil? points)                    (println "Invalid input. Please enter a number.")
           (< points 1)                     (println "Points must be at least 1.")
           (> points @player/*point_count*) (println "You don't have enough points.")
           :else                            (dosync
                                             (case characteristic
                                                   :power   (alter player/*power* + points)
                                                   :agility (alter player/*agility* + points)
                                                   :luck    (alter player/*luck* + points))
                                             (alter player/*point_count* - points)
                                             (println (str "Assigned " points " points to " characteristic "."))))))
      (println "Invalid characteristic. Use 'power', 'agility', or 'luck'."))))

;; Command data

(def commands
  {"move"             move,
   "north"            (fn [] (move :north)),
   "south"            (fn [] (move :south)),
   "east"             (fn [] (move :east)),
   "west"             (fn [] (move :west)),
   "grab"             grab
   "discard"          discard
   "inventory"        inventory
   "detect"           detect
   "look"             look
   "say"              say
   "help"             help
   "stats"            stats
   "distribute"       distribute
   "create-challenge" create-challenge
   "join-challenge"   join-challenge
   "start-challenge"  start-challenge
   "forge"            forge})

;; Command handling

(defn execute
  "Execute a command that is passed to us."
  [input]
  (try
    (let [[command & args] (.split input " +")]
      (apply (commands command) args))
    (catch Exception e
      (.printStackTrace e (new java.io.PrintWriter *err*))
      "You can't do that!")))
