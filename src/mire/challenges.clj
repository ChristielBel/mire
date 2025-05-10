(ns mire.challenges
    (:require [mire.player :as player]))

(def challenges (ref {}))

; Maps challenge IDs to challenge data refs

(defn create-challenge [creator challenge-type challenge-name]
  (dosync
   (if (and (contains? @challenges challenge-name) (not= (:status @(@challenges challenge-name)) :completed))
     false
     ; Return false if a challenge with the same name already exists
     (let [new-challenge (ref
                           {:name         challenge-name
                            :type         challenge-type
                            :creator      creator
                            :participants #{creator}
                            :status       :waiting
                            :scores       {}})]
       (alter challenges assoc challenge-name new-challenge)
       ;; Notify all players
       (doseq [[player-name player-stream] @player/streams]
         (when (and player-stream (not= player-name creator))
               (binding [*out* player-stream]
                        (println
                          (str "A new challenge '" challenge-name "' has been created by " creator "!"))
                        (println player/prompt)
                        (flush))))
       true))))

(defn join-challenge [challenge-name player]
  (dosync
   (if-let [challenge-ref (get @challenges challenge-name)]
     (let [challenge           @challenge-ref
           in-other-challenge? (some
                                 (fn [[_ ch-ref]]
                                    (and (not= (:status @ch-ref))
                                   (contains? (:participants @ch-ref) player)))
                                 @challenges)]
       (if (and (= (:status challenge) :waiting)
                (not in-other-challenge?))
         (do
           (alter challenge-ref update :participants conj player)
           (when-let [creator-stream (get @player/streams (:creator challenge))]
             (binding [*out* creator-stream]
                      (println (str player " has joined your challenge: " challenge-name))
                      (println player/prompt)
                      (flush)))
           true)
         false))
     false)))

(defn start-challenge [challenge-name creator]
  (dosync
   (if-let [challenge-ref (get @challenges challenge-name)]
     (let [challenge @challenge-ref]
       (if (and (= (:status challenge) :waiting)
                (= (:creator challenge) creator))
         (do
           (alter challenge-ref assoc :status :in-progress)
           true)
         false))
     false)))

(defn find-active-challenge [player]
  (some
    (fn [[_ challenge-ref]]
      (let [challenge @challenge-ref]
        (when
          (and (= (:status challenge) :in-progress)
               (contains? (:participants challenge) player))
          challenge-ref)))
    @challenges))

(defn add-score [challenge-ref player points]
  (dosync
   (alter challenge-ref update-in [:scores player] (fnil + 0) points)))