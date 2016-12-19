(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def the-map
  {:bedroom {:desc "It's a Sunday afternoon, you've been assigned lots of homework but you're in a mood for something more interesting. However, you're locked in the house because Mom wants you to study.You could go downstairs to the living room to see if there is anything entertaining there."
           :title "in your room"
           :dir {:down :living-room
		:west :master-bedroom
		:east :study
		:north :bathroom
		:up :terrace}
           :contents #{"back-pack" }}
   :living-room {:desc "The living room is quite familiar to you. It's 3'O clock and there is nothing you'd watch on the TV at this point. Your issue of the National Geographic is lying on the side table along with a plate of half eaten cookies."
              :title "in the living room"
              :dir {:up :bedroom
		:down :basement
		:north :library
		:west :gym
		:east :kitchen}
              :contents #{"magazine"}} ;;make a read command 
   :kitchen {:desc "The kitchen has dark brown cabinets with two double doors. These cabinets are filled with dishes, cups and bowls on one side and spices along with two boxes of assorted individual packs of oatmeal on the other. To the right, below the cabinet doors is the sink, there are dirty dishes in the sink just begging to be washed. You're in a mood for some cleaning and see these as an opportunity to impress mom but first you would want to grab that last cookie kept high up in the cabinet. You might want to jump to grab the cookie."
		:title "in the kitchen"
		:dir {:north :dining-room
		:west :living-room
		:down :secret-room}
		:contents #{"knife" "Apple" "Durian"}}
   :library {:desc "You have a small library with rows upon rows of book shelves. The books are arranged by genre but they aren't of much interest to you. There isn't much to do here other than just relax and unwind as you soak in the view from the window."
		:title "in the library"
		:dir {:south :living-room}
		:contents #{"book" "lamp" "magnifying-glass"}}
   :bathroom {:desc "There is absolutely nothing interesting about your bathroom. You could pick up your tooth brush in case you're going for some long adventure."
		:title "in the bathroom"
		:dir{:south :bedroom}
		:contents #{"tooth-brush"}}
   :dining-room {:desc "The dining room has a fancy glass table in the centre. The room seems like no one has eaten here for a while. You might want sit here and grab a bite."
		:title "in the dining room"
		:dir {:south :kitchen}
		:contents #{"spoon" "candle" "table-cloth"}}
   :pool {:desc "You've followed the hint and come as far as possible in one direction. You put your head down in disappointment only to see an object reflecting sunlight from the bottom of the pool. Wanna dive in?"
		:title "at the swimming pool"
		:dir {:east :gym}
		:contents #{}}
   :terrace {:desc "The terrace overlooks a beautiful valley one side of which is a melange of flowers and the other covered in tea plantation. There is a narrow brook separating the two portions. You sit here for a while pondering about the weird sound in the kitchen. You notice a pattern in the sky which may be a hint as to where the key might be. It appears to be pointing far in one direction."
		:title "on the terrace"
		:dir {:down :bedroom}
		:contents #{"coffee-mug"}}
   :basement {:desc "The basement is just a huge mess. I doubt you could get anywhere from here. However, look around if you find something interesting."
		:title "in the basement"
		:dir{:up :living-room
		:east :garage}
		:contents #{"invisibility-cloak"}}
   :master-bedroom {:desc "Mom is sleeping here. You really don't want to wake her up. She might have the key to that locked room though. Would you risk checking?"
		:title "in the master bedroom"
		:dir {:east :bedroom 
		:down  :living-room}
		:contents #{"lip-stick" "bracelet" "cupboard-key"}}
   :gym {:desc "The gym has a tread-mill and a couple of yoga-mats. There isn't much to do in this room. You might want to think about the hint you got on the terrace."
		:title "in the gym"
		:dir {:west :pool
		:east :living-room
		:north :rec-room}
		:contents #{"yoga-mat"}}
   :secret-room {:desc "Well, the curious little cat has finally opened this door and nothing could possibly make you happier than seeing what is in front of you. There is a dog baking magical unlimited double chocolate chip cookies. Could this afternoon be any better! You pack as many cookies as possible in your back-pack and head out only to come back later. "
		:title "in the secret room"
		:dir{:up :kitchen}
		:contents #{"UNLIMITED-COOKIES"}}
   :study {:desc "The study has french windows to its east. Your staudy table also faces the same direction. You sit to study but you're in no mood to complete your homework."
		:title "in the study"
		:dir {:west :bedroom}
		:contents #{"laptop"}}
   :garage {:desc "The garage is dinghy. There is one tiny window which allows fragments of light to pass through. You have a feeling you might find the key here. If it isn't here you might want to think about the hint you got on the terrace."
		:title "in the garage"
		:dir {:west :basement}
		:contents #{"car-key" "tyre"}}
   :rec-room {:desc "There is a foosball table here but you need someone to play with you. Your brother is studying upstairs. He is quite a geek and you'd need to bribe him to play."
		:title "in the rec room"
		:dir {:south :gym}
		:contents #{"basket-ball"}}
	

   })

(def adventurer
  {:location :bedroom
   :inventory #{}
   :tick 0
   :seen #{}})

(defn status [player]
  (let [location (player :location)]
    (print (str "You are " (-> the-map location :title) ". "))
    (when-not ((player :seen) location)
      (print (-> the-map location :desc)))
    (update-in player [:seen] #(conj % location))))

(defn print_inventory [player]
	(do (println (seq (player :inventory)))
		player))

(defn print_content [player]
	(let [location (player :location)]
	(do (println (str (-> the-map location :contents))) player)))

(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

(defn jump [player]
	(let [location (player :location)]
	(if (not (= location :kitchen )) (do (println "Jumping is a good exercise but there are no cookies here") player)
	(do (println "So what if you're a little short, you've still got the double chocolate chip cookies")
	(update-in player [:inventory] #(conj % "cookie"))))))
(defn wash [player]
	(let [location (player :location)]
		(if (not (= location :kitchen )) (do (println "There is nothing to wash here")player)
			(do (println "While washing the dishes you drop a spoon on the floor. You notice something unusual about the sound of the spoon falling on the ground. You knock at the floor to notice that it is rather hollow. There seems to be something there. ")player))))
(defn swim [player]
	(let [location (player :location)]
	(if (not (= location :pool)) (do (println "You'll most definitely not drown here but you might want to swim in the pool") player)
	(do (println "You've acquired the shiny key at the bottom of the pool. You wonder what this key leads to! You might want to check the other rooms or you could go back to the rooms that seemed a little fishy to you.")
	(update-in player [:inventory] #(conj % "key"))))))
(defn reader [player]
	(let [location (player :location)]
		(if (not (= location :living-room)) (do (println "There is nothing to read here. Your ssue of the National Geographic is in the living-room or you could read the homework assigned to you.")player) 
			(do (println "You flip through a couple of pages and come accross an adorable picture of a dog baking a cookie. That's rather absurd! Why would the National Geographic share this and do Dogs bake?! You've seen a little much of cookies today so you get a strong craving for cookies. You might want to head to the kitchen")player)))) 
(defn eat [player]
	(let [inventory (player [:inventory])]
		(if (inventory "cookie") (do (println "These cookies are delicious. Don't you wish you could have more!")(update-in player [:inventory] #(disj % "cookie")))
			(do (println "There is nothing to eat. Wish there were cookies!")player))) )
(defn go [dir player]
  (let [location (player :location)
        dest (->> the-map location :dir dir)
        inventory (player :inventory)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          player)
      (if (= dest :secret-room)
      	(if (not( inventory "key"))

      		(do (println "This room is locked.")player)
      		(assoc-in player [:location] dest))
      (assoc-in player [:location] dest)))))

(defn pick [obj player]
	(let [location (player :location)
		  inventory (player :inventory)
		  content ( get-in the-map [location :contents] )]
	(if (content (name obj))
		(if (contains? inventory obj) (do (println "You already have this item") player)
		(do (println (str "You have picked up a " obj)) (update-in player [:inventory] #(conj % obj))))
		(do (println "There is no such item in this room.")player))))

(defn quit []
	(System/exit 0))

(defn tock [player]
  (update-in player [:tick] inc))

(defn respond [player command]	 
 (if(contains? command 1)
 (pick (command 1) player)


 (match command
     [:look] (update-in player [:seen] #(disj % (-> player :location)))
     (:or [:n] [:north] ) (go :north player)
     [:south] (go :south player)
	 [:west] (go :west player)
	 [:east] (go :east player)
	 [:south] (go :south player)
	 [:down] (go :down player)
	 [:up] (go :up player)
	 [:inventory] (print_inventory player)
	 [:content] (print_content player)
	 [:status] (status player)
	 [:jump] (jump player)
	 [:swim] (swim player)
	 [:read] (reader player)
	 [:wash] (wash player)
	 [:eat]  (eat player)
	 [:quit] (quit)
	 
	 ;[:open] (open player)
         _ (do (println "I don't understand you.")
               player)

         ))) 

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (loop [local-map the-map
         local-player adventurer]
    (let [pl (status local-player)
          _  (println "What do you want to do?")
          command (read-line)]
      (recur local-map (respond pl (to-keywords command))))))
