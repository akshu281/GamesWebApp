
;;;Ask for User Motivation

(defrule Action
?Action <- (User 
(MBTI_Com ?MBTI_Com)
(Action nil)
)
=>
;;(printout t "You enjoy the excitement from chaotic and fast-pace games? (y/n)" crlf)
;;(bind ?response (read))
;;(modify ?Action (Action ?response))
)

(defrule Social
?Social <- (User 
(Action ?Action)
(Social nil)
)
=>
;;(printout t "You like to play with others, as a team against other teams? (y/n)" crlf)
;;(bind ?response (read))
;;(modify ?Social (Social ?response))
)


(defrule Mastery
?Mastery <- (User 
(Social ?Social)
(Mastery nil)
)
=>
(printout t "You enjoy strategic games which requires you to think ahead? (y/n)" crlf)
;;(bind ?response (read))
;;(modify ?Mastery (Mastery ?response))
)

(defrule Achievement
?Achievement <- (User 
(Mastery ?Mastery)
(Achievement nil)
)
=>
(printout t "You gain satisfaction from collecting the best items in the game? (y/n)" crlf)
;;(bind ?response (read))
;;(modify ?Achievement (Achievement ?response))
)

(defrule Immersion
?Immersion <- (User 
(Achievement ?Achievement)
(Immersion nil)
)
=>
(printout t "You enjoy games with elaborate plot and immerse yourself into the game? (y/n)" crlf)
;;(bind ?response (read))
;;(modify ?Immersion (Immersion ?response))
)


(defrule Creativity
?Creativity <- (User 
(Immersion ?Immersion)
(Creativity nil)
)
=>
(printout t "You enjoy games which allows you to demonstrate your creativity? (y/n)" crlf)
;;(bind ?response (read))
;;(modify ?Creativity (Creativity ?response))
)
------------------------------------------------------------------------------------------------------------------------------------------------------

;;;;;;;;;;;;;;;
;Shooter Games;
;;;;;;;;;;;;;;;

(defrule Action_Shooter
(Recommended_Game_Type FTS)
(User (Action y))
=>
(assert (Recommended_Game Call_of_Duty_WWII))
)

(defrule Social_Shooter
(Recommended_Game_Type FTS)
(User (Social y))
=>
(assert (Recommended_Game Overwatch))
)

(defrule Mastery_Shooter
(Recommended_Game_Type FTS)
(User (Mastery y))
=>
(assert (Recommended_Game Metal_Gear_Solid_V))
)


(defrule Achievement_Shooter
(Recommended_Game_Type FTS)
(User (Achievement y))
=>
(assert (Recommended_Game Destiny_2))
)


(defrule Immersion_Shooter
(Recommended_Game_Type FTS)
(User (Immersion y))
=>
(assert (Recommended_Game Bioshock_Infinite))
)

(defrule Creativity_Shooter
(Recommended_Game_Type FTS)
(User (Creativity y))
=>
(assert (Recommended_Game Portal_2))
)

;;;;;;;;;;;;;;;;;;;;
;Role-Playing Games;
;;;;;;;;;;;;;;;;;;;;

(defrule Action_RPG
(Recommended_Game_Type RPG)
(User (Action y))
=>
(assert (Recommended_Game Grand_Theft_Auto))
)

(defrule Social_RPG
(Recommended_Game_Type RPG)
(User (Social y))
=>
(assert (Recommended_Game Diablo_III))
)

(defrule Mastery_RPG
(Recommended_Game_Type RPG)
(User (Mastery y))
=>
(assert (Recommended_Game Dark_Soul_III))
)


(defrule Achievement_RPG
(Recommended_Game_Type RPG)
(User (Achievement y))
=>
(assert (Recommended_Game Witcher_3))
)


(defrule Immersion_RPG
(Recommended_Game_Type RPG)
(User (Immersion y))
=>
(assert (Recommended_Game Final_Fantasy_Series))
)

(defrule Creativity_RPG
(Recommended_Game_Type RPG)
(User (Creativity y))
=>
(assert (Recommended_Game Minecraft_Story_Mode))
)


(defrule Print_Recommended_Game
(declare (salience -1000))
(Recommended_Game ?Recommended_Game)
=>
(printout t "We recommend you "?Recommended_Game crlf)
)


