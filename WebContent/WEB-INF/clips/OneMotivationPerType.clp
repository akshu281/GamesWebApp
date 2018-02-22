;How to use allowed symbols to restrict values for MBTI?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Templates for certainty factor to work
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftemplate Certainty_Factor 
	(slot game_type) 
	(slot cf))	
	
(deftemplate Recommended_Game  
	(slot Game))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CF combination for multiple conclusions RULES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;combine POSITIVE certainty factors for multiple conclusions
;cf(cf1,cf2) = cf1 + cf2 * (1- cf1)

(defrule combine-positive-cf
  ?f1 <- (Certainty_Factor (game_type ?g)(cf ?cf1&:(>= ?cf1 0)))
  ?f2 <- (Certainty_Factor (game_type ?g)(cf ?cf2&:(>= ?cf2 0)))
  (test (neq ?f1 ?f2)) ; test pointers and not value
  =>
  (retract ?f2)
  (modify ?f1 (cf =(+ ?cf1 (* ?cf2 (- 1 ?cf1)))))
)

;combine NEGATIVE cf
;cf(cf1,cf2) = cf1 + cf2 * (1 + cf1)

(defrule combine-neg-cf
 (declare (salience -1))
  ?f1 <- (Certainty_Factor   (game_type ?g)(cf ?cf1&:(< ?cf1 0)))
  ?f2 <- (Certainty_Factor (game_type ?g)(cf ?cf2&:(< ?cf2 0)))
  (test (neq ?f1 ?f2))
  =>
  (retract ?f2)
  (modify ?f1 (cf =(+ ?cf1 (* ?cf2 (+ 1 ?cf1)))))
)

;combine one POSITIVE and one NEGATIVE
;cf(cf1,cf2) = (cf1 + cf2) / 1 - MIN[abs(cf1),abs(cf2)]

(defrule neg-pos-cf
 (declare (salience -1))
  ?f1 <- (Certainty_Factor (game_type ?g) (cf ?cf1))
  ?f2 <- (Certainty_Factor (game_type ?g) (cf ?cf2))
  (test (neq ?f1 ?f2))
  (test (< (* ?cf1 ?cf2) 0))
  =>
  (retract ?f2)
  (modify ?f1 (cf =(/ (+ ?cf1 ?cf2) (- 1 (min (abs ?cf1) (abs ?cf2))))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Allow facts to be duplicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule start 
(declare (salience 1000)) 
=> (set-fact-duplication TRUE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is written by Darren Teo, 2018, ISS NUS (Copyright) 
;; This is a rule-based system for recommending games to
;; the user. It is an interactive system that makes use of certainty factors. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftemplate User 
	(slot gender)
	(slot MBTI1	)
	(slot MBTI2	)
	(slot MBTI3	)
	(slot MBTI4	)
	(slot MBTI_Com)
	(slot FTS_Motivation)
	(slot RPG_Motivation)
)

;**** Rule 0: Assert fact Gender.
(defrule Gender
=>	
(printout t crlf "Are you male or female (m/f)")
(bind ?response (read))
(assert (User (gender ?response)))
	(switch ?response
	(case f then 	
		(assert (Certainty_Factor (game_type RPG)		(cf 0.39)))
		(assert (Certainty_Factor (game_type FTS)		(cf 0.32)))
		(assert (Certainty_Factor (game_type RTS)		(cf 0.18)))
		(assert (Certainty_Factor (game_type Racing)	(cf 0.46)))
		(assert (Certainty_Factor (game_type Puzzle)	(cf 0.68)))
		(assert (Certainty_Factor (game_type Platform)	(cf 0.46)))
		(assert (Certainty_Factor (game_type MMORPG)	(cf 0.29)))
		(assert (Certainty_Factor (game_type TBS)		(cf 0.18)))
		(assert (Certainty_Factor (game_type Sports)	(cf 0.14)))
		(assert (Certainty_Factor (game_type MOBA)		(cf 0.14)))
		(assert (Certainty_Factor (game_type Simulation)(cf 0.54)))
		(assert (Certainty_Factor (game_type Rhythm)	(cf 0.43))))
	(case m then 	
		(assert (Certainty_Factor (game_type RPG)		(cf 0.52)))
		(assert (Certainty_Factor (game_type FTS)		(cf 0.51)))
		(assert (Certainty_Factor (game_type RTS)		(cf 0.52)))
		(assert (Certainty_Factor (game_type Racing)	(cf 0.36)))
		(assert (Certainty_Factor (game_type Puzzle)	(cf 0.26)))
		(assert (Certainty_Factor (game_type Platform)	(cf 0.30)))
		(assert (Certainty_Factor (game_type MMORPG)	(cf 0.35)))
		(assert (Certainty_Factor (game_type TBS)		(cf 0.38)))
		(assert (Certainty_Factor (game_type Sports)	(cf 0.39)))
		(assert (Certainty_Factor (game_type MOBA)		(cf 0.38)))
		(assert (Certainty_Factor (game_type Simulation)(cf 0.12)))
		(assert (Certainty_Factor (game_type Rhythm)	(cf 0.14))))
)
	
)

;**** Rule 1: Assert fact MBTI1.
(defrule MBTI1
?MBTI1 <- (User 
(gender ?gender)
(MBTI1 nil)
)
=>	(printout t crlf "Are you outwardly or inwardly focused? (i/e)")
	(bind ?response (read))
	(modify ?MBTI1 (MBTI1 ?response))
)


;**** Rule 2: Modify MBTI2 into same fact.
(defrule MBTI2
?MBTI2 <- (User 
(MBTI1 ?MBTI1)
(MBTI2 nil)
)
=>
(printout t crlf "How do you prefer to take in information? (s/n)")
(bind ?response (read))
(modify ?MBTI2 (MBTI2 ?response))
)

;**** Rule 3: Modify MBTI3 into same fact.
(defrule MBTI3
?MBTI3 <- (User 
(MBTI2 ?MBTI2)
(MBTI3 nil)
)
=>
(printout t crlf "How do you prefer to make decisions? (t/f)")
(bind ?response (read))
(modify ?MBTI3 (MBTI3 ?response))
)

;**** Rule 4: Modify MBTI4 into same fact.
(defrule MBTI4
?MBTI4 <- (User 
(MBTI3 ?MBTI3)
(MBTI4 nil)
)
=>
(printout t crlf "How do you prefer to live your outer life? (j/p)")
(bind ?response (read))
(modify ?MBTI4 (MBTI4 ?response))
)

;**** Rule 5: MBTI_Combined.
(defrule MBTI_Combined
?MBTI_Com <- (User 
(MBTI1 ?MBTI1)
(MBTI2 ?MBTI2)
(MBTI3 ?MBTI3)
(MBTI4 ?MBTI4)
(MBTI_Com nil)
)
=>
(printout t "Your MBTI is " ?MBTI1 ?MBTI2 ?MBTI3 ?MBTI4 crlf)
(modify ?MBTI_Com (MBTI_Com (sym-cat ?MBTI1 ?MBTI2 ?MBTI3 ?MBTI4))))


;**** Rule 6: Ask user MBTI1.
(defrule MBTI_Com
?DontLoop <- (User (MBTI_Com ?MBTI_Com))
=>	
(switch ?MBTI_Com
	(case istj then 	
		(assert (Certainty_Factor (game_type RPG)		(cf 0.59)))
		(assert (Certainty_Factor (game_type FTS)		(cf 0.35)))
		(assert (Certainty_Factor (game_type RTS)		(cf 0.53)))
		(assert (Certainty_Factor (game_type Racing)	(cf 0.35)))
		(assert (Certainty_Factor (game_type Puzzle)	(cf 0.47)))
		(assert (Certainty_Factor (game_type Platform)	(cf 0.35)))
		(assert (Certainty_Factor (game_type MMORPG)	(cf 0.47)))
		(assert (Certainty_Factor (game_type TBS)		(cf 0.35)))
		(assert (Certainty_Factor (game_type Sports)	(cf 0.24)))
		(assert (Certainty_Factor (game_type MOBA)		(cf 0.41)))
		(assert (Certainty_Factor (game_type Simulation)(cf 0.24)))
		(assert (Certainty_Factor (game_type Rhythm)	(cf 0.18))))
	(case istp then 	
		(assert (Certainty_Factor (game_type RPG)		(cf 0.45)))
		(assert (Certainty_Factor (game_type FTS)		(cf 0.36)))
		(assert (Certainty_Factor (game_type RTS)		(cf 0.45)))
		(assert (Certainty_Factor (game_type Racing)	(cf 0.36)))
		(assert (Certainty_Factor (game_type Puzzle)	(cf 0.18)))
		(assert (Certainty_Factor (game_type Platform)	(cf 0.09)))
		(assert (Certainty_Factor (game_type MMORPG)	(cf 0.09)))
		(assert (Certainty_Factor (game_type TBS)		(cf 0.36)))
		(assert (Certainty_Factor (game_type Sports)	(cf 0.45)))
		(assert (Certainty_Factor (game_type MOBA)		(cf 0.27)))
		(assert (Certainty_Factor (game_type Simulation)(cf 0.09)))
		(assert (Certainty_Factor (game_type Rhythm)	(cf 0.18))))
	(case intp then 	
		(assert (Certainty_Factor (game_type RPG)		(cf 0.30)))
		(assert (Certainty_Factor (game_type FTS)		(cf 0.60)))
		(assert (Certainty_Factor (game_type RTS)		(cf 0.40)))
		(assert (Certainty_Factor (game_type Racing)	(cf 0.20)))
		(assert (Certainty_Factor (game_type Puzzle)	(cf 0.20)))
		(assert (Certainty_Factor (game_type Platform)	(cf 0.20)))
		(assert (Certainty_Factor (game_type MMORPG)	(cf 0.20)))
		(assert (Certainty_Factor (game_type TBS)		(cf 0.30)))
		(assert (Certainty_Factor (game_type Sports)	(cf 0.30)))
		(assert (Certainty_Factor (game_type MOBA)		(cf 0.30)))
		(assert (Certainty_Factor (game_type Simulation)(cf 0.00)))
		(assert (Certainty_Factor (game_type Rhythm)	(cf 0.00))))
	(case infp then 	
		(assert (Certainty_Factor (game_type RPG)		(cf 0.50)))
		(assert (Certainty_Factor (game_type FTS)		(cf 0.70)))
		(assert (Certainty_Factor (game_type RTS)		(cf 0.30)))
		(assert (Certainty_Factor (game_type Racing)	(cf 0.50)))
		(assert (Certainty_Factor (game_type Puzzle)	(cf 0.20)))
		(assert (Certainty_Factor (game_type Platform)	(cf 0.50)))
		(assert (Certainty_Factor (game_type MMORPG)	(cf 0.30)))
		(assert (Certainty_Factor (game_type TBS)		(cf 0.10)))
		(assert (Certainty_Factor (game_type Sports)	(cf 0.50)))
		(assert (Certainty_Factor (game_type MOBA)		(cf 0.20)))
		(assert (Certainty_Factor (game_type Simulation)(cf 0.00)))
		(assert (Certainty_Factor (game_type Rhythm)	(cf 0.20))))
	(case isfj then 	
		(assert (Certainty_Factor (game_type RPG)		(cf 0.33)))
		(assert (Certainty_Factor (game_type FTS)		(cf 0.56)))
		(assert (Certainty_Factor (game_type RTS)		(cf 0.44)))
		(assert (Certainty_Factor (game_type Racing)	(cf 0.56)))
		(assert (Certainty_Factor (game_type Puzzle)	(cf 0.56)))
		(assert (Certainty_Factor (game_type Platform)	(cf 0.11)))
		(assert (Certainty_Factor (game_type MMORPG)	(cf 0.44)))
		(assert (Certainty_Factor (game_type TBS)		(cf 0.33)))
		(assert (Certainty_Factor (game_type Sports)	(cf 0.22)))
		(assert (Certainty_Factor (game_type MOBA)		(cf 0.22)))
		(assert (Certainty_Factor (game_type Simulation)(cf 0.44)))
		(assert (Certainty_Factor (game_type Rhythm)	(cf 0.56))))
	(case estp then 	
		(assert (Certainty_Factor (game_type RPG)		(cf 0.75)))
		(assert (Certainty_Factor (game_type FTS)		(cf 0.50)))
		(assert (Certainty_Factor (game_type RTS)		(cf 0.50)))
		(assert (Certainty_Factor (game_type Racing)	(cf 0.50)))
		(assert (Certainty_Factor (game_type Puzzle)	(cf 0.38)))
		(assert (Certainty_Factor (game_type Platform)	(cf 0.25)))
		(assert (Certainty_Factor (game_type MMORPG)	(cf 0.25)))
		(assert (Certainty_Factor (game_type TBS)		(cf 0.25)))
		(assert (Certainty_Factor (game_type Sports)	(cf 0.25)))
		(assert (Certainty_Factor (game_type MOBA)		(cf 0.25)))
		(assert (Certainty_Factor (game_type Simulation)(cf 0.13)))
		(assert (Certainty_Factor (game_type Rhythm)	(cf 0.13))))
	(case intj then 	
		(assert (Certainty_Factor (game_type RPG)		(cf 0.63)))
		(assert (Certainty_Factor (game_type FTS)		(cf 0.13)))
		(assert (Certainty_Factor (game_type RTS)		(cf 0.50)))
		(assert (Certainty_Factor (game_type Racing)	(cf 0.50)))
		(assert (Certainty_Factor (game_type Puzzle)	(cf 0.50)))
		(assert (Certainty_Factor (game_type Platform)	(cf 0.75)))
		(assert (Certainty_Factor (game_type MMORPG)	(cf 0.38)))
		(assert (Certainty_Factor (game_type TBS)		(cf 0.88)))
		(assert (Certainty_Factor (game_type Sports)	(cf 0.25)))
		(assert (Certainty_Factor (game_type MOBA)		(cf 0.25)))
		(assert (Certainty_Factor (game_type Simulation)(cf 0.38)))
		(assert (Certainty_Factor (game_type Rhythm)	(cf 0.13))))
	(case entp then 	
		(assert (Certainty_Factor (game_type RPG)		(cf 0.50)))
		(assert (Certainty_Factor (game_type FTS)		(cf 0.33)))
		(assert (Certainty_Factor (game_type RTS)		(cf 0.67)))
		(assert (Certainty_Factor (game_type Racing)	(cf 0.33)))
		(assert (Certainty_Factor (game_type Puzzle)	(cf 0.33)))
		(assert (Certainty_Factor (game_type Platform)	(cf 0.50)))
		(assert (Certainty_Factor (game_type MMORPG)	(cf 0.33)))
		(assert (Certainty_Factor (game_type TBS)		(cf 0.33)))
		(assert (Certainty_Factor (game_type Sports)	(cf 0.50)))
		(assert (Certainty_Factor (game_type MOBA)		(cf 0.50)))
		(assert (Certainty_Factor (game_type Simulation)(cf 0.17)))
		(assert (Certainty_Factor (game_type Rhythm)	(cf 0.00))))
	(case isfp then 	
		(assert (Certainty_Factor (game_type RPG)		(cf 0.67)))
		(assert (Certainty_Factor (game_type FTS)		(cf 0.67)))
		(assert (Certainty_Factor (game_type RTS)		(cf 0.50)))
		(assert (Certainty_Factor (game_type Racing)	(cf 0.50)))
		(assert (Certainty_Factor (game_type Puzzle)	(cf 0.33)))
		(assert (Certainty_Factor (game_type Platform)	(cf 0.83)))
		(assert (Certainty_Factor (game_type MMORPG)	(cf 0.33)))
		(assert (Certainty_Factor (game_type TBS)		(cf 0.33)))
		(assert (Certainty_Factor (game_type Sports)	(cf 0.33)))
		(assert (Certainty_Factor (game_type MOBA)		(cf 0.50)))
		(assert (Certainty_Factor (game_type Simulation)(cf 0.33)))
		(assert (Certainty_Factor (game_type Rhythm)	(cf 0.33))))
	(case esfj then 	
		(assert (Certainty_Factor (game_type RPG)		(cf 0.20)))
		(assert (Certainty_Factor (game_type FTS)		(cf 0.80)))
		(assert (Certainty_Factor (game_type RTS)		(cf 0.00)))
		(assert (Certainty_Factor (game_type Racing)	(cf 0.40)))
		(assert (Certainty_Factor (game_type Puzzle)	(cf 0.20)))
		(assert (Certainty_Factor (game_type Platform)	(cf 0.20)))
		(assert (Certainty_Factor (game_type MMORPG)	(cf 0.20)))
		(assert (Certainty_Factor (game_type TBS)		(cf 0.00)))
		(assert (Certainty_Factor (game_type Sports)	(cf 0.40)))
		(assert (Certainty_Factor (game_type MOBA)		(cf 0.60)))
		(assert (Certainty_Factor (game_type Simulation)(cf 0.40)))
		(assert (Certainty_Factor (game_type Rhythm)	(cf 0.20))))
	(case estj then 	
		(assert (Certainty_Factor (game_type RPG)		(cf 0.50)))
		(assert (Certainty_Factor (game_type FTS)		(cf 0.50)))
		(assert (Certainty_Factor (game_type RTS)		(cf 0.50)))
		(assert (Certainty_Factor (game_type Racing)	(cf 0.25)))
		(assert (Certainty_Factor (game_type Puzzle)	(cf 0.50)))
		(assert (Certainty_Factor (game_type Platform)	(cf 0.50)))
		(assert (Certainty_Factor (game_type MMORPG)	(cf 0.75)))
		(assert (Certainty_Factor (game_type TBS)		(cf 0.25)))
		(assert (Certainty_Factor (game_type Sports)	(cf 0.50)))
		(assert (Certainty_Factor (game_type MOBA)		(cf 0.50)))
		(assert (Certainty_Factor (game_type Simulation)(cf 0.25)))
		(assert (Certainty_Factor (game_type Rhythm)	(cf 0.25))))
	(case infj then 	
		(assert (Certainty_Factor (game_type RPG)		(cf 0.67)))
		(assert (Certainty_Factor (game_type FTS)		(cf 0.00)))
		(assert (Certainty_Factor (game_type RTS)		(cf 0.67)))
		(assert (Certainty_Factor (game_type Racing)	(cf 0.33)))
		(assert (Certainty_Factor (game_type Puzzle)	(cf 0.33)))
		(assert (Certainty_Factor (game_type Platform)	(cf 0.00)))
		(assert (Certainty_Factor (game_type MMORPG)	(cf 0.33)))
		(assert (Certainty_Factor (game_type TBS)		(cf 0.33)))
		(assert (Certainty_Factor (game_type Sports)	(cf 0.00)))
		(assert (Certainty_Factor (game_type MOBA)		(cf 0.33)))
		(assert (Certainty_Factor (game_type Simulation)(cf 0.33)))
		(assert (Certainty_Factor (game_type Rhythm)	(cf 0.33))))
	(case esfp then 	
		(assert (Certainty_Factor (game_type RPG)		(cf 0.33)))
		(assert (Certainty_Factor (game_type FTS)		(cf 0.33)))
		(assert (Certainty_Factor (game_type RTS)		(cf 0.33)))
		(assert (Certainty_Factor (game_type Racing)	(cf 0.33)))
		(assert (Certainty_Factor (game_type Puzzle)	(cf 0.67)))
		(assert (Certainty_Factor (game_type Platform)	(cf 0.33)))
		(assert (Certainty_Factor (game_type MMORPG)	(cf 0.33)))
		(assert (Certainty_Factor (game_type TBS)		(cf 0.33)))
		(assert (Certainty_Factor (game_type Sports)	(cf 0.33)))
		(assert (Certainty_Factor (game_type MOBA)		(cf 0.00)))
		(assert (Certainty_Factor (game_type Simulation)(cf 0.33)))
		(assert (Certainty_Factor (game_type Rhythm)	(cf 0.33))))
	(case enfp then 	
		(assert (Certainty_Factor (game_type RPG)		(cf 0.33)))
		(assert (Certainty_Factor (game_type FTS)		(cf 0.67)))
		(assert (Certainty_Factor (game_type RTS)		(cf 0.00)))
		(assert (Certainty_Factor (game_type Racing)	(cf 0.33)))
		(assert (Certainty_Factor (game_type Puzzle)	(cf 0.33)))
		(assert (Certainty_Factor (game_type Platform)	(cf 0.00)))
		(assert (Certainty_Factor (game_type MMORPG)	(cf 0.33)))
		(assert (Certainty_Factor (game_type TBS)		(cf 0.00)))
		(assert (Certainty_Factor (game_type Sports)	(cf 0.33)))
		(assert (Certainty_Factor (game_type MOBA)		(cf 0.00)))
		(assert (Certainty_Factor (game_type Simulation)(cf 0.33)))
		(assert (Certainty_Factor (game_type Rhythm)	(cf 0.33))))
	(case entj then 	
		(assert (Certainty_Factor (game_type RPG)		(cf 0.00)))
		(assert (Certainty_Factor (game_type FTS)		(cf 0.00)))
		(assert (Certainty_Factor (game_type RTS)		(cf 0.00)))
		(assert (Certainty_Factor (game_type Racing)	(cf 0.00)))
		(assert (Certainty_Factor (game_type Puzzle)	(cf 0.70)))
		(assert (Certainty_Factor (game_type Platform)	(cf 0.30)))
		(assert (Certainty_Factor (game_type MMORPG)	(cf 0.30)))
		(assert (Certainty_Factor (game_type TBS)		(cf 0.30)))
		(assert (Certainty_Factor (game_type Sports)	(cf 0.00)))
		(assert (Certainty_Factor (game_type MOBA)		(cf 0.00)))
		(assert (Certainty_Factor (game_type Simulation)(cf 0.30)))
		(assert (Certainty_Factor (game_type Rhythm)	(cf 0.30))))
	(case enfj then 	
		(assert (Certainty_Factor (game_type RPG)		(cf 0.50)))
		(assert (Certainty_Factor (game_type FTS)		(cf 0.50)))
		(assert (Certainty_Factor (game_type RTS)		(cf 0.50)))
		(assert (Certainty_Factor (game_type Racing)	(cf 0.50)))
		(assert (Certainty_Factor (game_type Puzzle)	(cf 0.50)))
		(assert (Certainty_Factor (game_type Platform)	(cf 0.50)))
		(assert (Certainty_Factor (game_type MMORPG)	(cf 0.50)))
		(assert (Certainty_Factor (game_type TBS)		(cf 0.50)))
		(assert (Certainty_Factor (game_type Sports)	(cf 0.50)))
		(assert (Certainty_Factor (game_type MOBA)		(cf 0.50)))
		(assert (Certainty_Factor (game_type Simulation)(cf 0.50)))
		(assert (Certainty_Factor (game_type Rhythm)	(cf 0.50))))
)
(modify ?DontLoop (MBTI_Com done))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Find max cf among game types;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffunction my-predicate 
(?cf1 ?cf2)
(> ?cf1 ?cf2))
   
(defrule find-max-value
(declare (salience -2))
?r1 <- (Certainty_Factor (game_type ?game_type) (cf ?cf1))
(not (Certainty_Factor (cf ?cf2&:(my-predicate ?cf2 ?cf1))))
(test (> ?cf1 0.72))
=>
(printout t ?game_type " is the recommended game type " (* ?cf1 100) "%" crlf)
(assert (Recommended_Game_Type ?game_type))
(retract ?r1)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Ask what the user look for in shooter game;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defrule FTS_Motivation
(declare (salience -3))
(Recommended_Game_Type FTS)
?FTS_Motivation <- (User 
(FTS_Motivation nil)
)
=>
(printout t "What do you look for in a shooter games? (Action/Social/Mastery/Achievement/Immersion/Creativity/Mobile)" crlf)
(bind ?response (read))
(modify ?FTS_Motivation (FTS_Motivation ?response))
)

;;;;;;;;;;;;;;;
;Shooter Games;
;;;;;;;;;;;;;;;

(defrule Action_Shooter
?RGT <- (Recommended_Game_Type FTS)
(User (FTS_Motivation Action))
=>
(assert (Recommended_Game (Game Call_of_Duty_WWII)))
(retract ?RGT)
)

(defrule Social_Shooter
?RGT <- (Recommended_Game_Type FTS)
(User (FTS_Motivation Social))
=>
(assert (Recommended_Game (Game Overwatch)))
(retract ?RGT)
)

(defrule Mastery_Shooter
?RGT <- (Recommended_Game_Type FTS)
(User (FTS_Motivation Mastery))
=>
(assert (Recommended_Game (Game Metal_Gear_Solid_V)))
(retract ?RGT)
)


(defrule Achievement_Shooter
?RGT <- (Recommended_Game_Type FTS)
(User (FTS_Motivation Achievement))
=>
(assert (Recommended_Game (Game Destiny_2)))
(retract ?RGT)
)


(defrule Immersion_Shooter
?RGT <- (Recommended_Game_Type FTS)
(User (FTS_Motivation Immersion))
=>
(assert (Recommended_Game (Game Bioshock_Infinite)))
(retract ?RGT)
)

(defrule Creativity_Shooter
?RGT <- (Recommended_Game_Type FTS)
(User (FTS_Motivation Creativity))
=>
(assert (Recommended_Game (Game Portal_2)))
(retract ?RGT)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Ask what the user look for in RPG game;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defrule RPG_Motivation
(declare (salience -3))
(Recommended_Game_Type RPG)
?RPG_Motivation <- (User 
(RPG_Motivation nil)
)
=>
(printout t "What do you look for in RPG game? (Action/Social/Mastery/Achievement/Immersion/Creativity/Mobile)" crlf)
(bind ?response (read))
(modify ?RPG_Motivation (RPG_Motivation ?response))
)

;;;;;;;;;;;;;;;;;;;;
;Role-Playing Games;
;;;;;;;;;;;;;;;;;;;;


(defrule Action_RPG
?RGT <- (Recommended_Game_Type RPG)
(User (RPG_Motivation Action))
=>
(assert (Recommended_Game (Game Grand_Theft_Auto)))
(retract ?RGT)
)

(defrule Social_RPG
?RGT <- (Recommended_Game_Type RPG)
(User (RPG_Motivation Social))
=>
(assert (Recommended_Game (Game Diablo_III)))
(retract ?RGT)
)

(defrule Mastery_RPG
?RGT <- (Recommended_Game_Type RPG)
(User (RPG_Motivation Mastery))
=>
(assert (Recommended_Game (Game Dark_Soul_III)))
(retract ?RGT)
)


(defrule Achievement_RPG
?RGT <- (Recommended_Game_Type RPG)
(User (RPG_Motivation Achievement))
=>
(assert (Recommended_Game (Game Witcher_3)))
(retract ?RGT)
)


(defrule Immersion_RPG
?RGT <- (Recommended_Game_Type RPG)
(User (RPG_Motivation Immersion))
=>
(assert (Recommended_Game (Game Final_Fantasy_Series)))
(retract ?RGT)
)

(defrule Creativity_RPG
?RGT <- (Recommended_Game_Type RPG)
(User (RPG_Motivation Creativity))
=>
(assert (Recommended_Game (Game Minecraft_Story_Mode)))
(retract ?RGT)
)

;;;;;;;;;;;;;;;
;Print results;
;;;;;;;;;;;;;;;

(defrule Print_Recommended_Game
(declare (salience -1000))
(Recommended_Game (Game ?Recommended_Game))
=>
(printout t "We recommend you "?Recommended_Game crlf)
)