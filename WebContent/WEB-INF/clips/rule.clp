;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is a rule-based system for recommending games to
;; the user. It is an interactive system that makes use of certainty factors. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Allow facts to be duplicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule start 
(declare (salience 1000)) 
=> (set-fact-duplication TRUE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define control pattern facts to prevent looping of rules firing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffacts Control_Pattern
   (control 1)
   (control 2)
   (control 3)
   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftemplate User 
	(slot gender)
	(slot MBTI1	)
	(slot MBTI2	)
	(slot MBTI3	)
	(slot MBTI4	)
	(slot MBTI_Com)
	(slot Action)
	(slot Social)
	(slot Mastery)
	(slot Achievement)
	(slot Immersion)
	(slot Creativity)
)

(deftemplate Certainty_Factor 
	(slot game_type) 
	(slot cf))
	
(deftemplate Recommended_Game_Type 
	(slot rg1)
	(slot rg2) 
	(slot rg3))
	
(deftemplate Recommended_Game  
	(slot Game))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CF combination for multiple conclusions RULES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;combine POSITIVE cfs
(defrule combine-positive-cf
  ?f1 <- (Certainty_Factor (game_type ?g)(cf ?cf1&:(>= ?cf1 0)))
  ?f2 <- (Certainty_Factor (game_type ?g)(cf ?cf2&:(>= ?cf2 0)))
  (test (neq ?f1 ?f2)) ; test pointers and not value
  =>
  (retract ?f2)
  (modify ?f1 (cf =(+ ?cf1 (* ?cf2 (- 1 ?cf1)))))
)

;combine NEGATIVE cf
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

;**** Rule 0: Assert fact Gender.
(defrule Gender
?control <- (control 1)
?gender <- (User (gender ?response))
=>	
;;(printout t crlf "Are you male or female (m/f)")
;;(bind ?response (read))
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
(retract ?control)
)

;**** Rule 1: Assert fact MBTI1.
(defrule MBTI1
?MBTI1 <- (User 
(gender ?gender)
(MBTI1 nil)
)
=>	
(printout t crlf "Are you outwardly or inwardly focused? (i/e)")
;;(bind ?response (read))
(modify ?MBTI1 (MBTI1 ?MBTI1))
)


;**** Rule 2: Modify MBTI2 into same fact.
(defrule MBTI2
?MBTI2 <- (User 
(MBTI1 ?MBTI1)
(MBTI2 nil)
)
=>
(printout t crlf "How do you prefer to take in information? (s/n)")
;;(bind ?response (read))
(modify ?MBTI2 (MBTI2 ?MBTI2))
)

;**** Rule 3: Modify MBTI3 into same fact.
(defrule MBTI3
?MBTI3 <- (User 
(MBTI2 ?MBTI2)
(MBTI3 nil)
)
=>
(printout t crlf "How do you prefer to make decisions? (t/f)")
;;(bind ?response (read))
(modify ?MBTI3 (MBTI3 ?MBTI3))
)

;**** Rule 4: Modify MBTI4 into same fact.
(defrule MBTI4
?MBTI4 <- (User 
(MBTI3 ?MBTI3)
(MBTI4 nil)
)
=>
(printout t crlf "How do you prefer to live your outer life? (j/p)")
;;(bind ?response (read))
(modify ?MBTI4 (MBTI4 ?MBTI4))
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
;(printout t "Your MBTI is " ?MBTI1 ?MBTI2 ?MBTI3 ?MBTI4 crlf)
(modify ?MBTI_Com (MBTI_Com ?MBTI_Com))
)


;**** Rule 6: assert MBTI cf.
(defrule MBTI_Com
?control <- (control 2)
(User (MBTI_Com ?MBTI_Com))
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
(retract ?control)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Ask for User Motivation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule Action
?Action <- (User 
(MBTI_Com ?MBTI_Com)
(Action nil)
)
=>
(printout t "Does either of the following game type appeal to you: “Fast-Paced Action” or “Destruction”?  (y/n)" crlf)
;;(bind ?response (read))
(modify ?Action (Action ?Action))
)

(defrule Social
?Social <- (User 
(Action ?Action)
(Social nil)
)
=>
(printout t "Do you like games that has teamwork or competing with others? (y/n)" crlf)
;;(bind ?response (read))
(modify ?Social (Social ?Social))
)

(defrule Mastery
?Mastery <- (User 
(Social ?Social)
(Mastery nil)
)
=>
(printout t "How about games that are hard to be master or requires you to ponder and think ahead? (y/n)" crlf)
;;(bind ?response (read))
(modify ?Mastery (Mastery ?Mastery))
)

(defrule Achievement
?Achievement <- (User 
(Mastery ?Mastery)
(Achievement nil)
)
=>
(printout t "Do you like to complete every single possible quest/ achievement or acquire powerful skills and gears?  (y/n)" crlf)
;;(bind ?response (read))
(modify ?Achievement (Achievement ?Achievement))
)

(defrule Immersion
?Immersion <- (User 
(Achievement ?Achievement)
(Immersion nil)
)
=>
(printout t "Is a Fantasy Setting or Strong Storyline a must have for you? (y/n)" crlf)
;;(bind ?response (read))
(modify ?Immersion (Immersion ?Immersion))
)

(defrule Creativity
?Creativity <- (User 
(Immersion ?Immersion)
(Creativity nil)
)
=>
(printout t "Finally, is it preferred for the game to have your own personal touch or open to experimentation? (y/n)" crlf)
;;(bind ?response (read))
(modify ?Creativity (Creativity ?Creativity))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Do not allow facts to be duplicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule stop_duplicate 
(declare (salience -2)) 
=> (set-fact-duplication FALSE))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Find top 3 game types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffunction my-predicate 
(?cf1 ?cf2)
(> ?cf1 ?cf2))

   
(defrule find-max-value
(declare (salience -3))
?control <- (control 3)
?r1 <- (Certainty_Factor (game_type ?game_type) (cf ?cf1))
(not (Certainty_Factor (cf ?cf2&:(my-predicate ?cf2 ?cf1))))
=>
(printout t ?game_type " is the recommended game type " (* ?cf1 100) "%" crlf)
(assert (Recommended_Game_Type (rg1 ?game_type)))
(retract ?r1 ?control)
)

(defrule find-max-value2
(declare (salience -3))
?rg2 <- (Recommended_Game_Type (rg1 ?rg1) (rg2 nil))
?r1 <- (Certainty_Factor (game_type ?game_type) (cf ?cf1))
(not (Certainty_Factor (cf ?cf2&:(my-predicate ?cf2 ?cf1))))
=>
(printout t ?game_type " is the recommended game type " (* ?cf1 100) "%" crlf)
(modify  ?rg2 (rg2 ?game_type))
(retract ?r1)
)

(defrule find-max-value3
(declare (salience -3))
?rg3 <- (Recommended_Game_Type (rg2 ?rg2) (rg3 nil))
?r1 <- (Certainty_Factor (game_type ?game_type) (cf ?cf1))
(not (Certainty_Factor (cf ?cf2&:(my-predicate ?cf2 ?cf1))))
=>
(printout t ?game_type " is the recommended game type " (* ?cf1 100) "%" crlf)
(modify  ?rg3 (rg3 ?game_type))
(retract ?r1)
)


;;;;;;;;;;;;;;;
;Shooter Games;
;;;;;;;;;;;;;;;

(defrule Action_Shooter
(or 
(Recommended_Game_Type (rg1 FTS))
(Recommended_Game_Type (rg2 FTS))
(Recommended_Game_Type (rg3 FTS))
)
(User (Action y))
=>
(assert (Recommended_Game (Game Shooter:Call_of_Duty_WWII_$38.99)))
)

(defrule Social_Shooter
(or 
(Recommended_Game_Type (rg1 FTS))
(Recommended_Game_Type (rg2 FTS))
(Recommended_Game_Type (rg3 FTS))
)
(User (Social y))
=>
(assert (Recommended_Game (Game Shooter:Overwatch_$20.00)))
)

(defrule Mastery_Shooter
(or 
(Recommended_Game_Type (rg1 FTS))
(Recommended_Game_Type (rg2 FTS))
(Recommended_Game_Type (rg3 FTS))
)
(User (Mastery y))
=>
(assert (Recommended_Game (Game Shooter:Metal_Gear_Solid_V_$42.00)))
)


(defrule Achievement_Shooter
(or 
(Recommended_Game_Type (rg1 FTS))
(Recommended_Game_Type (rg2 FTS))
(Recommended_Game_Type (rg3 FTS))
)
(User (Achievement y))
=>
(assert (Recommended_Game (Game Shooter:Destiny_2_$59.99)))
)


(defrule Immersion_Shooter
(or 
(Recommended_Game_Type (rg1 FTS))
(Recommended_Game_Type (rg2 FTS))
(Recommended_Game_Type (rg3 FTS))
)
(User (Immersion y))
=>
(assert (Recommended_Game (Game Shooter:Bioshock_Infinite_$29.00)))
)

(defrule Creativity_Shooter
(or 
(Recommended_Game_Type (rg1 FTS))
(Recommended_Game_Type (rg2 FTS))
(Recommended_Game_Type (rg3 FTS))
)
(User (Creativity y))
=>
(assert (Recommended_Game (Game Shooter:Portal_2_$18.50)))
)


;;;;;;;;;;;;;;;
;RPG Games;
;;;;;;;;;;;;;;;

(defrule Action_RPG
(or 
(Recommended_Game_Type (rg1 RPG))
(Recommended_Game_Type (rg2 RPG))
(Recommended_Game_Type (rg3 RPG))
)
(User (Action y))
=>
(assert (Recommended_Game (Game RPG:Grand_Theft_Auto_Collection_$49.00)))
)

(defrule Social_RPG
(or 
(Recommended_Game_Type (rg1 RPG))
(Recommended_Game_Type (rg2 RPG))
(Recommended_Game_Type (rg3 RPG))
)
(User (Social y))
=>
(assert (Recommended_Game (Game RPG:Diablo_III_$29.99)))
)

(defrule Mastery_RPG
(or 
(Recommended_Game_Type (rg1 RPG))
(Recommended_Game_Type (rg2 RPG))
(Recommended_Game_Type (rg3 RPG))
)
(User (Mastery y))
=>
(assert (Recommended_Game (Game RPG:Dark_Soul_III_$59.90)))
)


(defrule Achievement_RPG
(or 
(Recommended_Game_Type (rg1 RPG))
(Recommended_Game_Type (rg2 RPG))
(Recommended_Game_Type (rg3 RPG))
)
(User (Achievement y))
=>
(assert (Recommended_Game (Game RPG:Witcher_3_$50.39)))
)


(defrule Immersion_RPG
(or 
(Recommended_Game_Type (rg1 RPG))
(Recommended_Game_Type (rg2 RPG))
(Recommended_Game_Type (rg3 RPG))
)
(User (Immersion y))
=>
(assert (Recommended_Game (Game RPG:Final_Fantasy_XV_$69.90)))
)

(defrule Creativity_RPG
(or 
(Recommended_Game_Type (rg1 RPG))
(Recommended_Game_Type (rg2 RPG))
(Recommended_Game_Type (rg3 RPG))
)
(User (Creativity y))
=>
(assert (Recommended_Game (Game RPG:Minecraft_Story_Mode_$25.00)))
)


;;;;;;;;;;;;;;;
;Platform
;;;;;;;;;;;;;;;

(defrule Action_Platform
(or 
(Recommended_Game_Type (rg1 Platform))
(Recommended_Game_Type (rg2 Platform))
(Recommended_Game_Type (rg3 Platform))
)
(User (Action y))
=>
(assert (Recommended_Game (Game Platform:Unbox_Newbie_Adventure_$20.00)))
)

(defrule Social_Platform
(or 
(Recommended_Game_Type (rg1 Platform))
(Recommended_Game_Type (rg2 Platform))
(Recommended_Game_Type (rg3 Platform))
)
(User (Social y))
=>
(assert (Recommended_Game (Game Platform:Trine_2_Complete_Story_$20.00)))
)

(defrule Mastery_Platform
(or 
(Recommended_Game_Type (rg1 Platform))
(Recommended_Game_Type (rg2 Platform))
(Recommended_Game_Type (rg3 Platform))
)
(User (Mastery y))
=>
(assert (Recommended_Game (Game Platform:Getting_Over_It_with_Bennett_Foddy_$8.50)))
)


(defrule Achievement_Platform
(or 
(Recommended_Game_Type (rg1 Platform))
(Recommended_Game_Type (rg2 Platform))
(Recommended_Game_Type (rg3 Platform))
)
(User (Achievement y))
=>
(assert (Recommended_Game (Game Platform:Hollow_Knight_$15.00)))
)


(defrule Immersion_Platform
(or 
(Recommended_Game_Type (rg1 Platform))
(Recommended_Game_Type (rg2 Platform))
(Recommended_Game_Type (rg3 Platform))
)
(User (Immersion y))
=>
(assert (Recommended_Game (Game Platform:Night_in_the_wood_$14.00)))
)

(defrule Creativity_Platform
(or 
(Recommended_Game_Type (rg1 Platform))
(Recommended_Game_Type (rg2 Platform))
(Recommended_Game_Type (rg3 Platform))
)
(User (Creativity y))
=>
(assert (Recommended_Game (Game Platform:Human_Fall_Flat_$15.00)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Turn-Based Strategy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule Action_TBS
(or 
(Recommended_Game_Type (rg1 TBS))
(Recommended_Game_Type (rg2 TBS))
(Recommended_Game_Type (rg3 TBS))
)
(User (Action y))
=>
(assert (Recommended_Game (Game TBS:Total_War_Shogun_$29.00)))
)

(defrule Social_TBS
(or 
(Recommended_Game_Type (rg1 TBS))
(Recommended_Game_Type (rg2 TBS))
(Recommended_Game_Type (rg3 TBS))
)
(User (Social y))
=>
(assert (Recommended_Game (Game TBS:Gremlins_Inc_$18.00)))
)

(defrule Mastery_TBS
(or 
(Recommended_Game_Type (rg1 TBS))
(Recommended_Game_Type (rg2 TBS))
(Recommended_Game_Type (rg3 TBS))
)
(User (Mastery y))
=>
(assert (Recommended_Game (Game TBS:Civilization_V_$29.00)))
)


(defrule Achievement_TBS
(or 
(Recommended_Game_Type (rg1 TBS))
(Recommended_Game_Type (rg2 TBS))
(Recommended_Game_Type (rg3 TBS))
)
(User (Achievement y))
=>
(assert (Recommended_Game (Game TBS:XCOM2_$79.90)))
)


(defrule Immersion_TBS
(or 
(Recommended_Game_Type (rg1 TBS))
(Recommended_Game_Type (rg2 TBS))
(Recommended_Game_Type (rg3 TBS))
)
(User (Immersion y))
=>
(assert (Recommended_Game (Game TBS:Final_Fantasy_VII_$14.99)))
)

(defrule Creativity_TBS
(or 
(Recommended_Game_Type (rg1 TBS))
(Recommended_Game_Type (rg2 TBS))
(Recommended_Game_Type (rg3 TBS))
)
(User (Creativity y))
=>
(assert (Recommended_Game (Game TBS:Divinity_Original_Sin_2_$49.00)))
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;MMORPG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule Action_MMORPG
(or 
(Recommended_Game_Type (rg1 MMORPG))
(Recommended_Game_Type (rg2 MMORPG))
(Recommended_Game_Type (rg3 MMORPG))
)
(User (Action y))
=>
(assert (Recommended_Game (Game MMORPG:Path_of_Exile_$0)))
)

(defrule Social_MMORPG
(or 
(Recommended_Game_Type (rg1 MMORPG))
(Recommended_Game_Type (rg2 MMORPG))
(Recommended_Game_Type (rg3 MMORPG))
)
(User (Social y))
=>
(assert (Recommended_Game (Game MMORPG:Shakes_and_Fidget_$0)))
)

(defrule Mastery_MMORPG
(or 
(Recommended_Game_Type (rg1 MMORPG))
(Recommended_Game_Type (rg2 MMORPG))
(Recommended_Game_Type (rg3 MMORPG))
)
(User (Mastery y))
=>
(assert (Recommended_Game (Game MMORPG:Anarchy_Online_$0)))
)


(defrule Achievement_MMORPG
(or 
(Recommended_Game_Type (rg1 MMORPG))
(Recommended_Game_Type (rg2 MMORPG))
(Recommended_Game_Type (rg3 MMORPG))
)
(User (Achievement y))
=>
(assert (Recommended_Game (Game MMORPG:World_of_Warcraft_$49.99)))
)


(defrule Immersion_MMORPG
(or 
(Recommended_Game_Type (rg1 MMORPG))
(Recommended_Game_Type (rg2 MMORPG))
(Recommended_Game_Type (rg3 MMORPG))
)
(User (Immersion y))
=>
(assert (Recommended_Game (Game MMORPG:FINAL_FANTASY_XIV_$24.00)))
)

(defrule Creativity_MMORPG
(or 
(Recommended_Game_Type (rg1 MMORPG))
(Recommended_Game_Type (rg2 MMORPG))
(Recommended_Game_Type (rg3 MMORPG))
)
(User (Creativity y))
=>
(assert (Recommended_Game (Game MMORPG:Chronicles_of_Elyria_$0)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;RTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule Action_RTS
(or 
(Recommended_Game_Type (rg1 RTS))
(Recommended_Game_Type (rg2 RTS))
(Recommended_Game_Type (rg3 RTS))
)
(User (Action y))
=>
(assert (Recommended_Game (Game RTS:Supreme_Commander_$15.00)))
)

(defrule Social_RTS
(or 
(Recommended_Game_Type (rg1 RTS))
(Recommended_Game_Type (rg2 RTS))
(Recommended_Game_Type (rg3 RTS))
)
(User (Social y))
=>
(assert (Recommended_Game (Game RTS:Starcraft_II_$39.99)))
)

(defrule Mastery_RTS
(or 
(Recommended_Game_Type (rg1 RTS))
(Recommended_Game_Type (rg2 RTS))
(Recommended_Game_Type (rg3 RTS))
)
(User (Mastery y))
=>
(assert (Recommended_Game (Game RTS:Shadow_Tactics_Blades_of_the_Shogun_$39.00)))
)


(defrule Achievement_RTS
(or 
(Recommended_Game_Type (rg1 RTS))
(Recommended_Game_Type (rg2 RTS))
(Recommended_Game_Type (rg3 RTS))
)
(User (Achievement y))
=>
(assert (Recommended_Game (Game RTS:Warhammer_40000_$45.89)))
)


(defrule Immersion_RTS
(or 
(Recommended_Game_Type (rg1 RTS))
(Recommended_Game_Type (rg2 RTS))
(Recommended_Game_Type (rg3 RTS))
)
(User (Immersion y))
=>
(assert (Recommended_Game (Game RTS:Crusader_Kings_II_$39.00)))
)

(defrule Creativity_RTS
(or 
(Recommended_Game_Type (rg1 RTS))
(Recommended_Game_Type (rg2 RTS))
(Recommended_Game_Type (rg3 RTS))
)
(User (Creativity y))
=>
(assert (Recommended_Game (Game RTS:Stellaris_$15.60)))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Racing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule Action_Racing
(or 
(Recommended_Game_Type (rg1 Racing))
(Recommended_Game_Type (rg2 Racing))
(Recommended_Game_Type (rg3 Racing))
)
(User (Action y))
=>
(assert (Recommended_Game (Game Racing:Burnout_Paradise_$24.90)))
)

(defrule Social_Racing
(or 
(Recommended_Game_Type (rg1 Racing))
(Recommended_Game_Type (rg2 Racing))
(Recommended_Game_Type (rg3 Racing))
)
(User (Social y))
=>
(assert (Recommended_Game (Game Racing:Sonic_All_Stars_Racing_$20.00)))
)

(defrule Mastery_Racing
(or 
(Recommended_Game_Type (rg1 Racing))
(Recommended_Game_Type (rg2 Racing))
(Recommended_Game_Type (rg3 Racing))
)
(User (Mastery y))
=>
(assert (Recommended_Game (Game Racing:F1_2017_$55.00)))
)


(defrule Achievement_Racing
(or 
(Recommended_Game_Type (rg1 Racing))
(Recommended_Game_Type (rg2 Racing))
(Recommended_Game_Type (rg3 Racing))
)
(User (Achievement y))
=>
(assert (Recommended_Game (Game Racing:Forza_Horizon_3_$62.25)))
)


(defrule Immersion_Racing
(or 
(Recommended_Game_Type (rg1 Racing))
(Recommended_Game_Type (rg2 Racing))
(Recommended_Game_Type (rg3 Racing))
)
(User (Immersion y))
=>
(assert (Recommended_Game (Game Racing:Shift_2_$24.90)))
)

(defrule Creativity_Racing
(or 
(Recommended_Game_Type (rg1 Racing))
(Recommended_Game_Type (rg2 Racing))
(Recommended_Game_Type (rg3 Racing))
)
(User (Creativity y))
=>
(assert (Recommended_Game (Game Racing:Trailmakers_$20.00)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Simulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule Action_Simulation
(or 
(Recommended_Game_Type (rg1 Simulation))
(Recommended_Game_Type (rg2 Simulation))
(Recommended_Game_Type (rg3 Simulation))
)
(User (Action y))
=>
(assert (Recommended_Game (Game Simulation:RimWorld_$26.00)))
)

(defrule Social_Simulation
(or 
(Recommended_Game_Type (rg1 Simulation))
(Recommended_Game_Type (rg2 Simulation))
(Recommended_Game_Type (rg3 Simulation))
)
(User (Social y))
=>
(assert (Recommended_Game (Game Simulation:Tropico_5_$18.50)))
)

(defrule Mastery_Simulation
(or 
(Recommended_Game_Type (rg1 Simulation))
(Recommended_Game_Type (rg2 Simulation))
(Recommended_Game_Type (rg3 Simulation))
)
(User (Mastery y))
=>
(assert (Recommended_Game (Game Simulation:Rise_to_Ruins_$10.50)))
)


(defrule Achievement_Simulation
(or 
(Recommended_Game_Type (rg1 Simulation))
(Recommended_Game_Type (rg2 Simulation))
(Recommended_Game_Type (rg3 Simulation))
)
(User (Achievement y))
=>
(assert (Recommended_Game (Game Simulation:Harvest_Moon_$26.00)))
)


(defrule Immersion_Simulation
(or 
(Recommended_Game_Type (rg1 Simulation))
(Recommended_Game_Type (rg2 Simulation))
(Recommended_Game_Type (rg3 Simulation))
)
(User (Immersion y))
=>
(assert (Recommended_Game (Game Simulation:This_War_of_Mine_$20.00)))
)

(defrule Creativity_Simulation
(or 
(Recommended_Game_Type (rg1 Simulation))
(Recommended_Game_Type (rg2 Simulation))
(Recommended_Game_Type (rg3 Simulation))
)
(User (Creativity y))
=>
(assert (Recommended_Game (Game Simulation:The_Sims_3_$24.90)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;MOBA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule Action_MOBA
(or 
(Recommended_Game_Type (rg1 MOBA))
(Recommended_Game_Type (rg2 MOBA))
(Recommended_Game_Type (rg3 MOBA))
)
(User (Action y))
=>
(assert (Recommended_Game (Game MOBA:Corroded_$13.00)))
)

(defrule Social_MOBA
(or 
(Recommended_Game_Type (rg1 MOBA))
(Recommended_Game_Type (rg2 MOBA))
(Recommended_Game_Type (rg3 MOBA))
)
(User (Social y))
=>
(assert (Recommended_Game (Game MOBA:Strife_$0)))
)

(defrule Mastery_MOBA
(or 
(Recommended_Game_Type (rg1 MOBA))
(Recommended_Game_Type (rg2 MOBA))
(Recommended_Game_Type (rg3 MOBA))
)
(User (Mastery y))
=>
(assert (Recommended_Game (Game MOBA:Dota_2_$0)))
)


(defrule Achievement_MOBA
(or 
(Recommended_Game_Type (rg1 MOBA))
(Recommended_Game_Type (rg2 MOBA))
(Recommended_Game_Type (rg3 MOBA))
)
(User (Achievement y))
=>
(assert (Recommended_Game (Game MOBA:Atlas_Reactor_$0)))
)


(defrule Immersion_MOBA
(or 
(Recommended_Game_Type (rg1 MOBA))
(Recommended_Game_Type (rg2 MOBA))
(Recommended_Game_Type (rg3 MOBA))
)
(User (Immersion y))
=>
(assert (Recommended_Game (Game MOBA:Battlerite_$0)))
)

(defrule Creativity_MOBA
(or 
(Recommended_Game_Type (rg1 MOBA))
(Recommended_Game_Type (rg2 MOBA))
(Recommended_Game_Type (rg3 MOBA))
)
(User (Creativity y))
=>
(assert (Recommended_Game (Game MOBA:Smite_$0)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Puzzle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule Action_Puzzle
(or 
(Recommended_Game_Type (rg1 Puzzle))
(Recommended_Game_Type (rg2 Puzzle))
(Recommended_Game_Type (rg3 Puzzle))
)
(User (Action y))
=>
(assert (Recommended_Game (Game Puzzle:Little_Inferno_$10.00)))
)

(defrule Social_Puzzle
(or 
(Recommended_Game_Type (rg1 Puzzle))
(Recommended_Game_Type (rg2 Puzzle))
(Recommended_Game_Type (rg3 Puzzle))
)
(User (Social y))
=>
(assert (Recommended_Game (Game Puzzle:Fat_Mask_$10.00)))
)

(defrule Mastery_Puzzle
(or 
(Recommended_Game_Type (rg1 Puzzle))
(Recommended_Game_Type (rg2 Puzzle))
(Recommended_Game_Type (rg3 Puzzle))
)
(User (Mastery y))
=>
(assert (Recommended_Game (Game Puzzle:World_of_Goo_$10.00)))
)


(defrule Achievement_Puzzle
(or 
(Recommended_Game_Type (rg1 Puzzle))
(Recommended_Game_Type (rg2 Puzzle))
(Recommended_Game_Type (rg3 Puzzle))
)
(User (Achievement y))
=>
(assert (Recommended_Game (Game Puzzle:Gorogoa_$14.50)))
)


(defrule Immersion_Puzzle
(or 
(Recommended_Game_Type (rg1 Puzzle))
(Recommended_Game_Type (rg2 Puzzle))
(Recommended_Game_Type (rg3 Puzzle))
)
(User (Immersion y))
=>
(assert (Recommended_Game (Game Puzzle:Inside_$20)))
)

(defrule Creativity_Puzzle
(or 
(Recommended_Game_Type (rg1 Puzzle))
(Recommended_Game_Type (rg2 Puzzle))
(Recommended_Game_Type (rg3 Puzzle))
)
(User (Creativity y))
=>
(assert (Recommended_Game (Game Puzzle:Scribblenauts_Unlimited_$20)))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Rhythm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule Action_Rhythm
(or 
(Recommended_Game_Type (rg1 Rhythm))
(Recommended_Game_Type (rg2 Rhythm))
(Recommended_Game_Type (rg3 Rhythm))
)
(User (Action y))
=>
(assert (Recommended_Game (Game Rhythm:One_Finger_Death_Punch_$5.25)))
)

(defrule Social_Rhythm
(or 
(Recommended_Game_Type (rg1 Rhythm))
(Recommended_Game_Type (rg2 Rhythm))
(Recommended_Game_Type (rg3 Rhythm))
)
(User (Social y))
=>
(assert (Recommended_Game (Game Rhythm:The_Metronomicon_$20.00)))
)

(defrule Mastery_Rhythm
(or 
(Recommended_Game_Type (rg1 Rhythm))
(Recommended_Game_Type (rg2 Rhythm))
(Recommended_Game_Type (rg3 Rhythm))
)
(User (Mastery y))
=>
(assert (Recommended_Game (Game Rhythm:Super_Hexagon_$3.50)))
)


(defrule Achievement_Rhythm
(or 
(Recommended_Game_Type (rg1 Rhythm))
(Recommended_Game_Type (rg2 Rhythm))
(Recommended_Game_Type (rg3 Rhythm))
)
(User (Achievement y))
=>
(assert (Recommended_Game (Game Rhythm:Aaero_$14.50)))
)


(defrule Immersion_Rhythm
(or 
(Recommended_Game_Type (rg1 Rhythm))
(Recommended_Game_Type (rg2 Rhythm))
(Recommended_Game_Type (rg3 Rhythm))
)
(User (Immersion y))
=>
(assert (Recommended_Game (Game Rhythm:Crypt_of_the_NecroDancer_$15.00)))
)

(defrule Creativity_Rhythm
(or 
(Recommended_Game_Type (rg1 Rhythm))
(Recommended_Game_Type (rg2 Rhythm))
(Recommended_Game_Type (rg3 Rhythm))
)
(User (Creativity y))
=>
(assert (Recommended_Game (Game Rhythm:Airtone_$29.00)))
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Sports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule Action_Sports
(or 
(Recommended_Game_Type (rg1 Sports))
(Recommended_Game_Type (rg2 Sports))
(Recommended_Game_Type (rg3 Sports))
)
(User (Action y))
=>
(assert (Recommended_Game (Game Sports:Mutant_Football_League_$18.50)))
)

(defrule Social_Sports
(or 
(Recommended_Game_Type (rg1 Sports))
(Recommended_Game_Type (rg2 Sports))
(Recommended_Game_Type (rg3 Sports))
)
(User (Social y))
=>
(assert (Recommended_Game (Game Sports:Rocket_League_$20.00)))
)

(defrule Mastery_Sports
(or 
(Recommended_Game_Type (rg1 Sports))
(Recommended_Game_Type (rg2 Sports))
(Recommended_Game_Type (rg3 Sports))
)
(User (Mastery y))
=>
(assert (Recommended_Game (Game Sports:Lethal_League_$14.00)))
)


(defrule Achievement_Sports
(or 
(Recommended_Game_Type (rg1 Sports))
(Recommended_Game_Type (rg2 Sports))
(Recommended_Game_Type (rg3 Sports))
)
(User (Achievement y))
=>
(assert (Recommended_Game (Game Sports:Football_Manager_$55.00)))
)


(defrule Immersion_Sports
(or 
(Recommended_Game_Type (rg1 Sports))
(Recommended_Game_Type (rg2 Sports))
(Recommended_Game_Type (rg3 Sports))
)
(User (Immersion y))
=>
(assert (Recommended_Game (Game Sports:Behold_the_Kickmen_$4.50)))
)

(defrule Creativity_Sports
(or 
(Recommended_Game_Type (rg1 Sports))
(Recommended_Game_Type (rg2 Sports))
(Recommended_Game_Type (rg3 Sports))
)
(User (Creativity y))
=>
(assert (Recommended_Game (Game Sports:Superflight_$3.25)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;No Motivation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule NM_FTS
(or 
(Recommended_Game_Type (rg1 FTS))
(Recommended_Game_Type (rg2 FTS))
(Recommended_Game_Type (rg3 FTS))
)
(User (Action n)(Social n)(Mastery n)(Achievement n)(Immersion n)(Creativity n))
=>
(assert (Recommended_Game (Game M_Shooter:Critical_Ops_$0)))
)


(defrule NM_RPG
(or 
(Recommended_Game_Type (rg1 RPG))
(Recommended_Game_Type (rg2 RPG))
(Recommended_Game_Type (rg3 RPG))
)
(User (Action n)(Social n)(Mastery n)(Achievement n)(Immersion n)(Creativity n))
=>
(assert (Recommended_Game (Game M_RPG:Doom_and_Destiny_Advanced_$4.99)))
)


(defrule NM_Platform
(or 
(Recommended_Game_Type (rg1 Platform))
(Recommended_Game_Type (rg2 Platform))
(Recommended_Game_Type (rg3 Platform))
)
(User (Action n)(Social n)(Mastery n)(Achievement n)(Immersion n)(Creativity n))
=>
(assert (Recommended_Game (Game M_Platform:Platform:Heart_Star_$0)))
)


(defrule NM_TBS
(or 
(Recommended_Game_Type (rg1 TBS))
(Recommended_Game_Type (rg2 TBS))
(Recommended_Game_Type (rg3 TBS))
)
(User (Action n)(Social n)(Mastery n)(Achievement n)(Immersion n)(Creativity n))
=>
(assert (Recommended_Game (Game M_TBS:Final_Fantasy_Tactics_$0)))
)


(defrule NM_MMORPG
(or 
(Recommended_Game_Type (rg1 MMORPG))
(Recommended_Game_Type (rg2 MMORPG))
(Recommended_Game_Type (rg3 MMORPG))
)
(User (Action n)(Social n)(Mastery n)(Achievement n)(Immersion n)(Creativity n))
=>
(assert (Recommended_Game (Game M_MMORPG:Aurcus_Online_$0)))
)

(defrule NM_RTS
(or 
(Recommended_Game_Type (rg1 RTS))
(Recommended_Game_Type (rg2 RTS))
(Recommended_Game_Type (rg3 RTS))
)
(User (Action n)(Social n)(Mastery n)(Achievement n)(Immersion n)(Creativity n))
=>
(assert (Recommended_Game (Game M_RTS:Titanfall_Assault_$0)))
)


(defrule NM_Racing
(or 
(Recommended_Game_Type (rg1 Racing))
(Recommended_Game_Type (rg2 Racing))
(Recommended_Game_Type (rg3 Racing))
)
(User (Action n)(Social n)(Mastery n)(Achievement n)(Immersion n)(Creativity n))
=>
(assert (Recommended_Game (Game M_Racing:Asphalt_Xtreme_$0)))
)


(defrule NM_Simulation
(or 
(Recommended_Game_Type (rg1 Simulation))
(Recommended_Game_Type (rg2 Simulation))
(Recommended_Game_Type (rg3 Simulation))
)
(User (Action n)(Social n)(Mastery n)(Achievement n)(Immersion n)(Creativity n))
=>
(assert (Recommended_Game (Game M_Simulation:Fallout_Shelter_$0)))
)


(defrule NM_MOBA
(or 
(Recommended_Game_Type (rg1 MOBA))
(Recommended_Game_Type (rg2 MOBA))
(Recommended_Game_Type (rg3 MOBA))
)
(User (Action n)(Social n)(Mastery n)(Achievement n)(Immersion n)(Creativity n))
=>
(assert (Recommended_Game (Game M_MOBA:Mobile_Legend_$0)))
)


(defrule NM_Puzzle
(or 
(Recommended_Game_Type (rg1 Puzzle))
(Recommended_Game_Type (rg2 Puzzle))
(Recommended_Game_Type (rg3 Puzzle))
)
(User (Action n)(Social n)(Mastery n)(Achievement n)(Immersion n)(Creativity n))
=>
(assert (Recommended_Game (Game M_Puzzle:Candy_Crush_$0)))
)


(defrule NM_Sports
(or 
(Recommended_Game_Type (rg1 Sports))
(Recommended_Game_Type (rg2 Sports))
(Recommended_Game_Type (rg3 Sports))
)
(User (Action n)(Social n)(Mastery n)(Achievement n)(Immersion n)(Creativity n))
=>
(assert (Recommended_Game (Game M_Sports:Stickman_Soccer_2016_$0)))
)


(defrule NM_Rhythm
(or 
(Recommended_Game_Type (rg1 Rhythm))
(Recommended_Game_Type (rg2 Rhythm))
(Recommended_Game_Type (rg3 Rhythm))
)
(User (Action n)(Social n)(Mastery n)(Achievement n)(Immersion n)(Creativity n))
=>
(assert (Recommended_Game (Game M_Rhythm:Deemo_$0)))
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Print Recommended Game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule Print_Recommended_Game
(declare (salience -1000))
(Recommended_Game (Game ?Recommended_Game))
=>
(printout t "We recommend you "?Recommended_Game crlf)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Troubleshoot with JAVA, find all facts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(deffunction find-games ()
;;(bind ?facts (find-all-facts ((?f Certainty_Factor)) (> ?f:cf 0.00)))
;;)
	
(deffunction find-games-list ()
	(bind ?facts (find-all-facts ((?f Recommended_Game)) (!= (str-compare ?f:Game "nil") 0)))
)