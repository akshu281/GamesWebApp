
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



;**** Rule 0: Assert fact Gender.
(defrule Gender
(User (gender ?gender))
=>	
;;(printout t crlf "Are you male or female (m/f)")
;;(bind ?response (read))
;;(assert (User (gender ?response)))

	(switch ?gender
	(case f then 	
		(assert (Certainty_Factor (game_type Rhythm)	(cf 0.43))))
	(case m then 	
		(assert (Certainty_Factor (game_type Rhythm)	(cf 0.14))))
)
	
)

;**** Rule 1: Assert fact MBTI1.
(defrule MBTI1
?MBTI1 <- (User 
(MBTI1 ?MBTI1))
;;(MBTI2 nil)
;;?MBTI1 <- (User (gender ?gender))
=>	;;(printout t crlf "Are you outwardly or inwardly focused? (i/e)")
	;;(bind ?response (read))
	;;(modify ?MBTI1 (MBTI1 ?response))
	(modify ?MBTI1 (MBTI1 ?MBTI1))
)


;**** Rule 2: Modify MBTI2 into same fact.
(defrule MBTI2
?MBTI2 <- (User 
(MBTI1 ?MBTI1)
;;(MBTI2 nil)
)
=>
;;(printout t crlf "How do you prefer to take in information? (s/n)")
;;(bind ?response (read))
;;(modify ?MBTI2 (MBTI2 ?response))
(modify ?MBTI2 (MBTI2 ?MBTI2))
)

;**** Rule 3: Modify MBTI3 into same fact.
(defrule MBTI3
?MBTI3 <- (User 
(MBTI2 ?MBTI2)
;;(MBTI3 nil)
)
=>
;;(printout t crlf "How do you prefer to make decisions? (t/f)")
;;(bind ?response (read))
(modify ?MBTI3 (MBTI3 ?MBTI3))
)

;**** Rule 4: Modify MBTI4 into same fact.
(defrule MBTI4
?MBTI4 <- (User 
(MBTI3 ?MBTI3)
;;(MBTI4 nil)
)
=>
;;(printout t crlf "How do you prefer to live your outer life? (j/p)")
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
;;(MBTI_Com nil)
)
=>
;;(printout t "Your MBTI is " ?MBTI1 ?MBTI2 ?MBTI3 ?MBTI4 crlf)
;;(modify ?MBTI_Com (MBTI_Com (sym-cat ?MBTI1 ?MBTI2 ?MBTI3 ?MBTI4))))
(modify ?MBTI_Com (MBTI_Com ?MBTI_Com)))
;;;; Insert the missing block here !


;**** Rule 6: Ask user MBTI1.
(defrule MBTI_Com
(User (MBTI_Com ?MBTI_Com))
=>	
;(printout t "Your Certainty_Factor has been determined" crlf ) 
(switch ?MBTI_Com
	(case istj then 	
		(assert (Certainty_Factor (game_type RPG)(cf 0.59)))
		(printout t "Something")
	)
)

)

(defrule find-max-value
(declare (salience -2))
?r1 <- (Certainty_Factor (game_type ?game_type) (cf ?cf1))
(not (Certainty_Factor (cf ?cf2&:(my-predicate ?cf2 ?cf1))))
(test (> ?cf1 0.72))
=>
(printout t ?game_type " is the recommended game type " (* ?cf1 100) "%" crlf)
(assert (Certainty_Factor (game_type ?game_type)))
(retract ?r1)
)


;; Insert remaining code below the dashed line here