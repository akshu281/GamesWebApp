;How to use allowed symbols to restrict values for MBTI?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Templates for certainty factor to work
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftemplate working_goal (slot goal) (slot cf))
(deftemplate recomendation (slot OW) (slot FTS) (slot RTS))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CF combination for multiple conclusions RULES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;combine POSITIVE certainty factors for multiple conclusions
;cf(cf1,cf2) = cf1 + cf2 * (1- cf1)

(defrule combine-positive-cf
  ?f1 <- (working_goal (goal ?g)(cf ?cf1&:(>= ?cf1 0)))
  ?f2 <- (working_goal (goal ?g)(cf ?cf2&:(>= ?cf2 0)))
  (test (neq ?f1 ?f2)) ; test pointers and not value
  =>
  (retract ?f2)
  (modify ?f1 (cf =(+ ?cf1 (* ?cf2 (- 1 ?cf1)))))
)

;combine NEGATIVE cf
;cf(cf1,cf2) = cf1 + cf2 * (1 + cf1)

(defrule combine-neg-cf
 (declare (salience -1))
  ?f1 <- (working_goal   (goal ?g)(cf ?cf1&:(< ?cf1 0)))
  ?f2 <- (working_goal (goal ?g)(cf ?cf2&:(< ?cf2 0)))
  (test (neq ?f1 ?f2))
  =>
  (retract ?f2)
  (modify ?f1 (cf =(+ ?cf1 (* ?cf2 (+ 1 ?cf1)))))
)

;combine one POSITIVE and one NEGATIVE
;cf(cf1,cf2) = (cf1 + cf2) / 1 - MIN[abs(cf1),abs(cf2)]

(defrule neg-pos-cf
 (declare (salience -1))
  ?f1 <- (working_goal (goal ?g) (cf ?cf1))
  ?f2 <- (working_goal (goal ?g) (cf ?cf2))
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

(deftemplate MBTI 
	(slot MBTI1)
	(slot MBTI2)
	(slot MBTI3)
	(slot MBTI4)
)

;**** Rule 1: Assert fact MBTI1.
(defrule MBTI1
=>	(printout t crlf "Are you outwardly or inwardly focused? (I/E)")
	(bind ?response (read))
	(assert (MBTI (MBTI1 ?response))
	)
)
;**** Rule 2: Modify MBTI2 into same fact.
(defrule MBTI2
?MBTI2 <- (MBTI 
(MBTI1 ?MBTI1)
(MBTI2 nil)
)
=>
(printout t crlf "How do you prefer to take in information? (S/N)")
(bind ?response (read))
(modify ?MBTI2 (MBTI2 ?response))
)

;**** Rule 3: Modify MBTI3 into same fact.
(defrule MBTI3
?MBTI3 <- (MBTI 
(MBTI2 ?MBTI2)
(MBTI3 nil)
)
=>
(printout t crlf "How do you prefer to make decisions? (T/F)")
(bind ?response (read))
(modify ?MBTI3 (MBTI3 ?response))
)

;**** Rule 4: Modify MBTI4 into same fact.
(defrule MBTI4
?MBTI4 <- (MBTI 
(MBTI3 ?MBTI3)
(MBTI4 nil)
)
=>
(printout t crlf "How do you prefer to live your outer life? (J/P)")
(bind ?response (read))
(modify ?MBTI4 (MBTI4 ?response))
)

;**** Rule 5: INTJ.
(defrule INTJ
(MBTI (MBTI1 I))
(MBTI (MBTI2 N))
(MBTI (MBTI3 T))
(MBTI (MBTI4 J))
=>
(printout t crlf "Your MBTI is INTJ" crlf)
(assert (working_goal (goal OW) (cf 0.63)))
(assert (working_goal (goal FTS) (cf 0.13)))
(assert (working_goal (goal RTS) (cf 0.50)))
)

;**** Rule 6: Penalise Shooter if giddy.
(defrule giddy
=>
(printout t crlf "Do you feel giddy easily? (Y/N)")
(bind ?giddy (read))
	(if (eq ?giddy Y) then
	(assert (working_goal (goal FTS) (cf -0.7)))))

;**** Print out the final results
; this is not an elegant way to program - imagine if you have 30 current_goals!
; also note the output is not sorted by CF
(defrule compile_recommendations
	(declare (salience -10))
	(working_goal (goal OW) (cf ?cf-r))
	(working_goal (goal FTS) (cf ?cf-m))
	(working_goal (goal RTS) (cf ?cf-f))
=>	(assert (recomendation (OW ?cf-r) (FTS ?cf-m) (RTS ?cf-f)))
	(printout t crlf "Our recommendation is as follows :")
	(printout t crlf "Open World			: " 			(integer (* ?cf-r 100)) "%")
	(printout t crlf "Shooter   			: " 			(integer (* ?cf-m 100)) "%")
	(printout t crlf "Real Time Strategy   	: " 			(integer (* ?cf-f 100)) "%" crlf)
)