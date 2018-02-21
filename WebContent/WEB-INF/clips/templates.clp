;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Templates for certainty factor to work
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftemplate User 
	(slot gender)
	(slot MBTI1)
	(slot MBTI2)
	(slot MBTI3)
	(slot MBTI4)
	(slot MBTI_Com)
	(slot Action)
	(slot Social)
	(slot Mastery)
	(slot Achievement)
	(slot Immersion)
	(slot Creativity)
)

;;(deftemplate Recommended_Game_Type
	;;(slot Recommended_Game_Type)
	;;(slot game_type))
	
(deftemplate Certainty_Factor 
	(slot game_type) 
	(slot cf))	
	
;;(deftemplate Recommended_Game
	;;(slot Recommended_Game)
	;;(slot game))