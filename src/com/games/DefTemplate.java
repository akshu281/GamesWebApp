package com.games;

import java.util.ArrayList;
import java.util.HashMap;

public class DefTemplate {
public static HashMap<String,String []> templateMap=new HashMap<>();
static {
	templateMap.put("Certanty_Factor", new String[]{"game_type","cf"});
	templateMap.put("User", new String[]{"gender","MBTI1","MBTI2","MBTI3","MBTI4","MBTI_Com","Action","Social","Mastery","Achievement","Immersion","Creativity"});
}
}
