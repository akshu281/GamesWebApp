package com.games;

import java.util.ArrayList;
import java.util.HashMap;

public class DefTemplate {
public static HashMap<String,String []> templateMap=new HashMap<>();
static {
	templateMap.put("person", new String[]{"name","age","occupation"});
}
}
