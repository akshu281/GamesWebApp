package com.games;

import com.google.gson.Gson;

import net.sf.clipsrules.jni.Environment;
import net.sf.clipsrules.jni.FactAddressValue;
import net.sf.clipsrules.jni.LexemeValue;
import net.sf.clipsrules.jni.MultifieldValue;
import net.sf.clipsrules.jni.NumberValue;

import com.games.DefTemplate;

import com.games.Config;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Logger;

public class GamesServlet extends HttpServlet {
	
	public static Environment clips=null;
	
	private static final Logger LOGGER = Logger.getLogger(GamesServlet.class.getName());

	private static final Gson gson = new Gson();
	
	public GamesServlet() {
		
		clips=initialize(clips,Config.getServletContext());
		
	}
	public static Environment initialize(Environment clips, ServletContext servletContext) {
		// TODO Auto-generated method stub
		try {
			clips=new Environment();
			System.out.println("Opening Templates File in next step");
			String templates=loadResourceFile(servletContext, "templates.clp");
			clips.loadFromString(templates);
			clips.run();
			//clips.loadFromResource("D:\\GamesWebApp\\\\WebContent\\\\WEB-INF\\\\clips\\\\templates.clp");
			//clips.loadFromResource("D:\\GamesWebApp\\WebContent\\WEB-INF\\clips\\rules.clp");
			System.out.println("Opening Rules File in next step");
			String rules=loadResourceFile(servletContext, "rules.clp");
			clips.loadFromString(rules);
			clips.reset();
			clips.run();
			return clips;       
		}
		catch(Exception e) {
			e.printStackTrace();
			return null;
		}
		
	}
	
	public static String loadResourceFile(ServletContext context,String filename) {
		 System.out.println("loading from resource file");
		 try {
			InputStream s=context.getResourceAsStream("/WEB-INF/clips/"+filename);
			String clipsinfo="";
			int c=s.read();
			while(c!=-1) {
				clipsinfo+=(char)c;
				c=(int)s.read();
				
			}
			s.close();
			System.out.println(clipsinfo);
			return clipsinfo;
		 }
		 catch(Exception e) {
			 e.printStackTrace();
			 return null;
			 
		 }
	}
	
	
	
	@Override
	protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		//doGet(req, resp);
		//String [] URLSplits=req.getRequestURI().split("/");
		
        String action = req.getParameter("action");
        if (action == null) {
            return;
            }
            
        	else if (action.equals("personality"))    
            {
            	String [] inputsplits1=req.getParameter("inputfacts").split(",");
        		
        		int length=inputsplits1.length;
        		for(int i=0; i<length; i++)
        			System.out.println("Splits per index: "+inputsplits1[i]);
        		
        		String messagedata=req.getParameter("inputfacts");
        		System.out.println("Data Recd from JS:" + messagedata);
        		
        		String gender="(User (gender "+inputsplits1[1]+")"; //Included to form an assertstring format
        		String mbti1="(MBTI1 "+inputsplits1[2]+")";
        		String mbti2="(MBTI2 "+inputsplits1[3]+")";
        		String mbti3="(MBTI3 "+inputsplits1[4]+")";
        		String mbti4="(MBTI4 "+inputsplits1[5]+")";
        		//String mbti_Com="(MBTI_Com nil))";
        		//String game_type="(Certainty_Factor (game_type nil)";
        		//String cf="(cf nil))";
        		//String data_to_assert=gender+mbti1+mbti2+mbti3+mbti4+mbti_Com+game_type+cf;
        		String data_to_assert=gender+mbti1+mbti2+mbti3+mbti4+")";
        		//String datatoassert=gender+")";
        		
        		if(inputsplits1[0].equals("User"))			//To check if its of User template
        		{
        			//	if(addfacts(clips,req.getParameter("data")))	
        			System.out.println("Matched User template and about to call addfacts function for personality");
        			if(addfacts(clips,data_to_assert))
        				resp.getWriter().append("Success Post");
        			else {
        				resp.getWriter().append("Failure Post");
        				}
        		}  //End of Wiz 1
            }
        
        	else if (action.equals("gametype"))
            {
            	String [] inputsplits=req.getParameter("inputfacts1").split(",");
        		
        		int length=inputsplits.length;
        		for(int i=0; i<length; i++)
        			System.out.println("Splits per index: "+inputsplits[i]);
        		
        		String messagedata=req.getParameter("inputfacts1");
        		System.out.println("Data Recd from JS:" + messagedata);
        		
        		String actiondata="(User (Action "+inputsplits[1]+")"; //Included to form an assertstring format
        		String social="(Social "+inputsplits[2]+")";
        		String mastery="(Mastery "+inputsplits[3]+")";
        		String achievement="(Achievement "+inputsplits[4]+")";
        		String immersion="(Immersion "+inputsplits[5]+")";
        		String creativity="(Creativity "+inputsplits[6]+")";
        		String data_to_assert=actiondata+social+mastery+achievement+immersion+creativity+")";
        		
        		if(inputsplits[0].equals("User"))			//To check if its of User template
        		{
        			//	if(addfacts(clips,req.getParameter("data")))	
        			System.out.println("Matched User template and about to call addfacts function for games type");
        			if(addfacts(clips,data_to_assert))
        				resp.getWriter().append("Success Post");
        			else {
        				resp.getWriter().append("Failure Post");
        				}
        		}  //End of Wiz 2
            }     
				
	}

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		
		//String [] URLSplits=req.getRequestURI().split("/");
		//int length=URLSplits.length;
		//String template=URLSplits[length-1];
		System.out.println("Entering Get Block");
		String input=req.getParameter("action");
		String template="Certainty_Factor"; //Specifying the output field
		//if(input.equals("finish")) 
		//{
			ArrayList facts=getfacts(clips,template);
			System.out.println("Facts returned during GET: "+ facts.toString());
			resp.getWriter().append(facts.toString());
		
	/*  String action = req.getParameter("action");
		if (action == null) {
			return;
		}
	  switch (action) {
		case "start":
			getStartDetails(req, resp);
			break;

		default:
			break;
		}*/

		// resp.setContentType("application/json");
		// resp.setCharacterEncoding("UTF-8");
		// try (PrintWriter pw = resp.getWriter()) {
		// pw.write(jsonObject.toString());
		// }
	//}
	}
	
	public static boolean addfacts(Environment clips,String data) {
		try {
			System.out.println("Fact string to be asserted:" +data);
			clips.assertString(data);
			//System.out.println("Asserted String using addfacts: "+data);
			clips.run();
			return true;
		}
		catch(Exception e) {
			e.printStackTrace();
			return false;
		}
	}
	
	public static ArrayList getfacts(Environment clips,String template) {
		// TODO Auto-generated method stub
		try {
			/* got it yeah :P */			
			System.out.println("About to enter find all facts");
			//List<FactAddressValue> facts=clips.findAllFacts(template);
			
		//FactAddressValue pv=(FactAddressValue) clips.findAllFacts(template);			
		//System.out.println("Facts: " + facts);
		//	System.out.println("Fact Returned from GetFacts func:"+pv.toString());
			
			ArrayList responseArray=new ArrayList<>();
			
			MultifieldValue pv1 = (MultifieldValue) clips.eval("(find-games)");
		      
		      for (int i = 0; i < pv1.size(); i++) 
		        {
		        FactAddressValue fv = (FactAddressValue) pv1.get(i);
		        

		         float certainty = ((NumberValue) fv.getSlotValue("cf")).floatValue()*100; 
		         
		         String gametype = fv.getSlotValue("game_type").toString();
		         System.out.println("Game Type Returned from Clips:"+gametype+"and cf "+ certainty);
		         responseArray.add(gametype+certainty);
		        }
		      return responseArray;
		
		
	/*	for (FactAddressValue fact:facts) {
			String[] slots=DefTemplate.templateMap.get(template);
			HashMap<String, String> result=new HashMap<>();
			for(String slot:slots) {
				System.out.println("Fact Returned from GetFacts func: "+fact.getSlotValue(slot));
				result.put(slot,fact.getSlotValue(slot).toString());
			}
			responseArray.add(result);
		}
		System.out.println("Facts sizes:"+responseArray.size());
		return responseArray; */
		
		
		}
		
		catch(Exception e) {
			
		}
		
		return null;
		
	}
}
