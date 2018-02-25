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
	String datatoassert;
	
	public GamesServlet() {
		
		clips=initialize(clips,Config.getServletContext());
		
	}
	public static Environment initialize(Environment clips, ServletContext servletContext) {
		// TODO Auto-generated method stub
		try {
			clips=new Environment();
			System.out.println("Opening Templates File in next step");
			System.out.println("Opening Rules File in next step");
			String rules=loadResourceFile(servletContext, "rule.clp");
			clips.reset();
			clips.loadFromString(rules);
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
			InputStream is=context.getResourceAsStream("/WEB-INF/clips/"+filename);
			String clipsinfo="";
			int c=is.read();
			while(c!=-1) {
				clipsinfo+=(char)c;
				c=(int)is.read();
				
			}
			is.close();
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
        		String mbti_Com="(MBTI_Com "+inputsplits1[2]+inputsplits1[3]+inputsplits1[4]+inputsplits1[5]+")";
        		datatoassert=gender+mbti1+mbti2+mbti3+mbti4+mbti_Com;
            }

        
        	else if (action.equals("gametype"))
            {
            	String [] inputsplits1=req.getParameter("inputfacts1").split(",");
        		
        		int length=inputsplits1.length;
        		for(int i=0; i<length; i++)
        			System.out.println("Splits per index: "+inputsplits1[i]);
        		
        		String messagedata=req.getParameter("inputfacts1");
        		System.out.println("Data Recd from JS:" + messagedata);
        		
        		
        		String actiondata1="(Action "+inputsplits1[1]+")";
        		String social="(Social "+inputsplits1[2]+")";
        		String mastery="(Mastery "+inputsplits1[3]+")";
        		String achievement="(Achievement "+inputsplits1[4]+")";
        		String immersion="(Immersion "+inputsplits1[5]+")";
        		String creativity="(Creativity "+inputsplits1[6]+")";
        		//String data_to_assert=actiondata+social+mastery+achievement+immersion+creativity+")";
        		String data_to_assert=datatoassert+actiondata1+social+mastery+achievement+immersion+creativity+")";
        		
        		if(inputsplits1[0].equals("User"))			//To check if its of User template
        		{
        			//	if(addfacts(clips,req.getParameter("data")))	
        			System.out.println("Matched User template and about to call addfacts function for games type");
        			if(addfacts(clips,data_to_assert))
        			{	System.out.println("Assert Success");
        				resp.getWriter().append("Success Post");}
        			else {
        				resp.getWriter().append("Failure Post");
        				System.out.println("Assert Fail");
        				}
        		}  //End of Wiz 2
            }     
				
	}

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		System.out.println("Entering Get Block");
		String input=req.getParameter("action");
		//String template="Certainty_Factor"; //Specifying the output field
		//if(input.equals("finish")) 
		//{
			//ArrayList facts=getfacts(clips,template);
			//ArrayList facts=getfacts(clips);
			//System.out.println("Facts returned during GET: "+ facts.toString());
			//resp.getWriter().append(facts.toString());
			String facts=getfacts(clips);
			System.out.println("Facts returned during GET: "+ facts);
			resp.getWriter().append(facts);
	}
	
	public static boolean addfacts(Environment clips,String data) {
		try {
			System.out.println("Fact string to be asserted:" +data);
			clips.reset();
			clips.assertString(data);
			clips.run();
			return true;
		}
		catch(Exception e) {
			e.printStackTrace();
			return false;
		}
	}
	
	//public static ArrayList getfacts(Environment clips,String template) {
	//	public static ArrayList getfacts(Environment clips) {
			public static String getfacts(Environment clips) {
		// TODO Auto-generated method stub
		try {
			/* got it yeah :P */			
			System.out.println("About to enter find all facts");
			ArrayList responseArray=new ArrayList<>();
			String fresh="";
			MultifieldValue pv1 = (MultifieldValue) clips.eval("(find-games-list)");
			//List<FactAddressValue> pv1=clips.findAllFacts("(find-games)");
			
			System.out.println(pv1);
		      
		      for (int i = 0; i < pv1.size(); i++) 
		        {
		        FactAddressValue fv = (FactAddressValue) pv1.get(i);
		         //float certainty = ((NumberValue) fv.getSlotValue("cf")).floatValue()*100; 
		        // String gametype = fv.getSlotValue("game_type").toString();
		         //System.out.println("Game Type Returned from Clips:"+gametype+" and cf "+ certainty);
		         //responseArray.add(gametype +" - "+certainty);
		        String games_result= fv.getSlotValue("Game").toString();
		        System.out.println("Games Returned from Clips:"+games_result);
		        
		        		fresh= fresh +"\n"+games_result;
		       // System.out.println(games_result.substring(1, games_result.length() - 1));
		        responseArray.add(games_result+"\n");
		        }
		   //   return responseArray;
			return fresh;	
		}
		catch(Exception e) {	
		}
		return null;	
	}
}
