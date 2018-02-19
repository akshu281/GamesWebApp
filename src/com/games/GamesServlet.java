package com.games;

import com.google.gson.Gson;

import net.sf.clipsrules.jni.Environment;
import net.sf.clipsrules.jni.FactAddressValue;
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
			clips.reset();
			clips.run();
			//clips.loadFromResource("D:\\GamesWebApp\\\\WebContent\\\\WEB-INF\\\\clips\\\\templates.clp");
			
			//clips.loadFromResource("D:\\GamesWebApp\\WebContent\\WEB-INF\\clips\\rules.clp");
			System.out.println("Opening Rules File in next step");
			String rules=loadResourceFile(servletContext, "rules.clp");
			clips.loadFromString(rules);
			//clips.reset();
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
		
		String [] inputsplits=req.getParameter("inputfacts").split(",");
		
		int length=inputsplits.length;
		for(int i=0; i<length; i++)
			System.out.println("Splits per index: "+inputsplits[i]);
		
		String messagedata=req.getParameter("inputfacts");
		System.out.println("Data Recd from JS:" + messagedata);
		
		String gender="(User (gender "+inputsplits[1]+")"; //Included to form an assertstring format
		String mbti1=" (MBTI1 "+inputsplits[2]+")";
		String mbti2=" (MBTI2 "+inputsplits[3]+")";
		String mbti3="(MBTI3 "+inputsplits[4]+")";
		String mbti4="(MBTI4 "+inputsplits[5]+")";
		String mbti_com="nil";
		String game_type="nil";
		String cf="nil";
		// probelm is here too. the fact must look like (User (gender m)(nextslot value)(nest value))
		//for now we have a rule which fires for gender only right ? 
		//so other slots we shouldnt insert. 
		// i will remove them and test. ?? 
		// okay in that case 
		//String datatoassert=gender+" "+mbti1+" "+mbti2+" "+mbti3+" "+mbti4+" "+mbti_com+" "+game_type+" "+cf+" "+")";
		String datatoassert=gender+")";
		
		if(inputsplits[0].equals("User"))			//To check if its of User template
		{
		
			//	if(addfacts(clips,req.getParameter("data")))
		
			System.out.println("Matched User template and about to call addfacts function");
			if(addfacts(clips,datatoassert))
				resp.getWriter().append("Success Post");
			else {
				resp.getWriter().append("Failure Post");
				}
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
	
	/*private void getStartDetails(HttpServletRequest req, HttpServletResponse resp) throws IOException {
		String I_E = req.getParameter("optradio");
		String S_N = req.getParameter("optradio1");
		String T_F = req.getParameter("optradio2");
		String J_P = req.getParameter("optradio3");
		//System.out.println(I_E);

		LOGGER.info("Int or Ext : " + I_E);
		LOGGER.info("Sen or Intui : " + S_N);
		LOGGER.info("Thi or Fee : " + T_F);
		LOGGER.info("Jud or Per : " + J_P);

		try (PrintWriter pw = resp.getWriter()) {
			pw.write("Hitman is your game!");
			System.out.println("Hitman is your game!");
		}
	}
	*/
	public static boolean addfacts(Environment clips,String data) {
		try {
			clips.assertString(data);
			System.out.println("Asserted String using addfacts: "+data);
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
			List<FactAddressValue> facts=clips.findAllFacts(template);
			System.out.println("Facts: " + facts);
		
		ArrayList<HashMap> responseArray=new ArrayList<>();
		
		for (FactAddressValue fact:facts) {
			String[] slots=DefTemplate.templateMap.get(template);
			HashMap<String, String> result=new HashMap<>();
			for(String slot:slots) {
				System.out.println("Fact Returned from GetFacts func: "+fact.getSlotValue(slot));
				result.put(slot,fact.getSlotValue(slot).toString());
			}
			responseArray.add(result);
		}
		System.out.println("Facts sizes:"+responseArray.size());
		return responseArray;
		
		}
		catch(Exception e) {
			
		}
		return null;
		
	}
}
