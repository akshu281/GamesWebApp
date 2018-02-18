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
			File f=new File("New.txt");
			System.out.println("File created :: " + f.createNewFile());
			System.out.println("File created :: " + f.getCanonicalPath());
			clips=new Environment();
			clips.loadFromResource("D:\\GamesWebApp\\WebContent\\WEB-INF\\clips\\rules.clp");
			clips.run();
			clips.loadFromResource("D:\\GamesWebApp\\WebContent\\WEB-INF\\clips\\templates.clp");
			clips.reset();
			clips.run();
			return clips;
	       
		}
		catch(Exception e) {
			e.printStackTrace();
			return null;
		}
		
	}
	@Override
	protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		//doGet(req, resp);
		String [] URLSplits=req.getRequestURI().split("/");
		int length=URLSplits.length;
		if(addfacts(clips,req.getParameter("data")))
				resp.getWriter().append("Success Post");
			else {
				resp.getWriter().append("Failure Post");
		}
	}

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		
		String [] URLSplits=req.getRequestURI().split("/");
		int length=URLSplits.length;
		String template=URLSplits[length-1];
		if(URLSplits[length-2].equals("getfacts")) {
			ArrayList facts=getfacts(clips,template);
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
	}
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
			List<FactAddressValue> facts=clips.findAllFacts(template);
		
		ArrayList<HashMap> responseArray=new ArrayList<>();
		
		for (FactAddressValue fact:facts) {
			String[] slots=DefTemplate.templateMap.get(template);
			HashMap<String, String> result=new HashMap<>();
			for(String slot :slots) {
				result.put(slot,fact.getSlotValue(slot).toString());
			}
			responseArray.add(result);
		}
		System.out.println("found"+responseArray.size());
		return responseArray;
		
		}
		catch(Exception e) {
			
		}
		return null;
		
	}
}
