package clipscode;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.servlet.ServletContext;


import net.sf.clipsrules.jni.Environment;
import net.sf.clipsrules.jni.FactAddressValue;
import servlets.DefTemplate;

public class ClipsManager {

	public static Environment initialize(Environment clips, ServletContext servletContext) {
		// TODO Auto-generated method stub
		try {
			clips=new Environment();
			//loading template
			String template=loadResourceFile(servletContext, "template.clp");
			clips.loadFromString(template);
			clips.run();
			//loading rules
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
		 System.out.println("loading from resource");
		 try {
			InputStream s=context.getResourceAsStream("/WEB-INF/ClipsSources/"+filename);
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
	
	public static boolean addFact(Environment clips,String data) {
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
	
	public static ArrayList getAllFacts(Environment clips,String template) {
		// TODO Auto-generated method stub
		try {
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
