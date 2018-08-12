package servlets;

import java.io.IOException;
import java.util.ArrayList;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import clipscode.ClipsManager;
import net.sf.clipsrules.jni.Environment;
@WebServlet("/ClipsServer/*")
public class ClipsServer extends HttpServlet {

/* static variables */
	public static Environment clips=null;
	
	
public ClipsServer() {
	// TODO Auto-generated constructor stub
	clips=ClipsManager.initialize(clips,Config.getServletContext());
	
}
@Override
protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
	// TODO Auto-generated method stub
	String [] URLSplits=req.getRequestURI().split("/");
	int length=URLSplits.length;
	String template=URLSplits[length-1];
	if(URLSplits[length-2].equals("getfacts")) {
		ArrayList facts=ClipsManager.getAllFacts(clips,template);
		resp.getWriter().append(facts.toString());
	}
	
	
}

@Override
protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
	// TODO Auto-generated method stub
	String [] URLSplits=req.getRequestURI().split("/");
	int length=URLSplits.length;
	
	
	
		if(ClipsManager.addFact(clips,req.getParameter("data")))
			resp.getWriter().append("Success");
		else {
			resp.getWriter().append("Failure");
	}
}
}
