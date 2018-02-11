package com.games;

import com.google.gson.Gson;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.logging.Logger;

public class GamesServlet extends HttpServlet {
	private static final Logger LOGGER = Logger.getLogger(GamesServlet.class.getName());

	private static final Gson gson = new Gson();

	@Override
	protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		doGet(req, resp);
	}

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		String action = req.getParameter("action");
		if (action == null) {
			return;
		}
		switch (action) {
		case "start":
			getStartDetails(req, resp);
			break;

		default:
			break;
		}

		// resp.setContentType("application/json");
		// resp.setCharacterEncoding("UTF-8");
		// try (PrintWriter pw = resp.getWriter()) {
		// pw.write(jsonObject.toString());
		// }
	}

	private void getStartDetails(HttpServletRequest req, HttpServletResponse resp) throws IOException {
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
}
