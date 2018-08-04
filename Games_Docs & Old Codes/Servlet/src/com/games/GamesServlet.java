package com.games;

import com.google.gson.Gson;
import com.google.gson.JsonObject;

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

//        resp.setContentType("application/json");
//        resp.setCharacterEncoding("UTF-8");
//        try (PrintWriter pw = resp.getWriter()) {
//            pw.write(jsonObject.toString());
//        }
    }

    private void getStartDetails(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        String uname = req.getParameter("name");
        int age = Integer.parseInt(req.getParameter("age"));
        String email = req.getParameter("email");

        LOGGER.info("Name : " + uname);
        LOGGER.info("age : " + age);
        LOGGER.info("age : " + email);

        try (PrintWriter pw = resp.getWriter()) {
            pw.write("Hai Akshaya");
        }
    }
}
