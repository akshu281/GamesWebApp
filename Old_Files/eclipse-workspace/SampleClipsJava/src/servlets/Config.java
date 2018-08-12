package servlets;

import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.annotation.WebListener;

@WebListener
public class Config implements ServletContextListener {
	public static ServletContext servletContext=null;
    @Override
    public void contextInitialized(ServletContextEvent event) {
//        ServletContext servletContext = event.getServletContext(); 
        System.out.println("inside config servlet");
        servletContext=event.getServletContext();
              
    }

    @Override
    public void contextDestroyed(ServletContextEvent event) {
        // NOOP.
    }
    public static ServletContext getServletContext() {
    	return servletContext;
    }

}