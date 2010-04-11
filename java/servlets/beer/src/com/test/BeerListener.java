/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.test;

import javax.servlet.*;

/**
 * Web application lifecycle listener.
 * @author mmuthanna
 */

public class BeerListener implements ServletContextListener {

    public void contextInitialized(ServletContextEvent arg0) {
        ServletContext sc = arg0.getServletContext();
        sc.setAttribute("jazz", "Renee Olstead");       
    }

    public void contextDestroyed(ServletContextEvent arg0) {
        throw new UnsupportedOperationException("Not supported yet.");
    }
}