package com.test;

import com.model.*;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;

public class BeerSelect extends HttpServlet {

  @Override
  public void doPost(HttpServletRequest request,
                     HttpServletResponse response) 
                     throws IOException {
    response.setContentType("text/html");
    PrintWriter out = response.getWriter();

    out.println("Beer Selection Advice<p/>");
    
    String c = request.getParameter("color");

    out.println("Got color: " + c + "<p/>");

    for (String brand : new BeerExpert().getBrands(c)) {
      out.println("Brand: <b>" + brand + "</b><br/>");
    }
    
    out.println("Initialized jazz = " + (String) getServletContext().getAttribute("jazz"));
    
    out.println("<p/>Connecting to Asterisk server: " + 
      getServletContext().getInitParameter("server"));
  }
}