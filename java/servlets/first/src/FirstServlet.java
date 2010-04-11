import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;

public class FirstServlet extends HttpServlet {
  public void doGet(HttpServletRequest request,
                    HttpServletResponse response)
                    throws IOException {
    PrintWriter out = response.getWriter();
    java.util.Date today = new java.util.Date();

    out.println("<html>" +
                "<body>" +
                  "<h1 align=center>Today is " + today +
                  "</h1>" +
                "</body>" +
                "</html>");
  }
}
