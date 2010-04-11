
Chapter 8. A 3D Checkboard

From:
  Killer Game Programming in Java
  Andrew Davison
  O'Reilly, May 2005
  ISBN: 0-596-00730-2
  http://www.oreilly.com/catalog/killergame/
  Web Site for the book: http://fivedots.coe.psu.ac.th/~ad/jg

Contact Address:
  Dr. Andrew Davison
  Dept. of Computer Engineering
  Prince of Songkla University
  Hat yai, Songkhla 90112, Thailand
  E-mail: ad@fivedots.coe.psu.ac.th


If you use this code, please mention my name, and include a link
to the book's Web site.

Thanks,
  Andrew

---------------------------------
Compilation: 

$ javac *.java
    // you must have Java 3D installed for the compilation to succeed;
    // if you get "Warning" messages, please see the note below


Java 3D is available from http://java.sun.com/products/java-media/3D/

---------------------------------
Execution:

$ java Checkers3D

Move about by dragging the mouse. Hold the ALT key and drag to
move in and out.

--------------------
Displaying the scene graph

The calls to Java3dTree have already been added to WrapCheckers3D.java
in WrapCheckers3D() and createSceneGraph(). Uncomment them.

Compilation and execution are done with batch files:

$ compileChk

$ Checkers3D

These batch files assume that j3dtree.jar is in the **parent** 
directory above the application directory.

Another frame opens which shows the scene graph.

j3dtree.jar appears in Daniel Selman's "Jave 3D Programming" 
excellent textbook. It can be downloaded from my site 
(http://fivedots.coe.psu.ac.th/~ad/jg/j3dtree.jar) or as part 
of Selman's source code downloadable from 
http://www.manning.com/selman/

-----------
Note on "unchecked or unsafe operation" Warnings

As explained in chapter 3, I have not used J2SE 5.0's type-safe 
collections, so that this code will compile in early versions of
J2SE (e.g. version 1.4).

The warning messages are always related to my use of collections
(e.g. ArrayList) without specifying a type for the objects they will
contain at run time.

No. of Warnings generated in J2SE 5.0 for the examples:
/Checkers3D: 9

---------------------
Last updated: 16th April 2005
