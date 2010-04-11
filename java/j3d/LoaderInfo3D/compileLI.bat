@echo off
echo Compiling LoaderInfo3D files...

javac -classpath %CLASSPATH%;ncsa\portfolio.jar *.java
echo Finished.