@echo off
echo Executing Java3D application...

java -cp %CLASSPATH%;ncsa\portfolio.jar LoaderInfo3D  %1 %2
echo Finished.