#!/bin/sh -x

javac -cp xmlrpc-common-3.1.jar:xmlrpc-client-3.1.jar XmlRpcClientTest.java
java -cp xmlrpc-common-3.1.jar:xmlrpc-client-3.1.jar:ws-commons-util-1.0.2.jar:. XmlRpcClientTest
