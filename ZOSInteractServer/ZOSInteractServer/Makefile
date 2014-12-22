###########################################################################
#                                                                         #
# Licensed Materials - Property of IBM                                    #
#                                                                         #
# 5955-A01                                                                #
#                                                                         #
# (C) Copyright IBM Corp. 2005, All Rights Reserved.                      #
#                                                                         #
# US Government Users Restricted Rights - Use, duplication or             #
# disclosure restricted by GSA ADP Schedule Contract with IBM Corp.       #
#                                                                         #
###########################################################################

# Makefile for example dbx plugin

CC      = c++
#CFLAGS  = -Wc,xplink $(LIBS)
#LDFLAGS = -Wl,xplink
CFLAGS  = -c -Wc,xplink $(LIBS)
LIBS    =

OBJS = RestServerEvents Service StringUtil RestClient Request Main Method Resource Response

all: $(OBJS) bind

Request: "./src/Request.C" "./src/Request.h"
	$(CC) $(CFLAGS) "./src/Request.C"

Main: "./src/Main.C"  
	$(CC) $(CFLAGS) "./src/Main.C"  

Method: "./src/Method.C" "./src/Method.h"
	$(CC) $(CFLAGS) "./src/Method.C" 

Resource:
	$(CC) $(CFLAGS)	"./src/Resource.C"

Response:
	$(CC) $(CFLAGS)	"./src/Response.C"

RestClient: "./src/RestClient.C"  "./src/RestClient.h"
	$(CC) $(CFLAGS)	"./src/RestClient.C" 

RestServerEvents:
	$(CC) $(CFLAGS)	"./src/RestServerEvents.C"

Service:
	$(CC) $(CFLAGS)	"./src/Service.C"

StringUtil:
	$(CC) $(CFLAGS)	"./src/StringUtil.C"
		
bind:
	$(CC) -o restexe -Wl,xplink RestClient.o Request.o Main.o Method.o Resource.o Response.o RestServerEvents.o Service.o StringUtil.o
#"./src/Request.C" "./src/Resource.C" ./src/Response.C ./src/RestClient.C ./src/RestServerEvents.C ./src/Service.C ./src/StringUtil.C
	
#-o restexe

clean:
	rm -f *.[ox] *.dbg *.dll
