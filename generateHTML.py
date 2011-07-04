#!/usr/bin/env python

import sys

#This program can be run with two arguments via command line:
#Example : $ python generateHTML.py TESTCASES mine.html
#If not given the arguments asked through prompt
#Testcases File contains n lines of the following format
#filename width height borderSizesx4 borderRadiix8  	
#Eg : border 800 600 50,50,50,50 100,100,100,100,100,100,100,100
#filename is ignored by this program but must not be null

def bodyBlock(info):
	infoT = tuple([int(m) for m in info])
	block = """
	<div style="width:%dpx;height:%dpx;border-style:dashed;border-width:%dpx %dpx %dpx %dpx;border-top-left-radius: %dpx %dpx;border-top-right-radius: %dpx %dpx;border-bottom-right-radius: %dpx %dpx;border-bottom-left-radius: %dpx %dpx;">
		&nbsp;
	</div>""" % infoT
	return block

if __name__=='__main__':
	try:
		testcaseFile = sys.argv[1]
	except:
		print "Testcases File"
		testcaseFile = raw_input()
	f = open(testcaseFile,'r')
	try: 
		outputFile = sys.argv[2]
	except:
		print "Output File"
		outputFile = raw_input()
	head = """
<head>
	<title>Test Case Visualization</title>
	<style>
		div{
			display:block;
			margin:10px;
		}
	</style>
</head>\n"""
	body = "<body>"
	count = 0	
	for line in f.readlines():
		if '#' in line:
			continue
		line = line.strip('\r\n').replace(',',' ')
		info = line.split()[1:]
		body += bodyBlock(info)
		count += 1
	body += "\n</body>"
	f = open(outputFile,'w')
	f.write("<!DOCTYPE HTML>\n<html>" + head + body + "\n</html>")	
