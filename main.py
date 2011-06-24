#!/usr/bin/python
#GSoC Logic Implementation
from math import *
import cairo 
import sys

#Corner Indices
TL=0
TR=1
BR=2
BL=3

#Side indices
T=0
R=1
B=2
L=3

#The precision of angle measurement[in radians]
delta = 1.0 * pi/180

def main(filename='border'):
	initInnerRadii()
	
	#Set the color for drawing : editable
	ctx.set_source_rgb (1.0, 0.0, 0.0)
	
	#Draw each side
	drawDashedSide(T)
	ctx.set_source_rgb (0.0, 1.0, 0.0)
	drawDashedSide(R)
	ctx.set_source_rgb (1.0, 1.0, 0.0)
	drawDashedSide(L)
	ctx.set_source_rgb (0.0, 0.0, 1.0)
	drawDashedSide(B)
	
	#Writing out the surface to png file
	surface.write_to_png (filename + '.png') 

def initInnerRadii():
	""" Initializes the innerRadii for all corners """ 
	for i in range(0,4):
		if(i%2==0):	
			innerRadii[i][0] = max(borderRadii[i][0]-borderSizes[(i-1)%4],0)
			innerRadii[i][1] = max(borderRadii[i][1]-borderSizes[i],0)oCurveDims[0]
		else:
			innerRadii[i][0] = max(borderRadii[i][0]-borderSizes[i],0)
			innerRadii[i][1] = max(borderRadii[i][1]-borderSizes[(i-1)%4],0)
			
def drawDashedSide(sideIndex):
	""" Draws the side with given sideIndex	"""
	
	global dashLength
	
	#Get dashLength,gapLength and offset 
	raw = calculateDashes(sideIndex)
	gapLength = raw[0]
	offset = (raw[1]+dashLength)/borderLengths[sideIndex]
	
	dashes = [dashLength/borderLengths[sideIndex],gapLength/borderLengths[sideIndex]]
	
	#Side is drawn in 3 parts
	#Visual shown for top side: 
	#Corner section left of side drawn clockwise till 1, line from 1 to 2
	#Corner section right of side drawn anticlockwise till 2 
	# 	   1____________________2
	#	  /                     \
	#	 /                       \
	
	#Draw straight section 
	drawStraightSection(sideIndex,dashes,offset)

	#Draw left section
	drawCorner(sideIndex,dashLength,gapLength,-1)
	
	#Draw right section 
	drawCorner((sideIndex+1)%4,dashLength,gapLength,1)			

def calculateDashes(sideIndex):
	"""For a particular side computes the [gap width,offsetRaw] for that side """
	
	# The length of dash 
	global dashLength 
	
	# Straight Path Length
	straightLength = borderLengths[sideIndex] - borderRadii[sideIndex][sideIndex%2] - borderRadii[(sideIndex+1)%4][sideIndex%2]
	
	# Length of curved section : 1 -> Left of side
	curvedLength1 = ComputeCurvedLength(sideIndex,sideIndex)
	
	# Length of curved section : 2 -> Right of side
	curvedLength2 = ComputeCurvedLength(sideIndex,(sideIndex+1)%4)

	totalLength = straightLength + curvedLength1 + curvedLength2
	
	# The totalLength = n * (dash length + gap length)
	# The constrain on 0.5*dashLength <= gapLength <= dashLength : for a range the lowest value chosen
	n = floor(totalLength/(1.5*dashLength))
	
	if n>0:	gapLength = totalLength/n - dashLength 
	else:dashLength
	
	# Calculate offset for dash pattern
	# This is for the offset parameter in ctx.set_dash 
	
	# Check if curvedLength1 is sufficient for half a dash 
	if curvedLength1 < dashLength/2:
		offset= curvedLength1
	else:
		offset = curvedLength1 - dashLength/2 - floor((curvedLength1-dashLength/2)/(dashLength+gapLength))*(dashLength+gapLength)
	
	return [gapLength,offset]
	
def ComputeCurvedLength(sideIndex,cornerIndex):
	"""Computes the length of side to be taken for gap width calculation"""
	
	# A side is considered to be made of 3 sections
	# one straight section and two curved sections on either borderLengths 
	# of the straight section 
	# The curved section is a part of the corner adjacent to side 
	# demarcated by the angle proportional to the border widths
	
	# A sample corner 
	#	   .________<-side under consideration
	#	  /|
	#	 / |a
	#	/__|
	#	|b
	# All calculations are made along the 'central' ellipse
	# This is an approximation : its semi major axis are the average of the outer and inner
	# a is radius of ellipse [central ellipse] adjacent to side under consideration 
	a = (borderRadii[cornerIndex][1-sideIndex%2]+innerRadii[cornerIndex][1-sideIndex%2])/2
	
	# b is the other radius 
	b = (borderRadii[cornerIndex][sideIndex%2]+innerRadii[cornerIndex][sideIndex%2])/2
	
	combinedSize = borderSizes[sideIndex]+borderSizes[(sideIndex-1)%4]
	
	# t is the demarcating angle 
	t = borderSizes[sideIndex]/combinedSize * (pi/2) if sideIndex==cornerIndex 
	else borderSizes[sideIndex]/combinedSize * (pi/2)
	
	#TODO Improve Explaination
	# Calculation ::
	# There exist 2 cases : When major axis along x axis or along y axis
	# In general if R,r are length of semi major axis and sLength of curved section : 1 -> Left of sideemi minor axis
	# Ray at angle t intersects ellipse at a point which satisties y = x*tant
	# Also point x,y can be written as RcosT,rsinT where T is the paramteric angle	
	# T is parametric angle of point which seperates the sections of curve
	if a>=b:
		T = atan((a/b)*tan(t))	
		k = 1-(b/a)**2
		return a*EllipseE(k,T,0.0)
	else:
		T = atan(b/(a*tan(t)))
		k = 1-(a/b)**2
		return b*EllipseE(k,pi/2,T)	

def EllipseE(k,ph2,ph1):
	"""
	Ellipse(k,ph1,ph2) : Computes the elliptic integral second kind 
	Integral from ph1 to ph2 of sqrt (1-k*sinSquare(phi))
	k is m**2 where m is eccentricity
	Uses Simpson's 3/8 rule : gives result correctly approxiamted to one decimal place 
	Furthur Reading :http://en.wikipedia.org/wiki/Simpson's_rule
	"""
	
	def func(x,t):
		return sqrt(1-x*sin(t)*sin(t))
	return abs(ph2-ph1)/8*(func(k,ph1)+3*func(k,(2*ph1+ph2)/3)+3*func(k,(ph1+2*ph2)/3)+func(k,ph2))

def drawStraightSection(sideIndex,dashes,offset):	
	""" Draws the straight section of given side """
	
	#Getting start and end points 
	if(sideIndex == T):
		startX,startY,endX,endY = 0.0+borderRadii[TL][0],0.0+borderSizes[sideIndex]/2,
									 width-borderRadii[TR][0],0.0+borderSizes[sideIndex]/2
	elif(sideIndex == R):
		startX,startY,endX,endY = width-(borderSizes[sideIndex]/2),0.0+borderRadii[TR][1],
									 width-(borderSizes[sideIndex]/2),height-borderRadii[BR][1]
	elif(sideIndex == B):
		startX,startY,endX,endY = width-borderRadii[BR][0],height-(borderSizes[sideIndex]/2),
									 0.0+borderRadii[BL][0],height-(borderSizes[sideIndex]/2)
	elif(sideIndex == L):
		startX,startY,endX,endY = 0.0+(borderSizes[sideIndex]/2),height-borderRadii[BL][1],
									 0.0+(borderSizes[sideIndex]/2),0.0+borderRadii[TL][1]
	else:
		print "Some error crept in DRAW SIDE : INDEX"
	
	#Reducing to range 0,1 for cairo	
	startX,startY,endX,endY = startX/width,startY/height,endX/width,endY/height
	
	#Draw!
	ctx.save()
	ctx.move_to(startX,startY)
	ctx.line_to(endX,endY)
	ctx.set_line_width (borderSizes[sideIndex]/borderLengths[(sideIndex+1)%4])
	ctx.set_dash(dashes,offset)
	ctx.stroke()
	ctx.restore() # To remove dash pattern

def absToParam(ta,a,b):
	"""To convert absolute to parametric angle"""
	return atan(a*tan(ta)/b)

def paramToAbs(tp,a,b):
	"""To convert parametric to absolute angle"""
	return atan(b*tan(tp)/a)

def drawCorner(corner,dash,gap,dir):
	"""Draws section of corner in counter clockwise sense """
	
	print "corner",corner ,"  ", dir
	
	#dir = -1 => CW
	#dir = 1 => CCW (this function)
	
	if corner == TL:   cornerOffsetX,cornerOffsetY = borderRadii[0][0], borderRadii[0][1]
	elif corner == TR: cornerOffsetX,cornerOffsetY = width - borderRadii[1][0], borderRadii[1][1]
	elif corner == BR: cornerOffsetX,cornerOffsetY = width-borderRadii[2][0], height-borderRadii[2][1]
	elif corner == BL: cornerOffsetX,cornerOffsetY = borderRadii[3][0], height - borderRadii[3][1]
	
	# Save the state of canvas
	ctx.save()
	ctx.translate(cornerOffsetX/width,cornerOffsetY/height) # Move to corner center
	ctx.transform(cairo.Matrix(1,0,0,-1,0,0)) # Flip Y Axis
	
	# We move along the outer curve with dimensions oCurveDims
	# oCurveDims[0] is radii along X Axis and oCurveDims[1] is radii along Y Axis
	# Similary iCurveDims is for the innerCurve and curveDims for central curve
	oCurveDims = borderRadii[corner]
	iCurveDims = innerRadii[corner]
	curveDims = [(oCurveDims[0]+iCurveDims[0])/2,
				 (oCurveDims[1]+iCurveDims[1])/2]
	
	# start represents angle in  [0,pi/2]
	start = (borderSizes[corner]/(borderSizes[(corner-1)%4]+borderSizes[corner]))*pi/2
	
	# End Angle [0,2*pi] 
	# (2-corner)%4*pi/2 is when drawing anticlockwise
	# while drawing clockwise it is (2-corner)%4*pi/2 - pi/2
	# calcAngle signifies nothing - it is just used for convenience used to reduce repeated 
	# calculation of endAngle - pi/2 
	endAngle = (2-corner)%4 * pi/2
	if endAngle == 0:
		endAngle = 2*pi
	
	if dir==1: 
		calcAngle = endAngle - pi/2
	else: 
		endAngle -= pi/2
		calcAngle = endAngle
		
	#Start angle [0,2*pi]
	startAngle = calcAngle + absToParam(start,curveDims[(corner+1)%2],curveDims[corner%2])
	
	previous = current = startAngle

	currentO = calcAngle + absToParam(start,oCurveDims[(corner+1)%2],oCurveDims[corner%2])
	previousO = currentO

	currentI = calcAngle + absToParam(start,iCurveDims[(corner+1)%2],iCurveDims[corner%2]) 
	previousI = currentI
	
	# R is semi major axis , r is semi minor axis
	# k is square of eccentricity
	R,r = curveDims if curveDims[0]>curveDims[1] else curveDims[1],curveDims[0]
	k = 1- r**2/R**2
	
	# curlen keeps track of current length covered
	curlen = 0.0
	
	# maintain a flag to exit when we reached endAngle
	# exit after drawing
	flag = 0
	
	# While moving clockwise current decreases whereas while moving ccw 
	# current increases . 
	# The dir provided is used to choose increment/decrement
	while curlen<dash/2:
		current += dir*delta
		curlen += R*EllipseE(k,current,current-dir*delta)
	
	# We wish to check if current > endAngle while moving CCW 
	# and current < endAngle while moving CW
	# Dir == -1 => Flipping inequality  
	if dir*current>dir*endAngle:
		current=endAngle
		flag=1
			
	currentO = absToParam(paramToAbs(current - calcAngle,curveDims[(corner+1)%2],curveDims[corner%2]),
									oCurveDims[(corner+1)%2],oCurveDims[corner%2]) + calcAngle
	currentI = absToParam(paramToAbs(current - calcAngle,curveDims[(corner+1)%2],curveDims[corner%2]),
									iCurveDims[(corner+1)%2],iCurveDims[corner%2]) + calcAngle

	stepO = (currentO - previousO)/30
	stepI = (currentI - previousI)/30
	
	ctx.move_to(oCurveDims[0]*cos(previousO)/width,oCurveDims[1]*sin(previousO)/height)

	for i in range(1,31):
		ctx.line_to(oCurveDims[0]*cos(previousO+stepO*i)/width,oCurveDims[1]*sin(previousO+stepO*i)/height)
	
	ctx.line_to(iCurveDims[0]*cos(currentI)/width,iCurveDims[1]*sin(currentI)/height)
	
	for i in range(1,31):
		ctx.line_to(iCurveDims[0]*cos(currentI-stepI*i)/width,iCurveDims[1]*sin(currentI-stepI*i)/height)
	
	ctx.close_path()
	ctx.fill()
	
	previous = current
	curlen=0
	
	if flag:
		return
	
	while True:
		while curlen<gap:
			current += dir*delta
			curlen += R*EllipseE(k,current,current-dir*delta)
		
		previous = current
		currentO = absToParam(paramToAbs(current - calcAngle,curveDims[(corner+1)%2],curveDims[corner%2]),
										oCurveDims[(corner+1)%2],oCurveDims[corner%2]) + calcAngle
		currentI = absToParam(paramToAbs(current - calcAngle,curveDims[(corner+1)%2],curveDims[corner%2]),
										iCurveDims[(corner+1)%2],iCurveDims[corner%2]) + calcAngle
		previousO = currentO
		previousI = currentI
		curlen=0
		
		if dir*current>dir*endAngle:
			break	
		while curlen<dash:
			current += dir*delta
			curlen += R*EllipseE(k,current,current-dir*delta)
		
		if dir*current>dir*endAngle:
			current=endAngle
			flag=1
				
		currentO = absToParam(paramToAbs(current - calcAngle,curveDims[(corner+1)%2],curveDims[corner%2]),
										oCurveDims[(corner+1)%2],oCurveDims[corner%2]) + calcAngle
		currentI = absToParam(paramToAbs(current - calcAngle,curveDims[(corner+1)%2],curveDims[corner%2]),
										iCurveDims[(corner+1)%2],iCurveDims[corner%2]) + calcAngle
	
		stepO = (currentO - previousO)/30
		stepI = (currentI - previousI)/30
		
		ctx.move_to(oCurveDims[0]*cos(previousO)/width,oCurveDims[1]*sin(previousO)/height)
	
		for i in range(1,31):
			ctx.line_to(oCurveDims[0]*cos(previousO+stepO*i)/width,oCurveDims[1]*sin(previousO+stepO*i)/height)
		
		ctx.line_to(iCurveDims[0]*cos(currentI)/width,iCurveDims[1]*sin(currentI)/height)
		
		for i in range(1,31):
			ctx.line_to(iCurveDims[0]*cos(currentI-stepI*i)/width,iCurveDims[1]*sin(currentI-stepI*i)/height)
		
		ctx.close_path()
		ctx.fill()
		
		#print iCurveDims[0]*cos(currentI),iCurveDims[1]*sin(currentI)
		#print oCurveDims[0]*cos(currentO),oCurveDims[1]*sin(currentO)
		
		previous = current
		curlen=0
		
		if flag:
			break
			
	ctx.restore() 

# Input from file :
# Each line in file must be in following format 
# filename width height borderSizesx4 borderRadiix8  	
# Eg : border 800 600 50 50 50 50 100 100 100 100 100 100 100 100
# Comment lines start with #

if __name__ == '__main__':

	try:
		filename = sys.argv[1]
	except:
		print "Enter testcases filename :"	 
		filename = raw_input()
	
	#try:
	f = open(filename,'r')
	#except IOError:
	#	print "Error file doesnt exist"	
	
	for line in f.readlines():
		if '#' in line:
			continue
		else:
			
			try:
				line = line.rstrip('\r\n')
				parts = line.split()
				outputFile,width,height = parts[0],float(parts[1]),float(parts[2])
				borderLengths = [width,height,width,height]
				borderSizes = [float(parts[m]) for m in range(3,7)]
				borderRadii = [[float(parts[m]),float(parts[m+1])] for m in range(7,15,2)]
				innerRadii = [[0,0],[0,0],[0,0],[0,0]]
				dashLength = max(borderSizes)*2
		
				#Cairo initialization
				surface = cairo.ImageSurface (cairo.FORMAT_ARGB32,int(width), int(height))
				ctx = cairo.Context (surface)
				ctx.scale (width,height)	 

				main(outputFile)
			except:
				print "Bad formatting in " + line			
				continue

