#!/usr/bin/python
#GSoC Logic Implementation
from math import *
import cairo 

#Corner and Side indices
c_tl=0
c_tr=1
c_br=2
c_bl=3

side_top=0
side_right=1
side_bottom=2
side_left=3

#Input Parameters
width=800.0
height=600.0
sides=[width,height,width,height]
borderSizes=[50.0,50.0,50.0,50.0]
borderRadii=[[300.0,300.0],[100.0,100.0],[100.0,100.0],[100.0,100.0]]
delta=1.0 #Angle Increment

#Computed Parameters 
innerRadii=[[0,0],[0,0],[0,0],[0,0]]

#Cairo initialization
surface = cairo.ImageSurface (cairo.FORMAT_ARGB32,int(width), int(height))
ctx = cairo.Context (surface)
ctx.scale (width,height)

def EllipseE(k,ph2,ph1=0.0):
	"""Computes the elliptic integral second kind : Integral from ph1 to ph2 of sqrt (1-k*sinSquare(phi))
	Uses Simpson's 3/8 rule : gives result correctly approxiamted to one decimal place 
	Furthur Reading :http://en.wikipedia.org/wiki/Simpson's_rule"""
	def func(x,t):
		return sqrt(1-x*sin(t)*sin(t))
	return abs(ph2-ph1)/8*(func(k,ph1)+3*func(k,(2*ph1+ph2)/3)+3*func(k,(ph1+2*ph2)/3)+func(k,ph2))

def absToParam(ta,a,b):
	"""Absolute to Parametric angle"""
	return atan(b*tan(ta)/a)

def paramToAbs(tp,a,b):
	"""Parametric to Absolute angle"""
	return atan(a*tan(tp)/b)

def ComputeCurvedLength(sideIndex,cornerIndex):
	"""Computes the length to be included in gap width calculation"""
	#		.________<-side under consideration
	#	  /|
	#	 / |a
	#	/__|
	#	|b
	#t is the demarcating angle 
	#T is parametric angle of point which seperates the sections of curve
	#a is radii of ellipse [central ellipse] adjacent to side under consideration  
	#b is the other radii
	 
	a = (borderRadii[cornerIndex][1-sideIndex%2]+innerRadii[cornerIndex][1-sideIndex%2])/2 
	b = (borderRadii[cornerIndex][sideIndex%2]+innerRadii[cornerIndex][sideIndex%2])/2
	t = (borderSizes[sideIndex]/(borderSizes[sideIndex]+borderSizes[(sideIndex-1)%4]))*(pi/2) if sideIndex==cornerIndex else (borderSizes[sideIndex]/(borderSizes[sideIndex]+borderSizes[(sideIndex+1)%4]))*(pi/2)
	
	#Calculation ::
	#There exist 2 cases : When major axis along x axis or along y axis
	#In general if R,r are length of semi major axis and semi minor axis
	#Ray at angle t intersects ellipse at a point which satisties y = x*tant
	#Also point x,y can be written as RcosT,rsinT where T is the paramteric angle	
	
	if a>=b:
		T = atan((a/b)*tan(t))	
		k = 1-(b/a)**2
		return a*EllipseE(k,T)
	else:
		T = atan(b/(a*tan(t)))
		k = 1-(a/b)**2
		return b*EllipseE(k,pi/2,T)
	
def calculateDashes(sideIndex):
	"""For a particular side computes the [dash width,gap width,offsetRaw] for that side """
	dashWidth = 2*borderSizes[sideIndex]
	
	#Straight Path Length
	straightLength = sides[sideIndex] - borderRadii[sideIndex][sideIndex%2] - borderRadii[(sideIndex+1)%4][sideIndex%2]
	
	#1->Left of side
	curvedLength1 = ComputeCurvedLength(sideIndex,sideIndex)
	
	#2->Right of side
	curvedLength2 = ComputeCurvedLength(sideIndex,(sideIndex+1)%4)

	totalLength = straightLength + curvedLength1 + curvedLength2
	
	# The totalLength = n * (dash length + gap length)
	# The constrain on 0.5*dashWidth <= gapWidth <= dashWidth : for a range the lowest value chosen
	n=floor(totalLength/(1.5*dashWidth))
	gapWidth = totalLength/n - dashWidth
	
	# Calculate offset for dash pattern
	# This is for the offset parameter in ctx.set_dash 
	if(curvedLength1<dashWidth/2):
		offset=curvedLength1
	else:
		offset=(curvedLength1 -dashWidth/2) - floor((curvedLength1-dashWidth/2)/(dashWidth+gapWidth))*(dashWidth+gapWidth)
	
	return [dashWidth,gapWidth,offset]

def drawStraightSection(sideIndex,dashes,offset):	
	""" Draws the straight section of given side """
	if(sideIndex == side_top):
		[startX,startY,endX,endY] = [0.0+borderRadii[c_tl][0],0.0+borderSizes[sideIndex]/2,
									 width-borderRadii[c_tr][0],0.0+borderSizes[sideIndex]/2]
	elif(sideIndex == side_right):
		[startX,startY,endX,endY] = [width-(borderSizes[sideIndex]/2),0.0+borderRadii[c_tr][1],
									 width-(borderSizes[sideIndex]/2),height-borderRadii[c_br][1]]
	elif(sideIndex == side_bottom):
		[startX,startY,endX,endY] = [width-borderRadii[c_br][0],height-(borderSizes[sideIndex]/2),
									 0.0+borderRadii[c_bl][0],height-(borderSizes[sideIndex]/2)]
	elif(sideIndex == side_left):
		[startX,startY,endX,endY] = [0.0+(borderSizes[sideIndex]/2),height-borderRadii[c_bl][1],
									 0.0+(borderSizes[sideIndex]/2),0.0+borderRadii[c_tl][1]]
	else:
		print "Some error crept in DRAW SIDE : INDEX"
	
	#Reducing to range 0,1 for cairo	
	[startX,startY,endX,endY] = [startX/width,startY/height,endX/width,endY/height]
	
	#Draw!
	ctx.save()
	ctx.move_to(startX,startY)
	ctx.line_to(endX,endY)
	ctx.set_source_rgb (1.0, 0.0, 0.0) # Solid color
	ctx.set_line_width (borderSizes[sideIndex]/sides[(sideIndex+1)%4])
	ctx.set_dash(dashes,offset)
	ctx.stroke()
	ctx.restore()
 
def drawCCW(corner,dash,gap):
	"""Draws section of corner in counter clockwise sense """
	
	if corner == 0: [cornerOffsetX,cornerOffsetY] = borderRadii[0]
	elif corner == 1: [cornerOffsetX,cornerOffsetY] = [width - borderRadii[1][0],borderRadii[1][1]] 
	elif corner == 2: [cornerOffsetX,cornerOffsetY]	= [width-borderRadii[2][0],height-borderRadii[2][1]]  
	elif corner == 3: [cornerOffsetX,cornerOffsetY] = [borderRadii[3][0],height - borderRadii[3][1]]
	
	ctx.save()
	ctx.translate(cornerOffsetX/width,cornerOffsetY/height) # Move to corner center
	ctx.transform(cairo.Matrix(1,0,0,-1,0,0)) # Flip Y Axis
	
	oCurveDims = borderRadii[corner]
	iCurveDims = innerRadii[corner]
	curveDims = [(oCurveDims[0]+iCurveDims[0])/2,(oCurveDims[1]+iCurveDims[1])/2]
	
	#Start Angle [0,pi/2]
	start = (borderSizes[corner]/(borderSizes[(corner-1)%4]+borderSizes[corner]))*pi/2
	
	#End Angle [0,2*pi] 
	endAngle = (2-corner)%4 * pi/2
	if endAngle == 0:
		endAngle = 2*pi
	
	#Start angle [0,2*pi]
	startAngle = endAngle - pi/2 + absToParam(start,curveDims[(corner+1)%2],curveDims[corner%2])
									
	current = startAngle
	previous = current

	currentO = endAngle - pi/2 + absToParam(start,oCurveDims[(corner+1)%2],oCurveDims[corner%2])
	previousO = currentO

	currentI = endAngle - pi/2 + absToParam(start,iCurveDims[(corner+1)%2],iCurveDims[corner%2]) 
	previousI = currentI
	
	[R,r] = curveDims if curveDims[0]>curveDims[1] else [curveDims[1],curveDims[0]]
	k = 1- r**2/R**2
	curlen = 0.0
	flag = 0.0
	
	while curlen<dash/2:
		current += delta*pi/180
		curlen += R*EllipseE(k,current,current-delta*pi/180.0)
	
	if current>endAngle:
		current=endAngle
		flag=1
			
	currentO = absToParam(paramToAbs(current - (endAngle - pi/2),curveDims[(corner+1)%2],curveDims[corner%2]),oCurveDims[(corner+1)%2],oCurveDims[corner%2]) + endAngle - pi/2
	currentI = absToParam(paramToAbs(current - (endAngle - pi/2),curveDims[(corner+1)%2],curveDims[corner%2]),iCurveDims[(corner+1)%2],iCurveDims[corner%2]) + endAngle - pi/2

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
			current += delta*pi/180
			curlen += R*EllipseE(k,current,current-delta*pi/180)
		
		previous = current
		currentO = absToParam(paramToAbs(current - (endAngle - pi/2),curveDims[(corner+1)%2],curveDims[corner%2]),oCurveDims[(corner+1)%2],oCurveDims[corner%2]) + endAngle - pi/2
		currentI = absToParam(paramToAbs(current - (endAngle - pi/2),curveDims[(corner+1)%2],curveDims[corner%2]),iCurveDims[(corner+1)%2],iCurveDims[corner%2]) + endAngle - pi/2
		previousO = currentO
		previousI = currentI
		curlen=0
		if current>endAngle:
			break	
		while curlen<dash:
			current += delta*pi/180
			curlen += R*EllipseE(k,current,current-delta*pi/180.0)
		
		if current>endAngle:
			current=endAngle
			flag=1
				
		currentO = absToParam(paramToAbs(current - (endAngle - pi/2),curveDims[(corner+1)%2],curveDims[corner%2]),oCurveDims[(corner+1)%2],oCurveDims[corner%2]) + endAngle - pi/2
		currentI = absToParam(paramToAbs(current - (endAngle - pi/2),curveDims[(corner+1)%2],curveDims[corner%2]),iCurveDims[(corner+1)%2],iCurveDims[corner%2]) + endAngle - pi/2
	
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
			break
			
	ctx.restore() 

def drawCW(corner,dash,gap):
	if corner == 0: [cornerOffsetX,cornerOffsetY] = borderRadii[0]
	elif corner == 1: [cornerOffsetX,cornerOffsetY] = [width - borderRadii[1][0],borderRadii[1][1]] 
	elif corner == 2: [cornerOffsetX,cornerOffsetY]	= [width-borderRadii[2][0],height-borderRadii[2][1]]  
	elif corner == 3: [cornerOffsetX,cornerOffsetY] = [borderRadii[3][0],height - borderRadii[3][1]]
	
	ctx.save()
	ctx.translate(cornerOffsetX/width,cornerOffsetY/height) # Move to corner center
	ctx.transform(cairo.Matrix(1,0,0,-1,0,0)) # Flip Y Axis
	
	oCurveDims = borderRadii[corner]
	iCurveDims = innerRadii[corner]
	curveDims = [(oCurveDims[0]+iCurveDims[0])/2,(oCurveDims[1]+iCurveDims[1])/2]
	
	#Start Angle [0,pi/2]
	start = (borderSizes[corner]/(borderSizes[(corner-1)%4]+borderSizes[corner]))*pi/2
	
	endAngle = (2-corner)%4 * pi/2
	if endAngle == 0:
		endAngle = 2*pi
	
	endAngle -= pi/2	
	
	startAngle = endAngle + absToParam(start,curveDims[(corner+1)%2],curveDims[corner%2])
									
	current = startAngle
	previous = current

	currentO = endAngle + absToParam(start,oCurveDims[(corner+1)%2],oCurveDims[corner%2])
	previousO = currentO

	currentI = endAngle + absToParam(start,iCurveDims[(corner+1)%2],iCurveDims[corner%2]) 
	previousI = currentI
	
	[R,r] = curveDims if curveDims[0]>curveDims[1] else [curveDims[1],curveDims[0]]
	k = 1- r**2/R**2
	curlen = 0.0
	flag = 0.0
	
	while curlen<dash/2:
		current -= delta*pi/180
		curlen += R*EllipseE(k,current,current+delta*pi/180.0)
	
	if current<=endAngle:
		current=endAngle
		flag=1
			
	currentO = absToParam(paramToAbs(current - endAngle,curveDims[(corner+1)%2],curveDims[corner%2]),oCurveDims[(corner+1)%2],oCurveDims[corner%2]) + endAngle
	currentI = absToParam(paramToAbs(current - endAngle,curveDims[(corner+1)%2],curveDims[corner%2]),iCurveDims[(corner+1)%2],iCurveDims[corner%2]) + endAngle

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
			current -= delta*pi/180
			curlen += R*EllipseE(k,current,current+delta*pi/180)
		
		previous = current
		currentO = absToParam(paramToAbs(current - endAngle,curveDims[(corner+1)%2],curveDims[corner%2]),oCurveDims[(corner+1)%2],oCurveDims[corner%2]) + endAngle
		currentI = absToParam(paramToAbs(current - endAngle,curveDims[(corner+1)%2],curveDims[corner%2]),iCurveDims[(corner+1)%2],iCurveDims[corner%2]) + endAngle
		previousO = currentO
		previousI = currentI
		curlen=0
		if current<=endAngle:
			break
				
		while curlen<dash:
			current -= delta*pi/180
			curlen += R*EllipseE(k,current,current+delta*pi/180.0)
		
		if current<endAngle:
			current=endAngle
			flag=1
				
		currentO = absToParam(paramToAbs(current - endAngle,curveDims[(corner+1)%2],curveDims[corner%2]),oCurveDims[(corner+1)%2],oCurveDims[corner%2]) + endAngle
		currentI = absToParam(paramToAbs(current - endAngle,curveDims[(corner+1)%2],curveDims[corner%2]),iCurveDims[(corner+1)%2],iCurveDims[corner%2]) + endAngle
	
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
			break
			
	ctx.restore() 

def drawDashedSide(sideIndex):
	""" Draws section of the side with given sideIndex	"""
	#Init : Get dashWidth , gapWidth and offset 
	raw=calculateDashes(sideIndex)
	dashes=[raw[0]/sides[sideIndex],raw[1]/sides[sideIndex]]
	offset=(raw[2]+raw[0])/sides[sideIndex]
	
	#Draw straight section 
	drawStraightSection(sideIndex,dashes,offset)

	#Draw left section
	drawCW(sideIndex,raw[0],raw[1])
	#Draw right section 
	drawCCW((sideIndex+1)%4,raw[0],raw[1])
	
def initInnerRadii():
	""" Initializes the innerRadii for all corners """ 
	for i in range(0,4):
		if(i%2==1):
			innerRadii[i][0] = max(borderRadii[i][0]-borderSizes[(i-1)%4],0)
			innerRadii[i][1] = max(borderRadii[i][1]-borderSizes[i],0)
		else:
			innerRadii[i][0] = max(borderRadii[i][0]-borderSizes[i],0)
			innerRadii[i][1] = max(borderRadii[i][1]-borderSizes[(i-1)%4],0)
	 
def main():
	initInnerRadii()
	ctx.set_source_rgb (1.0, 0.0, 0.0)
	drawDashedSide(side_top)
	drawDashedSide(side_right)
	drawDashedSide(side_left)
	drawDashedSide(side_bottom)
	surface.write_to_png ("justSides.png") 	

if __name__ == '__main__':
	main()


