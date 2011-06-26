#!/usr/bin/python
#GSoC Logic Implementation
from math import *
from constants import *
import cairo 
import sys


# Make only draw methods public
# rest private ?? 

class Rectangle:
	def __init__(self,inp):
		self.parse(inp)		
		
		self.surface = cairo.ImageSurface (cairo.FORMAT_ARGB32, int(self.width), int(self.height))
		self.ctx = cairo.Context (self.surface)
		self.ctx.scale (self.width, self.height)
		
		self.delta = 0.1*pi/180
		
		self.initInnerRadii()
	
	def parse(self,inp):
		"""
		Input from file :
		Each line in file must be in following format 
		filename width height borderSizesx4 borderRadiix8  	
		Eg : border 800 600 50 50 50 50 100 100 100 100 100 100 100 100
		Comment lines start with #
		"""
		line = inp.rstrip('\r\n')
		parts = line.split()
		
		self.outputFile, self.width, self.height = parts[0], float(parts[1]), float(parts[2])
		self.borderLengths = [self.width, self.height] * 2
		self.borderSizes = [float(parts[m]) for m in range(3, 7)]
		self.borderRadii = [[float(parts[m]), float(parts[m + 1])] for m in range(7, 15, 2)]
		self.innerRadii = [[0, 0], [0, 0], [0, 0], [0, 0]]
		self.dashLength = max(self.borderSizes)*2
		
	def draw(self):
		""" 
		"""
		#Set the color for drawing & draw each side
		self.ctx.set_source_rgb (1.0, 0.0, 0.0)
		self.drawDashedSide(T)
		
		self.ctx.set_source_rgb (0.0, 1.0, 0.0)
		self.drawDashedSide(R)
		
		self.ctx.set_source_rgb (1.0, 1.0, 0.0)
		self.drawDashedSide(L)
		
		self.ctx.set_source_rgb (0.0, 0.0, 1.0)
		self.drawDashedSide(B)
	
		#Writing out the surface to png file
		self.surface.write_to_png (self.outputFile + '.png') 

	def initInnerRadii(self):
		""" Initializes the innerRadii for all corners """ 
		for i in range(0, 4):
			if(i%2 == 0):	
				self.innerRadii[i][0] = max(self.borderRadii[i][0] - self.borderSizes[(i-1)%4], 0)
				self.innerRadii[i][1] = max(self.borderRadii[i][1] - self.borderSizes[i], 0)
			else:
				self.innerRadii[i][0] = max(self.borderRadii[i][0] - self.borderSizes[i], 0)
				self.innerRadii[i][1] = max(self.borderRadii[i][1] - self.borderSizes[(i-1)%4], 0)
			
	def drawDashedSide(self,sideIndex):
		""" Draws the side with given sideIndex	"""
	
		#Get dashLength, gapLength and offset 
		dashLength = self.dashLength
		gapLength,offset = self.calculateDashes(sideIndex) 
	
		#dashed - >  Parameter required for dashed stroking
		dashes = [dashLength/self.borderLengths[sideIndex],
				  gapLength/self.borderLengths[sideIndex]]
	
		#Side is drawn in 3 parts
		#Visual shown for top side: 
		#Corner section left of side drawn clockwise till 1, line from 1 to 2
		#Corner section right of side drawn anticlockwise till 2 
		# 	   1____________________2
		#	  /                     \
		#	 /                       \
	
		#Draw straight section 
		self.drawStraightSection(sideIndex, dashes, offset)

		#Draw left section
		self.drawCorner(sideIndex, dashLength, gapLength, -1)
	
		#Draw right section 
		self.drawCorner((sideIndex + 1)%4, dashLength, gapLength, 1)			

	def calculateDashes(self,sideIndex):
		"""For a particular side computes the [gap width, offsetRaw] for that side """
	
		dashLength = self.dashLength
		
		# Straight Path Length
		straightLength = self.borderLengths[sideIndex] - \
						 self.borderRadii[sideIndex][sideIndex%2] - \
						 self.borderRadii[(sideIndex + 1)%4][sideIndex%2]
	
		# Length of curved section : Left of side
		curvedLengthL = self.computeCurvedLength(sideIndex, sideIndex)
	
		# Length of curved section : Right of side
		curvedLengthR = self.computeCurvedLength(sideIndex, (sideIndex + 1)%4)

		totalLength = straightLength + curvedLengthL + curvedLengthR
	
		# The totalLength = n * (dash length + gap length)
		# The constrain on 0.5*dashLength < = gapLength < = dashLength 
		# For a range of values for gapLength the lowest value chosen
		n = floor(totalLength/(1.5*dashLength))
	
		if n > 0 : gapLength = totalLength/n - dashLength 
		else: gapLength = dashLength
	
		# Calculate offset for dash pattern
		# This is for the offset parameter in ctx.set_dash 
	
		# Check if curvedLengthL is sufficient for half a dash 
		if curvedLengthL < dashLength/2:
			offset = curvedLengthL + dashLength/2
		else:
			offset = curvedLengthL + dashLength/2 - floor((curvedLengthL-dashLength/2)/(dashLength + gapLength))*(dashLength + gapLength)
	
		return (gapLength, offset/self.borderLengths[sideIndex])
	
	def computeCurvedLength(self,sideIndex, cornerIndex):
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
		
		a = (self.borderRadii[cornerIndex][1-sideIndex%2] + self.innerRadii[cornerIndex][1-sideIndex%2])/2
	
		# b is the other radius 
		b = (self.borderRadii[cornerIndex][sideIndex%2] + self.innerRadii[cornerIndex][sideIndex%2])/2
	
		borderSizes = self.borderSizes
		combinedSize = borderSizes[sideIndex] + borderSizes[(sideIndex-1)%4]
	
		# t is the demarcating angle 
		
		if sideIndex==cornerIndex: 
			combinedSize = borderSizes[sideIndex]+borderSizes[(sideIndex-1)%4]
		else: 
			combinedSize = borderSizes[sideIndex]+borderSizes[(sideIndex+1)%4]
			
		t = borderSizes[sideIndex]/combinedSize * pi/2
		
		#TODO Improve Explaination
		# Calculation ::
		# There exist 2 cases : When major axis along x axis or along y axis
		# In general if R,r are length of semi major axis and sLength of curved section : 1 - >  Left of sideemi minor axis
		# Ray at angle t intersects ellipse at a point which satisties y = x*tant
		# Also point x,y can be written as RcosT,rsinT where T is the paramteric angle	
		# T is parametric angle of point which seperates the sections of curve
		if a >= b:
			T = atan(a/b * tan(t))	
			k = 1 - (b/a)**2
			return a * self.EllipseE(k,T, 0.0)
		else:
			T = atan(b/(a * tan(t)))
			k = 1 - (a/b)**2
			return b * self.EllipseE(k, pi/2, T)	

	def EllipseE(self,k, ph2, ph1):
		"""
		Ellipse(k,ph1,ph2) : Computes the elliptic integral second kind 
		Integral from ph1 to ph2 of sqrt (1-k*sinSquare(phi))
		k is m**2 where m is eccentricity
		Uses Simpson's 3/8 rule : gives result correctly approxiamted to one decimal place 
		Furthur Reading :http://en.wikipedia.org/wiki/Simpson's_rule
		"""
		def func(x, t):
			return sqrt(1-x*sin(t)*sin(t))
		return abs(ph2-ph1)/8*(func(k, ph1) + 3*func(k, (2*ph1 + ph2)/3) + 3*func(k, (ph1 + 2*ph2)/3) + func(k, ph2))

	def drawStraightSection(self,sideIndex, dashes, offset):	
		""" Draws the straight section of given side """
		
		borderSizes = self.borderSizes
		borderRadii = self.borderRadii
		width = self.width
		height = self.height
		
		#Getting start and end points 
		if(sideIndex == T):
			startY = endY = 0.0 + borderSizes[sideIndex]/2
			startX = 0.0 + borderRadii[TL][0]
			endX = width - borderRadii[TR][0]
			
		elif(sideIndex == R):
			startX = endX = self.width - borderSizes[sideIndex]/2
			startY = 0.0 + borderRadii[TR][1]
			endY = height - borderRadii[BR][1]
		
		elif(sideIndex == B):
			startX = width - borderRadii[BR][0]
			startY = endY = height - borderSizes[sideIndex]/2 
			endX = 0.0 + borderRadii[BL][0]
			
		elif(sideIndex == L):
			startX = endX = 0.0 + borderSizes[sideIndex]/2
			startY = height - borderRadii[BL][1]
			endY = 0.0 + borderRadii[TL][1]
		
		else:
			print "Some error crept in DRAW SIDE : INDEX"
	
		#Reducing to range 0, 1 for cairo	
		startX, startY, endX, endY = startX/width, startY/height, endX/width, endY/height
	
		#Draw!
		ctx = self.ctx
		ctx.save()
		ctx.move_to(startX, startY)
		ctx.line_to(endX, endY)
		ctx.set_line_width (borderSizes[sideIndex]/self.borderLengths[(sideIndex + 1)%4])
		ctx.set_dash(dashes, offset)
		ctx.stroke()
		ctx.restore() # To remove dash pattern

	def absToParam(self,ta, a, b):
		"""
		To convert absolute to parametric angle
		ta is theta absolute [0, pi/2]
		a is arm wrt which angle given
		b is the other arm
		"""
		return atan(a*tan(ta)/b)

	def paramToAbs(self,tp, a, b):
		"""
		To convert parametric to absolute angle
		tp is theta parametric [0, pi/2]
		a is arm wrt which angle given
		b is the other arm
		"""
		return atan(b*tan(tp)/a)

	def drawCorner(self,corner, dash, gap, dir):
		"""Draws section of corner in given direction (dir) """

		ctx = self.ctx
		width = self.width
		height = self.height
		delta = self.delta
		
		#dir = -1  = >   CW
		#dir = 1  = >   CCW 
	
		if corner == TL: 
			cornerOffsetX, cornerOffsetY = self.borderRadii[0][0], self.borderRadii[0][1]
		elif corner == TR: 
			cornerOffsetX, cornerOffsetY = self.width - self.borderRadii[1][0], self.borderRadii[1][1]
		elif corner == BR: 
			cornerOffsetX, cornerOffsetY = self.width - self.borderRadii[2][0], \
										   self.height - self.borderRadii[2][1]
		elif corner == BL: 
			cornerOffsetX, cornerOffsetY = self.borderRadii[3][0], self.height - self.borderRadii[3][1]
	
		# Save the state of canvas
		ctx.save()
		ctx.translate(cornerOffsetX/width, cornerOffsetY/height) # Move to corner center
		ctx.transform(cairo.Matrix(1, 0, 0, -1, 0, 0)) # Flip Y Axis
	
		# We move along the outer curve with dimensions oCurveDims
		# oCurveDims[0] is radii along X Axis and oCurveDims[1] is radii along Y Axis
		# Similary iCurveDims is for the innerCurve and curveDims for central curve
		oCurveDims = self.borderRadii[corner]
		iCurveDims = self.innerRadii[corner]
		curveDims = [(oCurveDims[0] + iCurveDims[0])/2, 
					 (oCurveDims[1] + iCurveDims[1])/2]
	
		# start represents angle in  [0, pi/2]
		start = self.borderSizes[corner]/(self.borderSizes[(corner-1)%4] + self.borderSizes[corner]) * pi/2
	
		# End Angle [0, 2*pi] 
		# (2-corner)%4*pi/2 is when drawing anticlockwise
		# while drawing clockwise it is (2-corner)%4*pi/2 - pi/2
		# calcAngle signifies nothing - it is just used for convenience used to reduce repeated 
		# calculation of endAngle - pi/2 
		endAngle = (2-corner)%4 * pi/2
		if endAngle == 0:
			endAngle = 2*pi
	
		if dir == 1: 
			calcAngle = endAngle - pi/2
		else: 
			endAngle -= pi/2
			calcAngle = endAngle
		
		#Start angle [0, 2*pi]
		startAngle = calcAngle + self.absToParam(start, curveDims[(corner + 1)%2], curveDims[corner%2])
	
		previous = current = startAngle

		currentO = calcAngle + self.absToParam(start, oCurveDims[(corner + 1)%2], oCurveDims[corner%2])
		previousO = currentO

		currentI = calcAngle + self.absToParam(start, iCurveDims[(corner + 1)%2], iCurveDims[corner%2]) 
		previousI = currentI
	
		# R is semi major axis , r is semi minor axis
		# k is square of eccentricity
		R, r = curveDims 
		if r > R : R,r = r,R
		
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
			curlen += R * self.EllipseE(k, current, current - dir*delta)

		# We wish to check if current >   endAngle while moving CCW 
		# and current < endAngle while moving CW
		# Dir == -1  = >   Flipping inequality  
		if dir*current > dir*endAngle:
			current = endAngle
			flag = 1

		currentO = self.absToParam(
						self.paramToAbs(current - calcAngle, 
										curveDims[(corner + 1)%2], curveDims[corner%2]), 
						oCurveDims[(corner + 1)%2], oCurveDims[corner%2]
					) + calcAngle
		
		currentI = self.absToParam(
						self.paramToAbs(current - calcAngle, 
										curveDims[(corner + 1)%2], curveDims[corner%2]), 
						iCurveDims[(corner + 1)%2], iCurveDims[corner%2]
					) + calcAngle

		stepO = (currentO - previousO)/30
		stepI = (currentI - previousI)/30
	
		ctx.move_to(oCurveDims[0] * cos(previousO)/width, oCurveDims[1] * sin(previousO)/height)

		for i in range(1, 31):
			ctx.line_to(oCurveDims[0] * cos(previousO + stepO*i)/width, oCurveDims[1] * sin(previousO + stepO*i)/height)
	
		ctx.line_to(iCurveDims[0] * cos(currentI)/width, iCurveDims[1] * sin(currentI)/height)
	
		for i in range(1, 31):
			ctx.line_to(iCurveDims[0] * cos(currentI - stepI*i)/width, iCurveDims[1] * sin(currentI - stepI*i)/height)
	
		ctx.close_path()
		ctx.fill()

		previous = current
		curlen = 0
	
		if flag:
			return
	
		while True:
			while curlen<gap:
				current += dir*delta
				curlen += R * self.EllipseE(k, current, current - dir*delta)
		
			previous = current
			
			currentO = self.absToParam(
							self.paramToAbs(current - calcAngle, 
											curveDims[(corner + 1)%2], curveDims[corner%2]), 
							oCurveDims[(corner + 1)%2], oCurveDims[corner%2]
						) + calcAngle
		
			currentI = self.absToParam(
							self.paramToAbs(current - calcAngle, 
											curveDims[(corner + 1)%2], curveDims[corner%2]), 
							iCurveDims[(corner + 1)%2], iCurveDims[corner%2]
						) + calcAngle
			
			previousO = currentO
			previousI = currentI
			curlen = 0
		
			if dir*current > dir*endAngle:
				break	
			while curlen<dash:
				current += dir*delta
				curlen += R * self.EllipseE(k, current, current-dir*delta)
		
			if dir*current > dir*endAngle:
				current = endAngle
				flag = 1
				
			currentO = self.absToParam(
							self.paramToAbs(current - calcAngle, 
											curveDims[(corner + 1)%2], curveDims[corner%2]), 
							oCurveDims[(corner + 1)%2], oCurveDims[corner%2]
						) + calcAngle
		
			currentI = self.absToParam(
							self.paramToAbs(current - calcAngle, 
											curveDims[(corner + 1)%2], curveDims[corner%2]), 
							iCurveDims[(corner + 1)%2], iCurveDims[corner%2]
						) + calcAngle
			
			stepO = (currentO - previousO)/30
			stepI = (currentI - previousI)/30
		
			ctx.move_to(oCurveDims[0]*cos(previousO)/width, oCurveDims[1]*sin(previousO)/height)
	
			for i in range(1, 31):
				ctx.line_to(oCurveDims[0]*cos(previousO + stepO*i)/width, oCurveDims[1]*sin(previousO + stepO*i)/height)
		
			ctx.line_to(iCurveDims[0]*cos(currentI)/width, iCurveDims[1]*sin(currentI)/height)
		
			for i in range(1, 31):
				ctx.line_to(iCurveDims[0]*cos(currentI-stepI*i)/width, iCurveDims[1]*sin(currentI-stepI*i)/height)
		
			ctx.close_path()
			ctx.fill()
		
			previous = current
			curlen = 0
		
			if flag:
				break
			
		ctx.restore()
	
	def writeSurface():
		try:
			self.surface.write_to_png (self.outputFile + '.png') 
		except:
			"Init parameters first"
		
if __name__ == '__main__':
	
	try:
		filename = sys.argv[1]
	except:
		print "Enter testcases filename :"	 
		filename = raw_input()
	
	#try:
	f = open(filename, 'r')
	#except IOError:
	#	print "Error file doesnt exist"	
	
	for inp in f.readlines():
		if '#' in inp:
			continue
		else:
			try:
				rectangle = Rectangle(inp)
				rectangle.draw()
			except:
				print 'Bad Formatting in ',inp
				continue
				
