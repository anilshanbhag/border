#!/usr/bin/python
#GSoC Logic Implementation
from math import *
import cairo 
import sys

from constants import *

class Rectangle:
    def __init__(self,inp):
        self.parse(inp)
        
        # Cairo initialization
        try:
            self.surface = cairo.ImageSurface (cairo.FORMAT_ARGB32, int(self.width), int(self.height))
            self.ctx = cairo.Context (self.surface)
            self.ctx.scale (self.width, self.height)
        except:
            pass
            
        # Precision of angle measurement
        self.delta = 0.1 * pi/180
        
        # Handle cases where sum of adj borderRadii > borderLength
        self.normalize()
        self.initInnerRadii()
            
    def parse(self,inp):
        """
        Input from file :
        Each line in file must be in following format
        filename width height borderSizesx4 borderRadiix8
        Use ',' or ' ' for seperating entries
        Eg : border 800 600 50 50 50 50 100 100 100 100 100 100 100 100
        Comment lines start with #
        """
        line = inp.rstrip('\r\n')
        line = line.replace(',',' ')
        parts = line.split()
        
        self.outputFile, self.width, self.height = parts[0], float(parts[1]), float(parts[2])
        self.borderLengths = [self.width, self.height] * 2
        self.borderSizes = [float(parts[m]) for m in range(3, 7)]
        self.borderRadii = [[float(parts[m]), float(parts[m + 1])] for m in range(7, 15, 2)]
        self.innerRadii = [[0, 0], [0, 0], [0, 0], [0, 0]]
        self.dashLength = max(self.borderSizes)*2

    def initInnerRadii(self):
        """ Initializes the innerRadii for all corners """ 
        for i in range(0, 4):
            if(i%2 == 0):    
                self.innerRadii[i][0] = max(self.borderRadii[i][0] - self.borderSizes[(i-1)%4],0)
                self.innerRadii[i][1] = max(self.borderRadii[i][1] - self.borderSizes[i],0)
            else:
                self.innerRadii[i][0] = max(self.borderRadii[i][0] - self.borderSizes[i],0)
                self.innerRadii[i][1] = max(self.borderRadii[i][1] - self.borderSizes[(i-1)%4],0)

    def normalize(self):
        """Rationalize some input cases like when sum of 
        borderRadii along a side exceed sideLength """
        borderRadii = self.borderRadii
        borderLengths = self.borderLengths
        for i in range(0,4):
            if borderRadii[i][i%2] + borderRadii[(i+1)%4][i%2] > borderLengths[i]:
                ratio = borderLengths[i]/(borderRadii[i][i%2] + borderRadii[(i+1)%4][i%2])
                borderRadii[i] = [m*ratio for m in borderRadii[i]]
                borderRadii[(i+1)%4] = [m*ratio for m in borderRadii[(i+1)%4]]
         
    def draw(self):
        """Draws the entire rectangle""" 
        #If width or height zero cairo
        #initialization fails 
        if not (self.width and self.height):
            return
        
        #Set the color for drawing & draw each side
        self.ctx.set_source_rgb (1.0, 0.0, 0.0)
        self.drawDashedSide(T)
        
        self.ctx.set_source_rgb (0.0, 1.0, 0.0)
        self.drawDashedSide(R)

        self.ctx.set_source_rgb (0.0, 0.0, 1.0)
        self.drawDashedSide(B)        

        self.ctx.set_source_rgb (1.0, 1.0, 0.0)
        self.drawDashedSide(L)
        
        #Writing out the surface to png file
        self.surface.write_to_png (self.outputFile + '.png') 

    def drawDashedSide(self,sideIndex):
        """ Draws the side with given sideIndex    """
    
        #Get dashLength, gapLength and offset 
        dashLength = self.dashLength
        #Cairo starts stroking by default with dash .
        #Offset is used to shift pattern start point.
        gapLength,offset = self.calculateDashes(sideIndex) 
    
        #dashed - >  Parameter required for dashed stroking
        dashes = [dashLength/self.borderLengths[sideIndex],
                  gapLength/self.borderLengths[sideIndex]]
    
        #Side is drawn in 3 parts
        #Visual shown for top side: 
        #Corner section left of side drawn clockwise till 1, line from 1 to 2
        #Corner section right of side drawn anticlockwise till 2 
        #       1____________________2
        #      /                     \
        #     /                       \
    
        #Draw straight section 
        self.drawStraightSection(sideIndex, dashes, offset)

        #Draw left section
        if all(self.innerRadii[sideIndex]):
            self.drawCorner(sideIndex, dashLength, gapLength, -1)
    
        #Draw right section 
        #All corners are drawn in 2 steps - if innerRadii<0 its enough to handle once
        #And draw a solid corner
        cornerR = (sideIndex + 1)%4 
        if all(self.innerRadii[cornerR]):
            self.drawCorner(cornerR, dashLength, gapLength, 1)            
        else:
            self.drawSolidCorner(cornerR)
            
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
            offset = (curvedLengthL - dashLength/2)%(dashLength + gapLength) + dashLength
    
        return (gapLength, offset/self.borderLengths[sideIndex])
    
    def computeCurvedLength(self,sideIndex, cornerIndex):
        """Computes the length of side to be taken for gap width calculation"""
    
        # A side is considered to be made of 3 sections
        # one straight section and two curved sections on either borderLengths 
        # of the straight section 
        # The curved section is a part of the corner adjacent to side 
        # demarcated by the angle proportional to the border widths
    
        # A sample corner 
        #       .________<-side under consideration
        #      /|
        #     / |a
        #    /__|
        #    |b
        # All calculations are made along the 'central' ellipse
        # This is an approximation : its semi major axis are the average of the outer and inner
        
        # a is radius of ellipse [central ellipse] adjacent to side under consideration 
        
        a = (self.borderRadii[cornerIndex][1-sideIndex%2] + self.innerRadii[cornerIndex][1-sideIndex%2])/2
    
        # b is the other radius 
        b = (self.borderRadii[cornerIndex][sideIndex%2] + self.innerRadii[cornerIndex][sideIndex%2])/2
        
        #If a or b==0 then it is treated as solid corner
        if not (a and b):
            return 0
        
        borderSizes = self.borderSizes
       
        # t is the demarcating angle 
        
        if sideIndex==cornerIndex: 
            combinedSize = borderSizes[sideIndex]+borderSizes[(sideIndex-1)%4]
        else: 
            combinedSize = borderSizes[sideIndex]+borderSizes[(sideIndex+1)%4]
            
        t = borderSizes[sideIndex]/combinedSize * pi/2
        
        # Calculation ::
        # There exist 2 cases : 
        # When a is along major axis or along minor axis
        # Depending on this we need to choose appropriate limits 
        # In general if R,r are length of semi major axis and semi minor axis
        # Then all points on ellipse are of the from (R*cosT , r*sinT)    
        # Also we have y = x*tan(t)
        # Using appropriate reference frames we get : 
        
        if a >= b:
            T = pi/2 - atan(a/b * tan(t))    
            k = 1 - (b/a)**2
            return a * self.EllipseE(k,pi/2, T)
        else:
            #T = atan(b/(a * tan(t)))
            T = atan(a/b * tan(t))
            k = 1 - (a/b)**2
            return b * self.EllipseE(k, T, 0.0)    

    def EllipseE(self,k, ph2, ph1, shape = 1):
        """
        Ellipse(k,ph1,ph2) : Computes the elliptic integral second kind 
        Integral from ph1 to ph2 of sqrt (1-k*sinSquare(phi)) 
        OR sqrt (1-k*cosSquare(phi)) decided by shape
        k is m**2 where m is eccentricity
        Uses Simpson's 3/8 rule : gives result correctly approxiamted to one decimal place 
        Furthur Reading :http://en.wikipedia.org/wiki/Simpson's_rule
        """
        def func1(x, t):
            return sqrt(1-x*sin(t)*sin(t))
        def func2(x, t):
            return sqrt(1-x*cos(t)*cos(t))            
        if shape:
            return abs(ph2-ph1)/8*(func1(k, ph1) + 3*func1(k, (2*ph1 + ph2)/3) + \
                                   3*func1(k, (ph1 + 2*ph2)/3) + func1(k, ph2))
        else:
            return abs(ph2-ph1)/8*(func2(k, ph1) + 3*func2(k, (2*ph1 + ph2)/3) + \
                                   3*func2(k, (ph1 + 2*ph2)/3) + func2(k, ph2))

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
        
        #Additional parameter to get the right length
        #while using EllipseE
        if self.borderRadii[corner][1] > self.borderRadii[corner][0]: shape = 1
        else: shape = 0        
        
        ctx.save()
        
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
        combinedSize = self.borderSizes[(corner-1)%4] + self.borderSizes[corner]
        start = self.borderSizes[corner]/combinedSize * pi/2
    
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
        
        # First try to draw half a dash 
        while curlen<dash/2:
            current += dir*delta
            curlen = R * self.EllipseE(k, current, previous, shape)

        # We wish to check if current >   endAngle while moving CCW 
        # and current < endAngle while moving CW
        # Dir == -1  = >   Flipping inequality  
        if dir*current > dir*endAngle:
            current = endAngle
            flag = 1
        
        # atan has range[-pi/2,pi/2] 
        # so while calling absToParam or paramToAbs we convert to
        # an angle in range[-pi/2,pi/2] and then convert back
        
        # here we convert current into absolute angle and then convert 
        # it back into parameteric angle currentO
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
        
        #Consider dividing the curves into 30 sections each.
        stepO = (currentO - previousO)/30
        stepI = (currentI - previousI)/30
    
        ctx.move_to(oCurveDims[0] * cos(previousO)/width, oCurveDims[1] * sin(previousO)/height)
        
        #Iterate over to get the curved path .Here i in [1,30]  
        for i in range(1, 31):
            ctx.line_to(oCurveDims[0] * cos(previousO + stepO*i)/width,
                        oCurveDims[1] * sin(previousO + stepO*i)/height)
    
        ctx.line_to(iCurveDims[0] * cos(currentI)/width, iCurveDims[1] * sin(currentI)/height)
    
        for i in range(1, 31):
            ctx.line_to(iCurveDims[0] * cos(currentI - stepI*i)/width,
                        iCurveDims[1] * sin(currentI - stepI*i)/height)
    
        ctx.close_path()
        ctx.fill()

        previous = current
        curlen = 0
    
        if flag:
            ctx.restore()
            return
        
        # If we reach this point it means we havent reached endAngle
        # Now alternatively draw the gap , dash pattern
        while True:
            while curlen<gap:
                current += dir*delta
                curlen = R * self.EllipseE(k, current, previous, shape)
        
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
                curlen = R * self.EllipseE(k, current, previous, shape)
        
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
                ctx.line_to(oCurveDims[0] * cos(previousO + stepO*i)/width,
                            oCurveDims[1] * sin(previousO + stepO*i)/height)
        
            ctx.line_to(iCurveDims[0]*cos(currentI)/width, iCurveDims[1]*sin(currentI)/height)
        
            for i in range(1, 31):
                ctx.line_to(iCurveDims[0] * cos(currentI - stepI*i)/width,
                            iCurveDims[1] * sin(currentI - stepI*i)/height)
        
            ctx.close_path()
            ctx.fill()
        
            previous = current
            curlen = 0
        
            if flag:
                break
            
        ctx.restore()
    
    def drawSolidCorner(self,corner):
        """Fills the corner section : FillStyle - solid"""
        #Check if borderRadii exists at that corner

        borderRadii = (self.borderRadii[corner][0]/self.width, 
                      self.borderRadii[corner][1]/self.height)
        ctx = self.ctx
        
        ctx.save()
        
        #Check if borderRadii are all > 0
        if not all(borderRadii):
            offsetX,offsetY = 0.0,0.0
            if corner in [TR,BR]:
                offsetX = 1 - borderRadii[0]
            if corner in [BL,BR]:
                offsetY = 1 - borderRadii[1]            
            ctx.rectangle(offsetX,offsetY,
                          borderRadii[0],borderRadii[1])        
            ctx.fill()
        
        else:
            #The ratio to be used for the control points
            #of bezier curve
            #Refer rounded rectangle in gfxContext.cpp            
            alpha = 0.55191497
            
            if corner == TL:
                cornerX, cornerY = borderRadii[0], borderRadii[1]
            elif corner == TR:
                cornerX, cornerY = 1.0 - borderRadii[0], borderRadii[1]
            elif corner == BR:
                cornerX, cornerY = 1.0 - borderRadii[0], 1.0 - borderRadii[1]
            elif corner == BL: 
                cornerX, cornerY = borderRadii[0], 1.0 - borderRadii[1]            
            
            # p0 = p1 = p2 = p3 <- This will not work
            p0 = {}
            p1 = {}
            p2 = {}
            p3 = {}

            ctx.translate(cornerX, cornerY) # Move to corner center
            cornerMults = ((-1,-1),(1,-1),(1,1),(-1,1))
            
            cornerMult = cornerMults[corner]

            p0[0] = cornerMult[0] * borderRadii[0]
            p0[1] = 0.0

            p3[0] = 0.0
            p3[1] = cornerMult[1] * borderRadii[1]

            p1[0] = p0[0]  
            p1[1] = p3[1] * alpha

            p2[0] = p0[0] * alpha
            p2[1] = p3[1]
            
            ctx.move_to(0.0, 0.0)
            ctx.line_to(p0[0], p0[1])
            ctx.curve_to(p1[0], p1[1], p2[0], p2[1], p3[0], p3[1])
            ctx.close_path()
            ctx.fill()

            
            innerRadii = self.innerRadii[corner]
            #If only one innerRadii < 0 
            #fill in the left over rectangular space
            if not innerRadii[0]==innerRadii[1]==0:
                if innerRadii[0] > 0:
                    side = corner if corner%2 == 0 else (corner - 1)%4
                    ctx.rectangle(0.0, 0.0,cornerMult[0] * borderRadii[0],
                        -cornerMult[1] * (self.borderSizes[side]/self.height - borderRadii[1]))
                    ctx.fill()
                        
                if innerRadii[1] > 0:    
                    side = corner if corner%2 == 1 else (corner-1)%4
                    ctx.rectangle(0.0, 0.0,
                        -cornerMult[0] * (self.borderSizes[side]/self.width - borderRadii[0]) ,
                        cornerMult[1] * borderRadii[1])
                    ctx.fill()
                
        ctx.restore()                
    
    def writeSurface(self):
        """Write out the surface to png file"""
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
    #    print "Error file doesnt exist"
    
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

