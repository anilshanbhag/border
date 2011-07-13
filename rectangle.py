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
        """
        Rationalize some input cases like when sum of 
        borderRadii along a side exceed sideLength 
        """
        borderRadii = self.borderRadii
        borderLengths = self.borderLengths
        for i in range(0,4):
            pt1 = borderRadii[(i+1)%4][i%2] > self.borderSizes[(i+1)%4]
            pt2 = borderRadii[i][i%2] > self.borderSizes[(i-1)%4]
            if pt1 and pt2 :
                ratio = borderLengths[i] / (borderRadii[(i+1)%4][i%2] + borderRadii[i][i%2])  
            elif (not pt1) and pt2 :
                ratio = (borderLengths[i] - self.borderSizes[(i+1)%4]) / borderRadii[i][i%2]
            elif (not pt2) and pt1:
                ratio = (borderLengths[i] - self.borderSizes[(i-1)%4]) / borderRadii[(i+1)%4][i%2]
            else :
                ratio = 2
            if ratio < 1:
                borderRadii[i] = [m*ratio for m in borderRadii[i]]
                borderRadii[(i+1)%4] = [m*ratio for m in borderRadii[(i+1)%4]]    
    
    def draw(self):
        """Draws the entire rectangle""" 
        
        # If width or height zero cairo
        # initialization fails 
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
        
        self.surface.write_to_png (self.outputFile + '.png') 

    def drawDashedSide(self,side):
        """ Draws the side with given side    """
    
        #Get dashLength, gapLength and offset 
        dashLength = self.dashLength
        
        #Cairo starts stroking by default with dash .
        #Offset is used to shift pattern start point.
        gapLength,offset = self.calculateDashes(side) 
    
        #dashed - >  Parameter required for dashed stroking
        dashes = [dashLength/self.borderLengths[side],
                  gapLength/self.borderLengths[side]]
    
        #Side is drawn in 3 parts
        #Visual shown for top side: 
        #Corner section left of side drawn clockwise till 1, line from 1 to 2
        #Corner section right of side drawn anticlockwise till 2 
        #       1____________________2
        #      /                     \
        #     /                       \
    
        #Draw straight section 
        self.drawStraightSection(side, dashes, offset)

        #Draw left section
        # If any of the innerRadii are 0 : Length along corner  = 0
        # Fill if with solid color
        if all(self.innerRadii[side]):
            self.drawCorner(side, dashLength, gapLength, -1)
        else:
            self.drawSolidCorner(side,side,-1)

        #Draw right section 
        cornerR = (side + 1)%4 
        if all(self.innerRadii[cornerR]):
            self.drawCorner(cornerR, dashLength, gapLength, 1)            
        else:
            self.drawSolidCorner(cornerR,side,1)            
            
    def calculateDashes(self,side):
        """For a particular side computes the [gap width, offsetRaw] for that side """

        dashLength = self.dashLength
        
        sideL = (side - 1) % 4
        sideR = (side + 1) % 4
        
        # Straight Path Length
        straightLength = self.borderLengths[side] - \
                         max(self.borderRadii[side][side%2], self.borderSizes[sideL]) - \
                         max(self.borderRadii[sideR][side%2], self.borderSizes[sideR])

        # Length of curved section : Left of side
        curvedLengthL = self.computeCurvedLength(side, side)

        # Length of curved section : Right of side
        curvedLengthR = self.computeCurvedLength(side, sideR)

        totalLength = straightLength + curvedLengthL + curvedLengthR
    
        # The totalLength = n * (dash length + gap length)
        # The constrain on 0.5*dashLength < = gapLength < = dashLength 
        # For a range of values for gapLength the lowest value chosen
        n = floor(totalLength / (1.5 * dashLength))
    
        if n > 0 : gapLength = totalLength/n - dashLength 
        else: gapLength = dashLength

        # Calculate offset for dash pattern
        # This is for the offset parameter in ctx.set_dash 
        # Check if curvedLengthL is sufficient for half a dash 
        if curvedLengthL < dashLength/2:
            offset = curvedLengthL + dashLength/2
        else:
            offset = (curvedLengthL - dashLength/2)%(dashLength + gapLength) + dashLength
    
        return (gapLength, offset/self.borderLengths[side])
    
    def computeCurvedLength(self,side, cornerIndex):
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
        # All calculations are made along the ellipse defined by innerRadii
        
        # a is radius of ellipse [central ellipse] adjacent to side under consideration 
        
        a = self.innerRadii[cornerIndex][1-side%2]
    
        # b is the other radius 
        b = self.innerRadii[cornerIndex][side%2]
                
        #If a or b==0 then it is treated as solid corner
        if not (a and b):
            return 0

        borderSizes = self.borderSizes

        # t is the demarcating angle 

        if side == cornerIndex: 
            combinedSize = borderSizes[side] + borderSizes[(side-1)%4]
        else: 
            combinedSize = borderSizes[side] + borderSizes[(side+1)%4]
            
        t = borderSizes[side]/combinedSize * pi/2
        
        # Calculation ::
        # There exist 2 cases : 
        # When a is along major axis or along minor axis
        # Depending on this we need to choose appropriate limits 
        # In general if R,r are length of semi major axis and semi minor axis
        # Then all points on ellipse are of the from (R*cosT , r*sinT)    
        # Also we have y = x*tan(t)
        # Using appropriate reference frames we get : 
        
        if a >= b:
            T = pi/2 - self.paramToAbs(t, a, b)
            k = 1 - (b/a)**2
            return a * self.EllipseE(k, pi/2, T)
        else:
            T = self.paramToAbs(t, a, b)
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

    def drawStraightSection(self,side, dashes, offset):  
        """ Draws the straight section of given side """
        
        borderSizes = self.borderSizes
        borderRadii = self.borderRadii
        width = self.width
        height = self.height
        
        sideL = (side - 1) % 4
        sideR = (side + 1) % 4
        
        #Getting start and end points 
        if(side == T):
            startY = endY = 0.0 + borderSizes[side]/2
            startX = 0.0 + max(borderRadii[TL][0], borderSizes[sideL])
            endX = width - max(borderRadii[TR][0], borderSizes[sideR])
            
        elif(side == R):
            startX = endX = self.width - borderSizes[side]/2
            startY = 0.0 + max(borderRadii[TR][1], borderSizes[sideL])
            endY = height - max(borderRadii[BR][1], borderSizes[sideR])
        
        elif(side == B):
            startX = width - max(borderRadii[BR][0], borderSizes[sideL])
            startY = endY = height - borderSizes[side]/2 
            endX = 0.0 + max(borderRadii[BL][0], borderSizes[sideR])
            
        elif(side == L):
            startX = endX = 0.0 + borderSizes[side]/2
            startY = height - max(borderRadii[BL][1], borderSizes[sideL])
            endY = 0.0 + max(borderRadii[TL][1], borderSizes[sideR])
        
        else:
            print "Some error crept in DRAW SIDE : INDEX"
    
        #Reducing to range 0, 1 for cairo    
        startX, startY, endX, endY = startX/width, startY/height, endX/width, endY/height
    
        #Draw!
        ctx = self.ctx
        ctx.save()
        ctx.move_to(startX, startY)
        ctx.line_to(endX, endY)
        ctx.set_line_width (borderSizes[side]/self.borderLengths[sideR])
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

    def oIntersect(self, tp, ia, ib, oa, ob ):
        if sin(tp) == 0 or cos(tp) == 0:
            return tp
        x = ib*ob / sin(tp)
        y = ia*oa / cos(tp)
        z = ia**2 - ib**2
        sr = sqrt(x**2 + y**2)
        C = acos(z / sr)
        A = acos(y / sr)
        if C > 0:
            B = C - A
        else:
            B = - C - A
        return B

    def drawCorner(self,corner, dash, gap, dir):
        """Draws section of corner in given direction (dir) """
        
        # Entering this method means none of the innerRadii are <0
        # for the corner
        ctx = self.ctx
        width = self.width
        height = self.height
        delta = self.delta
        
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
    
        # We move along the outer curve with dimensions oCurve
        # oCurve[0] is radii along X Axis and oCurve[1] is radii along Y Axis
        # Similary iCurve is for the innerCurve
        oCurve = self.borderRadii[corner]
        iCurve = self.innerRadii[corner]

        #Additional parameter to get the right length
        #while using EllipseE
        if iCurve[1] > iCurve[0]: shape = 1
        else: shape = 0  

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
        
        #TODO Changed here
        #currentO = calcAngle + self.absToParam(start, oCurve[(corner + 1)%2], oCurve[corner%2])
        #currentO = calcAngle + self.paramToAbs(start, oCurve[(corner + 1)%2], oCurve[corner%2])
        #previousO = currentO

        #currentI = calcAngle + self.absToParam(start, iCurve[(corner + 1)%2], iCurve[corner%2])
        currentI = calcAngle + self.paramToAbs(start, iCurve[(corner + 1)%2], iCurve[corner%2])
        previousI = currentI

        #currentO = self.absToParam(
        #                self.paramToAbs(currentI - calcAngle, 
        #                                iCurve[(corner + 1)%2], iCurve[corner%2]), 
        #                oCurve[(corner + 1)%2], oCurve[corner%2]
        #            ) + calcAngle
        currentO = calcAngle + self.oIntersect(currentI - calcAngle,
                                   iCurve[(corner + 1)%2], iCurve[corner%2],
                                   oCurve[(corner + 1)%2], oCurve[corner%2])        
        previousO = currentO
    
        # R is semi major axis , r is semi minor axis
        # k is square of eccentricity
        R, r = iCurve 
        if r > R : R,r = r,R
        
        k = 1- r**2/R**2
    
        # curlen keeps track of current length covered
        curlen = 0.0
    
        # maintain a flag to exit when we reached endAngle
        # exit after drawing
        flag = 0
    
        # While moving clockwise currentI decreases whereas while moving ccw 
        # currentI increases . 
        # The dir provided is used to choose increment/decrement
        
        # First try to draw half a dash 
        while curlen < dash/2:
            currentI += dir*delta
            curlen = R * self.EllipseE(k, currentI, previousI, shape)

        # We wish to check if currentI >   endAngle while moving CCW 
        # and currentI < endAngle while moving CW
        # Dir == -1  = >   Flipping inequality  
        if dir*currentI >= dir*endAngle:
            currentI = endAngle
            flag = 1
        
        # atan has range[-pi/2,pi/2] 
        # so while calling absToParam or paramToAbs we convert to
        # an angle in range[-pi/2,pi/2] and then convert back
        
        # here we convert currentI into absolute angle and then convert 
        # it back into parameteric angle currentO
        
        currentO = calcAngle + self.oIntersect(currentI - calcAngle,
                                   iCurve[(corner + 1)%2], iCurve[corner%2],
                                   oCurve[(corner + 1)%2], oCurve[corner%2])
                                   
        #Consider dividing the curves into 30 sections each.
        stepO = (currentO - previousO)/30
        stepI = (currentI - previousI)/30
    
        ctx.move_to(oCurve[0] * cos(previousO)/width, oCurve[1] * sin(previousO)/height)
        
        #Iterate over to get the curved path .Here i in [1,30]  
        for i in range(1, 31):
            ctx.line_to(oCurve[0] * cos(previousO + stepO*i)/width,
                        oCurve[1] * sin(previousO + stepO*i)/height)
    
        ctx.line_to(iCurve[0] * cos(currentI)/width, iCurve[1] * sin(currentI)/height)
    
        for i in range(1, 31):
            ctx.line_to(iCurve[0] * cos(currentI - stepI*i)/width,
                        iCurve[1] * sin(currentI - stepI*i)/height)
    
        ctx.close_path()
        ctx.fill()
        
        previousO = currentO
        previousI = currentI
        curlen = 0
    
        if flag:
            ctx.restore()
            return
        
        # If we reach this point it means we havent reached endAngle
        # Now alternatively draw the gap , dash pattern
        while True:
            while curlen<gap:
                currentI += dir*delta
                curlen = R * self.EllipseE(k, currentI, previousI, shape)
            
            currentO = calcAngle + self.oIntersect(currentI - calcAngle,
                                       iCurve[(corner + 1)%2], iCurve[corner%2],
                                       oCurve[(corner + 1)%2], oCurve[corner%2])
            
            previousO = currentO
            previousI = currentI
            curlen = 0
        
            if dir*currentI >= dir*endAngle:
                break

            while curlen<dash:
                currentI += dir*delta
                curlen = R * self.EllipseE(k, currentI, previousI, shape)
        
            if dir*currentI >= dir*endAngle:
                currentI = endAngle
                flag = 1
            
            currentO = calcAngle + self.oIntersect(currentI - calcAngle,
                                       iCurve[(corner + 1)%2], iCurve[corner%2],
                                       oCurve[(corner + 1)%2], oCurve[corner%2])
            stepO = (currentO - previousO)/30
            stepI = (currentI - previousI)/30
        
            ctx.move_to(oCurve[0] * cos(previousO)/width, oCurve[1] * sin(previousO)/height)
    
            for i in range(1, 31):
                ctx.line_to(oCurve[0] * cos(previousO + stepO*i)/width,
                            oCurve[1] * sin(previousO + stepO*i)/height)
        
            ctx.line_to(iCurve[0]*cos(currentI)/width, iCurve[1]*sin(currentI)/height)
        
            for i in range(1, 31):
                ctx.line_to(iCurve[0] * cos(currentI - stepI*i)/width,
                            iCurve[1] * sin(currentI - stepI*i)/height)
        
            ctx.close_path()
            ctx.fill()
        
            previousI = currentI
            curlen = 0
        
            if flag:
                break
            
        ctx.restore()
    
    def drawSolidCorner(self,corner,side,dir):
        """Fills the corner section : FillStyle - solid"""
        
        #This method is to draw case C and case D of section 5.4 
        #at http://www.w3.org/TR/css3-background/
        #While working in a corner with solid corner there are
        #2 main points [which may or may not coincide]
        # The point defined by ellipse center has coordinates
        # centerX,centerY
        # The point defined by intersection of corner edges is
        # cornerX,cornerY
        
        ctx = self.ctx
        ctx.save()

        borderRadii = (self.borderRadii[corner][0]/self.width, 
                      self.borderRadii[corner][1]/self.height)

        width = self.width
        height = self.height
        borderSizes = self.borderSizes

        if corner == TL:
            centerX, centerY = borderRadii[0], borderRadii[1]
        elif corner == TR:
            centerX, centerY = 1.0 - borderRadii[0], borderRadii[1]
        elif corner == BR:
            centerX, centerY = 1.0 - borderRadii[0], 1.0 - borderRadii[1]
        elif corner == BL: 
            centerX, centerY = borderRadii[0], 1.0 - borderRadii[1]
  
        ctx.translate(centerX, centerY) 
        ctx.transform(cairo.Matrix(1, 0, 0, -1, 0, 0))
        
        #Multipliers to get right x,y at respective corners
        cornerMults = ((-1,1),(1,1),(1,-1),(-1,-1))
        cornerMult = cornerMults[corner]
        
        combinedSize = self.borderSizes[(corner-1)%4] + self.borderSizes[corner]
        start = self.borderSizes[corner]/combinedSize * pi/2
    
        endAngle = (2-corner)%4 * pi/2
        if endAngle == 0:
            endAngle = 2*pi
    
        if dir == 1: 
            calcAngle = endAngle - pi/2
        else: 
            endAngle -= pi/2
            calcAngle = endAngle
        
        oCurve = self.borderRadii[corner]    
        
        #In case borderRadii doesnt exist
        try:
            startAngle = calcAngle + self.paramToAbs(start, oCurve[(corner + 1)%2], oCurve[corner%2])
            
            ctx.move_to(oCurve[0] * cos(startAngle)/width, oCurve[1] * sin(startAngle)/height)
            
            step = (endAngle - startAngle)/30
        
            for i in range(1, 31):
                ctx.line_to(oCurve[0] * cos(startAngle + step*i)/width,
                            oCurve[1] * sin(startAngle + step*i)/height)
        except ZeroDivisionError:
            ctx.move_to(oCurve[0] * cos(endAngle)/width, oCurve[1] * sin(endAngle)/height)
        
        if corner%2 == 0:
            cornerX = (oCurve[0] - borderSizes[(corner-1)%4]) * cornerMult[0]
            cornerY = (oCurve[1] - borderSizes[corner]) * cornerMult[1]
            if corner == side:
                pX = min(oCurve[0] - borderSizes[(corner-1)%4],0) * cornerMult[0]
                pY = oCurve[1] * cornerMult[1]
            else:
                pX = oCurve[0] * cornerMult[0]
                pY = min(oCurve[1] - borderSizes[corner],0) * cornerMult[1]
        else:
            cornerX = (oCurve[0] - borderSizes[corner]) * cornerMult[0]
            cornerY = (oCurve[1] - borderSizes[(corner-1)%4]) * cornerMult[1]
            if corner == side:
                pX = oCurve[0] * cornerMult[0]
                pY = min(oCurve[1] - borderSizes[(corner-1)%4],0) * cornerMult[1]
            else:
                pX = min(oCurve[0] - borderSizes[corner],0) * cornerMult[0]
                pY = oCurve[1] * cornerMult[1]
        
        if side%2 == 0:
            qX = cornerX - self.innerRadii[corner][0] * cornerMult[0] 
            qY = cornerY
        else:    
            qX = cornerX
            qY = cornerY - self.innerRadii[corner][1] * cornerMult[1]
            
        ctx.line_to(pX / width, pY / height)
        ctx.line_to(qX / width, qY / height)
        ctx.line_to(cornerX / width, cornerY / height)    
        ctx.close_path()
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

    f = open(filename, 'r')
    
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
