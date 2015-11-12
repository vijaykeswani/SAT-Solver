import random
import sys

a = int(sys.argv[1])
b = int(sys.argv[2])
#a=a+1
#b = b+1
for i in range(0,a):
	var = chr(ord('a')+i)
	print var,

print ''
print b
v=[]
for i in range(0,b):
	y=""
	for j in range(0,a):
		x = random.randint(0,2)
		var = chr(ord('a') + j)
		nvar = "~" + chr(ord('a') + j)
		if x==0:
		    print var,
		    y=y+var
		elif x==1:
		    print nvar,
		    y=y+nvar
	v.append(y)
	print ''
v=list(set(v))
#print len(v)
