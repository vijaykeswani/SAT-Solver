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
for i in range(0,b):
	for j in range(0,a):
		x = random.randint(0,2)
		var = chr(ord('a') + j)
		nvar = "~" + chr(ord('a') + j)
		if x==0:
		    print var,
		else:
		    print nvar,
	print ''

