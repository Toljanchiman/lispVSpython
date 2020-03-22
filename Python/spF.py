#https://rextester.com/l/python3_online_compiler
from collections import OrderedDict
from sys import setrecursionlimit
setrecursionlimit(2000000000)
import pickle as cPickle
#tracemalloc не имеет смысла в pypy, потому что объекты не выделяются с помощью malloc
import tracemalloc
import timeit
import cProfile
import gc
gc.enable()
#from pprint import pprint
# (Решение1 '(<Spd2> (<Accl> 0.3) (<Time> 20) (<Spd1> 4)) Механика) ответ (f1 <Spd1> <Accl> <Time>) 10 (Решение1 '(<Time> (<Accl> 0.4) (<Spd1> 12) (<Spd2> 20)) Механика) ответ (f8 <Spd2> <Spd1> <Accl>) 20 (Решение1 '(<Time> (<Accl> 0.6) (<Spd1> 0) (<Dist> 30)) Механика) ответ (f9 <Spd1> <Accl> <Dist>) 10.0 (Решение1 '(<Accl> (<Spd1> 0) (<Time> 10) (<Dist> 5000)) Механика) ответ (f10 <Dist> <Spd1> <Time>) 100.0 (Решение1 '(<Spd2> (<Spd1> 0) (<Time> 10) (<Dist> 5000)) Механика) ответ (f1 <Spd1> (f10 <Dist> <Spd1> <Time>) <Time>) 1000.0 (Решение1 '(<Spd2> (<Accl> 616000) (<Spd1> 0) (<Dist> 0.415)) Механика) ответ (f2 <Accl> <Dist> <Spd1>) 715.038460504049 -- (Решение1 '(<Dist> (<Spd1> 20) (<Time> 5) (<Spd2> 0)) Механика) ответ (f6 <Spd1> <Time> (f8 <Spd2> <Spd1> <Time>)) 50.0 -- (Решение1 '(<Spd1> (<Dist> 100) (<Time> 20) (<Accl> 0.3)) Механика) ответ (f4 <Dist> <Time> <Accl>) 2.0 (Решение1 '(<Spd2> (<Dist> 100) (<Time> 20) (<Accl> 0.3)) Механика) ответ (f1 (f4 <Dist> <Time> <Accl>) <Accl> <Time>) 8.0 (Решение1 '(<Accl> (<Mass> 60000) (<FrPull> 90000) (<FrFric> 0)) Механика) ответ (f8 <FrPull> <FrFric> <Mass>) 1.5 -- (Решение1 '(<CfFric> (<FrFric> 2300) (<FrPull> 2300) (<Mass> 23000) (<Cfg> 9.8)) Механика) ответ (f13 <FrFric> <Mass> <Cfg>) 1.02040816326531E-02 -- (Решение1 '(<KinEn1> (<Mass> 6600) (<Spd1> 7800) (<Accl> 0)) Механика) ответ (f16 <Mass> <Spd1>) 200772000000.0 A = '<FrFric>' VT = ['<Dist>', '<KinEn1>', '<KinEn2>', '<FrPull>']
#(Решение1 '(<FrFric> (<Dist> 0.3) (<KinEn1> 20) (<KinEn2> 4) (<FrPull> 0.05)) Механика)
#(f12 (f17 (f18 <FrPull> <Dist>) <KinEn2> <KinEn1>) <Dist>)

f1 = lambda b,c,d,e,f,x,y,z: 0
f12 = lambda x,y: (x / y)
f17 = lambda x,y,z: ((x - y) + z)
f18 = lambda x,y: (x * y)

d2 = OrderedDict()
d2['<FrFric>'] = [['f1','<FrPull>','<Mass>','<Accl>'],['f1','<Cfg>', '<CfFric>', '<Mass>'],['f12','<OpFric>', '<Dist>']]
d2['<OpFric>']=[['f1','<FrFric>','<Dist>'],['f17','<OpPull>', '<KinEn2>', '<KinEn1>']]
d2['<OpPull>']=[['f18','<FrPull>', '<Dist>'],['f1','<KinEn2>', '<KinEn1>', '<OpFric>']]
d2['<Spd2>']=[['f1','<Spd1>', '<Accl>', '<Time>'],['f1','<Accl>', '<Dist>', '<Spd1>'],['f1','<KinEn2>', '<Mass>']]
d2['<Spd1>']=[['f1','<Spd2>', '<Accl>', '<Time>'],['f1','<Dist>', '<Time>', '<Accl>'],['f1','<Spd2>', '<Accl>', '<Dist>'],['f1','<KinEn1>', '<Mass>']]
d2['<Dist>']=[['f1','<Spd1>', '<Time>', '<Accl>'],['f1','<Spd2>', '<Spd1>', '<Accl>'],['f1','<OpPull>', '<FrPull>'],['f1','<OpFric>', '<FrFric>']]
d2['<Time>']=[['f1','<Spd2>', '<Spd1>', '<Accl>'],['f1','<Spd1>', '<Accl>', '<Dist>']]
d2['<Accl>']=[['f1','<Spd2>', '<Spd1>', '<Time>'],['f1','<Dist>', '<Spd1>', '<Time>'],['f1','<Spd2>', '<Spd1>', '<Dist>'],['f1','<FrPull>', '<FrFric>', '<Mass>']]
d2['<Mass>']=[['f1','<FrPull>', '<FrFric>', '<Accl>'],['f1','<FrFric>', '<CfFric>', '<Cfg>'],['f1','<KinEn2>', '<Spd2>'],['f1','<KinEn1>', '<Spd1>']]
d2['<FrPull>']=[['f1','<FrFric>', '<Mass>', '<Accl>'],['f1','<OpPull>', '<Dist>']]
d2['<CfFric>']=[['f1','<FrFric>', '<Mass>', '<Cfg>']]
d2['<KinEn2>']=[['f1','<Mass>', '<Spd2>'],['f1','<OpPull>', '<OpFric>', '<KinEn1>']]
d2['<KinEn1>']=[['f1','<Mass>', '<Spd1>'],['f1','<KinEn2>', '<OpPull>', '<OpFric>']]

A = '<FrFric>'
VT = ('<Dist>', '<KinEn1>', '<KinEn2>', '<FrPull>')
D = {'<Dist>':0.3,'<KinEn1>':20,'<KinEn2>':4,'<FrPull>':0.05}

def inputs():
	A = input("Введите аксиому::")
	print("\r\n Введите данные в формате:: {'<Accl>':0.3,'<Time>':20,'<Spd1>':4}")
	D = eval(input())
	VT = list(D.keys())
	print(A)
	print(D)
	print(VT)
	pass

def dfa(S, VT, VN, VNlist, d2):
	if len(VN) == 0:
		for itemVT in VT:
			if itemVT in d2:
				del d2[itemVT]		
		for sublistid0, cursublist0 in enumerate(d2[S]):
			tempCursublist0 = cPickle.loads(cPickle.dumps(cursublist0))
			del tempCursublist0[0]
			VNlist.append([tempCursublist0,[S],[sublistid0]])
	for itemid,item in enumerate(VNlist):
		for sitemid,sitem in enumerate(item[0]):
			if sitem in VT:
				continue
			if sitem not in d2:
				continue
			#это возможно нужно
			if sitem in item[1]:
				continue
			mainitem = []
			for sublistid, cursublist in enumerate(d2[sitem]):			
				if sublistid == 0:
					mainitem = cPickle.loads(cPickle.dumps(VNlist[itemid]))
					VNlist[itemid][1].append(sitem)
					VNlist[itemid][2].append(sublistid)
					tempCursublistIf = cPickle.loads(cPickle.dumps(cursublist))
					del tempCursublistIf[0]
					VNlist[itemid][0][sitemid:1] = tempCursublistIf
				else:
					mainitem = cPickle.loads(cPickle.dumps(mainitem))
					mainitem[1].append(sitem)
					mainitem[2].append(sublistid)
					tempCursublistElse = cPickle.loads(cPickle.dumps(cursublist))
					del tempCursublistElse[0]
					mainitem[0][sitemid:1] = tempCursublistElse
					VNlist.append(mainitem)
			for itemidx,itemx in enumerate(VNlist):
				END = set(itemx[0]).difference(set(VT))
				if (len(END) == 0):
					#print("Цепочка найдена!")
					#print(itemx[0])
					buidPosition(S, itemx)
					return
				if S in itemx[0]:
					del VNlist[itemidx]
			return dfa(S, VT, set([0]), VNlist, d2)
		VNlist[itemid][0].remove(sitem)
		#print("by!")
def buidPosition(superpos, itemx = []):
	#print("into BP!")
	#print(itemx)
	for itemid, item in enumerate(itemx[1]):
		#cursublist = d2[item][itemx[2][itemid]]
		#fn =  cursublist[0]
		#cursublist = (','.join(cursublist[1:]))
		#print(cursublist)
		#print(fn)
		superpos = superpos.replace(item, (d2[item][itemx[2][itemid]][0]+'(')+''.join( (','.join( d2[item][itemx[2][itemid]][1:] )) )+(')'))
		#superpos + (fn+'(')+''.join(cursublist)+(')') #D[" "]
		#superpos.replace(item, ('f(')+''.join()+(')'))
	print(superpos)
	#superpos = superpos.replace("<", "D['<")
	#superpos = superpos.replace(">", ">']")
	#print(eval(superpos))
	#pass

'''def loop(n=130):
	for x in range(1, n):
		#print("step X = "+str(x))
		dfa(A, VT, set([]), [], d2)'''
#inputs()
tracemalloc.start()
#cProfile.run("dfa(A, VT, set([]), [], d2)")
#cProfile.run("loop(130)")#loop(1000000)
print("\r\n Время выполнения:: "+str(timeit.timeit('dfa(A, VT, set([]), [], d2)', globals=globals(), number=1300))+" сек.")
print("\r\n Памяти использовано:: Текущ. - %d бйат, Макс. - %d байт" % tracemalloc.get_traced_memory())