local a=type local b=pcall local c=error local d=tonumber local e=assert local f
=setmetatable local g=string.format local h=table.move local i=table.pack local
j=table.unpack local k=table.create local l=table.insert local m=table.remove
local n=table.concat local o=coroutine.create local p=coroutine.yield local q=
coroutine.resume local r=coroutine.close local s=buffer.fromstring local t=
buffer.len local u=buffer.readu8 local v=buffer.readu32 local w=buffer.
readstring local x=buffer.readf32 local y=buffer.readf64 local z=bit32.bor local
A=bit32.band local B=bit32.btest local C=bit32.rshift local D=bit32.lshift local
E=bit32.extract local F=function(F)return a(F)=='number'end local G=function(G)
return a(G)=='string'end local H=function(H)return a(H)=='boolean'end local I=
function(I)return a(I)=='function'end local J={{'NOP',0,0,false},{'BREAK',0,0,
false},{'LOADNIL',1,0,false},{'LOADB',3,0,false},{'LOADN',4,0,false},{'LOADK',4,
3,false},{'MOVE',2,0,false},{'GETGLOBAL',1,1,true},{'SETGLOBAL',1,1,true},{
'GETUPVAL',2,0,false},{'SETUPVAL',2,0,false},{'CLOSEUPVALS',1,0,false},{
'GETIMPORT',4,4,true},{'GETTABLE',3,0,false},{'SETTABLE',3,0,false},{
'GETTABLEKS',3,1,true},{'SETTABLEKS',3,1,true},{'GETTABLEN',3,0,false},{
'SETTABLEN',3,0,false},{'NEWCLOSURE',4,0,false},{'NAMECALL',3,1,true},{'CALL',3,
0,false},{'RETURN',2,0,false},{'JUMP',4,0,false},{'JUMPBACK',4,0,false},{
'JUMPIF',4,0,false},{'JUMPIFNOT',4,0,false},{'JUMPIFEQ',4,0,true},{'JUMPIFLE',4,
0,true},{'JUMPIFLT',4,0,true},{'JUMPIFNOTEQ',4,0,true},{'JUMPIFNOTLE',4,0,true},
{'JUMPIFNOTLT',4,0,true},{'ADD',3,0,false},{'SUB',3,0,false},{'MUL',3,0,false},{
'DIV',3,0,false},{'MOD',3,0,false},{'POW',3,0,false},{'ADDK',3,2,false},{'SUBK',
3,2,false},{'MULK',3,2,false},{'DIVK',3,2,false},{'MODK',3,2,false},{'POWK',3,2,
false},{'AND',3,0,false},{'OR',3,0,false},{'ANDK',3,2,false},{'ORK',3,2,false},{
'CONCAT',3,0,false},{'NOT',2,0,false},{'MINUS',2,0,false},{'LENGTH',2,0,false},{
'NEWTABLE',2,0,true},{'DUPTABLE',4,3,false},{'SETLIST',3,0,true},{'FORNPREP',4,0
,false},{'FORNLOOP',4,0,false},{'FORGLOOP',4,8,true},{'FORGPREP_INEXT',4,0,false
},{'FASTCALL3',3,1,true},{'FORGPREP_NEXT',4,0,false},{'DEP_FORGLOOP_NEXT',0,0,
false},{'GETVARARGS',2,0,false},{'DUPCLOSURE',4,3,false},{'PREPVARARGS',1,0,
false},{'LOADKX',1,1,true},{'JUMPX',5,0,false},{'FASTCALL',3,0,false},{
'COVERAGE',5,0,false},{'CAPTURE',2,0,false},{'SUBRK',3,7,false},{'DIVRK',3,7,
false},{'FASTCALL1',3,0,false},{'FASTCALL2',3,0,true},{'FASTCALL2K',3,1,true},{
'FORGPREP',4,0,false},{'JUMPXEQKNIL',4,5,true},{'JUMPXEQKB',4,5,true},{
'JUMPXEQKN',4,6,true},{'JUMPXEQKS',4,6,true},{'IDIV',3,0,false},{'IDIVK',3,2,
false}}local K=-1 local L=-2 local function M()return{vectorCtor=function()c(
'vectorCtor was not provided')end,vectorSize=4,useNativeNamecall=false,
namecallHandler=function()c('Native __namecall handler was not provided')end,
extensions={},callHooks={},errorHandling=true,generalizedIteration=true,
allowProxyErrors=false,useImportConstants=false,staticEnvironment={},decodeOp=
function(N)return N end}end local function N(O)e(a(O)=='table',
'luau_settings should be a table')e(a(O.vectorCtor)=='function',
'luau_settings.vectorCtor should be a function')e(a(O.vectorSize)=='number',
'luau_settings.vectorSize should be a number')e(a(O.useNativeNamecall)==
'boolean','luau_settings.useNativeNamecall should be a boolean')e(a(O.
namecallHandler)=='function',
'luau_settings.namecallHandler should be a function')e(a(O.extensions)=='table',
'luau_settings.extensions should be a table of functions')e(a(O.callHooks)==
'table','luau_settings.callHooks should be a table of functions')e(a(O.
errorHandling)=='boolean','luau_settings.errorHandling should be a boolean')e(a(
O.generalizedIteration)=='boolean',
'luau_settings.generalizedIteration should be a boolean')e(a(O.allowProxyErrors)
=='boolean','luau_settings.allowProxyErrors should be a boolean')e(a(O.
staticEnvironment)=='table','luau_settings.staticEnvironment should be a table')
e(a(O.useImportConstants)=='boolean',
'luau_settings.useImportConstants should be a boolean')e(a(O.decodeOp)==
'function','luau_settings.decodeOp should be a function')end local function O(P,
Q)local R if Q==nil then R=P.mainProto else R=P.protoList[Q]end local S=-1 e(R.
lineinfoenabled,'proto must have debug enabled')for T=1,R.sizecode do local U=R.
instructionlineinfo[T]if U>S then S=U end end for T,U in R.protos do local V=O(P
,U)if V>S then S=V end end return S end local function P(Q,R,S,T,U)local V if R
==nil then V=Q.mainProto else V=Q.protoList[R]end e(V.lineinfoenabled,
'proto must have debug enabled')local W={}for X=1,V.sizecode do local Y=V.code[X
]local Z=V.instructionlineinfo[X]if Y.opcode==69 then local _=Y.E if(W[Z]or 0)<=
_ then W[Z]=_ end end end T(V.debugname,V.linedefined,S,W,U)for X,Y in V.protos
do P(Q,Y,S+1,T,U)end end local function Q(R,S,T)e(a(R)=='table',
'module must be a table')e(a(S)=='number'or a(S)=='nil',
'protoid must be a number or nil')e(a(T)=='function',
'callback must be a function')P(R,S,0,T,O(R))end local function R(S,T,U,V,W)
local X=S[U]if T<2 or X==nil then return X end X=X[V]if T<3 or X==nil then
return X end X=X[W]return X end local function S(T,U)if U==nil then U=M()else N(
U)end local V if a(T)=='string'then V=s(T)else V=T end local W=0 local function
X()local Y=u(V,W)W=W+1 return Y end local function Y()local Z=v(V,W)W=W+4 return
Z end local function Z()local _=x(V,W)W=W+4 return _ end local function _()local
aa=y(V,W)W=W+8 return aa end local function aa()local ab=0 for ac=0,4 do local
ad=X()ab=z(ab,D(A(ad,127),ac*7))if not B(ad,128)then break end end return ab end
local function ab()local ac=aa()if ac==0 then return''else local ad=w(V,W,ac)W=W
+ac return ad end end local ac=X()local ad=0 if ac==0 then c(
'the provided bytecode is an error message',0)elseif ac<3 or ac>6 then c(
'the version of the provided bytecode is unsupported',0)elseif ac>=4 then ad=X()
end local ae=aa()local af=k(ae)for ag=1,ae do af[ag]=ab()end local function ag(
ah)local ai=U.decodeOp(Y())local aj=A(ai,255)local ak=J[aj+1]local al=ak[1]local
am=ak[2]local an=ak[3]local ao=ak[4]local ap={opcode=aj,opname=al,opmode=am,
kmode=an,usesAux=ao}l(ah,ap)if am==1 then ap.A=A(C(ai,8),255)elseif am==2 then
ap.A=A(C(ai,8),255)ap.B=A(C(ai,16),255)elseif am==3 then ap.A=A(C(ai,8),255)ap.B
=A(C(ai,16),255)ap.C=A(C(ai,24),255)elseif am==4 then ap.A=A(C(ai,8),255)local
aq=A(C(ai,16),65535)if aq<32768 then ap.D=aq else ap.D=aq-65536 end elseif am==5
then local aq=A(C(ai,8),16777215)if aq<8388608 then ap.E=aq else ap.E=aq-
16777216 end end if ao then local aq=Y()ap.aux=aq l(ah,{value=aq,opname=
'auxvalue'})end return ao end local function ah(ai,aj)local ak=ai.kmode if ak==1
then ai.K=aj[ai.aux+1]elseif ak==2 then ai.K=aj[ai.C+1]elseif ak==3 then ai.K=aj
[ai.D+1]elseif ak==4 then local al=ai.aux local am=C(al,30)local an=A(C(al,20),
1023)ai.K0=aj[an+1]ai.KC=am if am==2 then local ao=A(C(al,10),1023)ai.K1=aj[ao+1
]elseif am==3 then local ao=A(C(al,10),1023)local ap=A(C(al,0),1023)ai.K1=aj[ao+
1]ai.K2=aj[ap+1]end if U.useImportConstants then ai.K=R(U.staticEnvironment,am,
ai.K0,ai.K1,ai.K2)end elseif ak==5 then ai.K=E(ai.aux,0,1)==1 ai.KN=E(ai.aux,31,
1)==1 elseif ak==6 then ai.K=aj[E(ai.aux,0,24)+1]ai.KN=E(ai.aux,31,1)==1 elseif
ak==7 then ai.K=aj[ai.B+1]elseif ak==8 then ai.K=A(ai.aux,15)end end
local function ai(aj)local ak=X()local al=X()local am=X()local an=X()~=0 if ac>=
4 then X()local ao=aa()W=W+ao end local ao=aa()local ap=k(ao)local aq=false for
ar=1,ao do if aq then aq=false else aq=ag(ap)end end local ar=k(ao)for as=1,ao
do ar[as]=ap[as].opcode end local as=aa()local at=k(as)for au=1,as do local av=
X()local aw if av==0 then aw=nil elseif av==1 then aw=X()~=0 elseif av==2 then
aw=_()elseif av==3 then aw=af[aa()]elseif av==4 then aw=Y()elseif av==5 then
local ax=aa()aw=k(ax)for ay=1,ax do aw[ay]=aa()end elseif av==6 then aw=aa()
elseif av==7 then local ax,ay,az,aA=Z(),Z(),Z(),Z()if U.vectorSize==4 then aw=U.
vectorCtor(ax,ay,az,aA)else aw=U.vectorCtor(ax,ay,az)end end at[au]=aw end for
au=1,ao do ah(ap[au],at)end local au=aa()local av=k(au)for aw=1,au do av[aw]=aa(
)+1 end local aw=aa()local ax=aa()local ay if ax~=0 then ay=af[ax]else ay='(??)'
end local az=X()~=0 local aA if az then local aB=X()local aC=C((ao-1),aB)+1
local aD=k(ao)local aE=k(aC)local aF=0 for aG=1,ao do aF=aF+X()aD[aG]=aF end
local aG=0 for aH=1,aC do aG=aG+Y()aE[aH]=aG%(4294967296)end aA=k(ao)for aH=1,ao
do l(aA,aE[C(aH-1,aB)+1]+aD[aH])end end if X()~=0 then local aB=aa()for aC=1,aB
do aa()aa()aa()X()end local aC=aa()for aD=1,aC do aa()end end return{
maxstacksize=ak,numparams=al,nups=am,isvararg=an,linedefined=aw,debugname=ay,
sizecode=ao,code=ap,debugcode=ar,sizek=as,k=at,sizep=au,protos=av,
lineinfoenabled=az,instructionlineinfo=aA,bytecodeid=aj}end if ad==3 then local
aj=X()while aj~=0 do aa()aj=X()end end local aj=aa()local ak=k(aj)for al=1,aj do
ak[al]=ai(al-1)end local al=ak[aa()+1]e(W==t(V),
'deserializer cursor position mismatch')al.debugname='(main)'return{stringList=
af,protoList=ak,mainProto=al,typesVersion=ad}end local function aa(ab,ac,ad)if
ad==nil then ad=M()else N(ad)end if a(ab)~='table'then ab=S(ab,ad)end local ae=
ab.protoList local af=ab.mainProto local ag=ad.callHooks.breakHook local ah=ad.
callHooks.stepHook local ai=ad.callHooks.interruptHook local aj=ad.callHooks.
panicHook local ak=true local function al()ak=false end local function am(an,ao,
ap)local function aq(...)local ar,as,at,au,av if ad.errorHandling then ar,as,at,
au,av=...else local aw=i(...)as=k(ao.maxstacksize)av={len=0,list={}}h(aw,1,ao.
numparams,0,as)if ao.numparams<aw.n then local ax=ao.numparams+1 local ay=aw.n-
ao.numparams av.len=ay h(aw,ax,ax+ay-1,1,av.list)end aw=nil ar={pc=0,name='NONE'
}at=ao.protos au=ao.code end local aw,ax,ay,az=-1,1,f({},{__mode='vs'}),f({},{
__mode='ks'})local aA=ao.k local aB=ao.debugcode local aC=ad.extensions local aD
=false local aE,aF while ak do if not aD then aE=au[ax]aF=aE.opcode end aD=false
ar.pc=ax ar.top=aw ar.name=aE.opname ax=ax+1 if ah then ah(as,ar,ao,an,ap)end if
aF==0 then elseif aF==1 then if ag then local aG=table.pack(ag(as,ar,ao,an,ap))
if aG[1]then return j(aG,2,#aG)end end ax=ax-1 aF=aB[ax]aD=true elseif aF==2
then as[aE.A]=nil elseif aF==3 then as[aE.A]=aE.B==1 ax=ax+aE.C elseif aF==4
then as[aE.A]=aE.D elseif aF==5 then as[aE.A]=aE.K elseif aF==6 then as[aE.A]=as
[aE.B]elseif aF==7 then local aG=aE.K as[aE.A]=aC[aG]or ac[aG]ax=ax+1 elseif aF
==8 then local aG=aE.K ac[aG]=as[aE.A]ax=ax+1 elseif aF==9 then local aG=ap[aE.B
+1]as[aE.A]=aG.store[aG.index]elseif aF==10 then local aG=ap[aE.B+1]aG.store[aG.
index]=as[aE.A]elseif aF==11 then for aG,aH in ay do if aH.index>=aE.A then aH.
value=aH.store[aH.index]aH.store=aH aH.index='value'ay[aG]=nil end end elseif aF
==12 then if ad.useImportConstants then as[aE.A]=aE.K else local aG=aE.KC local
aH=aE.K0 local T=aC[aH]or ac[aH]if aG==1 then as[aE.A]=T elseif aG==2 then as[aE
.A]=T[aE.K1]elseif aG==3 then as[aE.A]=T[aE.K1][aE.K2]end end ax=ax+1 elseif aF
==13 then as[aE.A]=as[aE.B][as[aE.C] ]elseif aF==14 then as[aE.B][as[aE.C] ]=as[
aE.A]elseif aF==15 then local aG=aE.K as[aE.A]=as[aE.B][aG]ax=ax+1 elseif aF==16
then local aG=aE.K as[aE.B][aG]=as[aE.A]ax=ax+1 elseif aF==17 then as[aE.A]=as[
aE.B][aE.C+1]elseif aF==18 then as[aE.B][aE.C+1]=as[aE.A]elseif aF==19 then
local aG=ae[at[aE.D+1] ]local aH=aG.nups local T=k(aH)as[aE.A]=am(an,aG,T)for U=
1,aH do local V=au[ax]ax=ax+1 local W=V.A if W==0 then local X={value=as[V.B],
index='value'}X.store=X T[U]=X elseif W==1 then local X=V.B local Y=ay[X]if Y==
nil then Y={index=X,store=as}ay[X]=Y end T[U]=Y elseif W==2 then T[U]=ap[V.B+1]
end end elseif aF==20 then local aG=aE.A local aH=aE.B local T=aE.K local U=as[
aH]as[aG+1]=U ax=ax+1 local V=true local W=ad.useNativeNamecall if W then local
X=ad.namecallHandler local Y=au[ax]local Z=Y.opcode local _,aI,aJ=Y.A,Y.B,Y.C if
ah then ah(as,ar,ao,an,ap)end if ai then ai(as,ar,ao,an,ap)end local aK if aI==0
then aK=aw-_ else aK=aI-1 end local aL=i(X(T,j(as,_+1,_+aK)))if aL[1]==true then
V=false ax=ax+1 aE=Y aF=Z ar.pc=ax ar.name=aE.opname m(aL,1)local aM=aL.n-1 if
aJ==0 then aw=_+aM-1 else aM=aJ-1 end h(aL,1,aM,_,as)end end if V then as[aG]=U[
T]end elseif aF==21 then if ai then ai(as,ar,ao,an,ap)end local aG,aH,aI=aE.A,aE
.B,aE.C local aJ if aH==0 then aJ=aw-aG else aJ=aH-1 end local aK=as[aG]local aL
=i(aK(j(as,aG+1,aG+aJ)))local aM=aL.n if aI==0 then aw=aG+aM-1 else aM=aI-1 end
h(aL,1,aM,aG,as)elseif aF==22 then if ai then ai(as,ar,ao,an,ap)end local aG=aE.
A local aH=aE.B local aI=aH-1 local aJ if aI==K then aJ=aw-aG+1 else aJ=aH-1 end
return j(as,aG,aG+aJ-1)elseif aF==23 then ax=ax+aE.D elseif aF==24 then if ai
then ai(as,ar,ao,an,ap)end ax=ax+aE.D elseif aF==25 then if as[aE.A]then ax=ax+
aE.D end elseif aF==26 then if not as[aE.A]then ax=ax+aE.D end elseif aF==27
then if as[aE.A]==as[aE.aux]then ax=ax+aE.D else ax=ax+1 end elseif aF==28 then
if as[aE.A]<=as[aE.aux]then ax=ax+aE.D else ax=ax+1 end elseif aF==29 then if as
[aE.A]<as[aE.aux]then ax=ax+aE.D else ax=ax+1 end elseif aF==30 then if as[aE.A]
==as[aE.aux]then ax=ax+1 else ax=ax+aE.D end elseif aF==31 then if as[aE.A]<=as[
aE.aux]then ax=ax+1 else ax=ax+aE.D end elseif aF==32 then if as[aE.A]<as[aE.aux
]then ax=ax+1 else ax=ax+aE.D end elseif aF==33 then as[aE.A]=as[aE.B]+as[aE.C]
elseif aF==34 then as[aE.A]=as[aE.B]-as[aE.C]elseif aF==35 then as[aE.A]=as[aE.B
]*as[aE.C]elseif aF==36 then as[aE.A]=as[aE.B]/as[aE.C]elseif aF==37 then as[aE.
A]=as[aE.B]%as[aE.C]elseif aF==38 then as[aE.A]=as[aE.B]^as[aE.C]elseif aF==39
then as[aE.A]=as[aE.B]+aE.K elseif aF==40 then as[aE.A]=as[aE.B]-aE.K elseif aF
==41 then as[aE.A]=as[aE.B]*aE.K elseif aF==42 then as[aE.A]=as[aE.B]/aE.K
elseif aF==43 then as[aE.A]=as[aE.B]%aE.K elseif aF==44 then as[aE.A]=as[aE.B]^
aE.K elseif aF==45 then local aG=as[aE.B]if aG then as[aE.A]=as[aE.C]or false
else as[aE.A]=aG end elseif aF==46 then local aG=as[aE.B]if aG then as[aE.A]=aG
else as[aE.A]=as[aE.C]or false end elseif aF==47 then local aG=as[aE.B]if aG
then as[aE.A]=aE.K or false else as[aE.A]=aG end elseif aF==48 then local aG=as[
aE.B]if aG then as[aE.A]=aG else as[aE.A]=aE.K or false end elseif aF==49 then
local aG,aH=aE.B,aE.C local aI,aJ=b(n,as,'',aG,aH)if not aI then aJ=as[aG]for aK
=aG+1,aH do aJ=aJ..as[aK]end end as[aE.A]=aJ elseif aF==50 then as[aE.A]=not as[
aE.B]elseif aF==51 then as[aE.A]=-as[aE.B]elseif aF==52 then as[aE.A]=#as[aE.B]
elseif aF==53 then as[aE.A]=k(aE.aux)ax=ax+1 elseif aF==54 then local aG={}for
aH,aI in pairs(aE.K)do aG[aA[aI+1] ]=nil end as[aE.A]=aG elseif aF==55 then
local aG=aE.A local aH=aE.B local aI=aE.C-1 if aI==K then aI=aw-aH+1 end h(as,aH
,aH+aI-1,aE.aux,as[aG])ax=ax+1 elseif aF==56 then local aG=aE.A local aH=as[aG]
if not F(aH)then local aI=d(aH)if aI==nil then c(
"invalid 'for' limit (number expected)")end as[aG]=aI aH=aI end local aI=as[aG+1
]if not F(aI)then local aJ=d(aI)if aJ==nil then c(
"invalid 'for' step (number expected)")end as[aG+1]=aJ aI=aJ end local aJ=as[aG+
2]if not F(aJ)then local aK=d(aJ)if aK==nil then c(
"invalid 'for' index (number expected)")end as[aG+2]=aK aJ=aK end if aI>0 then
if not(aJ<=aH)then ax=ax+aE.D end else if not(aH<=aJ)then ax=ax+aE.D end end
elseif aF==57 then if ai then ai(as,ar,ao,an,ap)end local aG=aE.A local aH=as[aG
]local aI=as[aG+1]local aJ=as[aG+2]+aI as[aG+2]=aJ if aI>0 then if aJ<=aH then
ax=ax+aE.D end else if aH<=aJ then ax=ax+aE.D end end elseif aF==58 then if ai
then ai(as,ar,ao,an,ap)end local aG=aE.A local aH=aE.K aw=aG+6 local aI=as[aG]if
(ad.generalizedIteration==false)or I(aI)then local aJ={aI(as[aG+1],as[aG+2])}h(
aJ,1,aH,aG+3,as)if as[aG+3]~=nil then as[aG+2]=as[aG+3]ax=ax+aE.D else ax=ax+1
end else local aJ,aK=q(az[aE],aI,as[aG+1],as[aG+2])if not aJ then c(aK)end if aK
==L then az[aE]=nil ax=ax+1 else h(aK,1,aH,aG+3,as)as[aG+2]=as[aG+3]ax=ax+aE.D
end end elseif aF==59 then if not I(as[aE.A])then c(g(
'attempt to iterate over a %s value',a(as[aE.A])))end ax=ax+aE.D elseif aF==60
then ax=ax+1 elseif aF==61 then if not I(as[aE.A])then c(g(
'attempt to iterate over a %s value',a(as[aE.A])))end ax=ax+aE.D elseif aF==63
then local aG=aE.A local aH=aE.B-1 if aH==K then aH=av.len aw=aG+aH-1 end h(av.
list,1,aH,aG,as)elseif aF==64 then local aG=ae[aE.K+1]local aH=aG.nups local aI=
k(aH)as[aE.A]=am(an,aG,aI)for aJ=1,aH do local aK=au[ax]ax=ax+1 local aL=aK.A if
aL==0 then local aM={value=as[aK.B],index='value'}aM.store=aM aI[aJ]=aM elseif
aL==2 then aI[aJ]=ap[aK.B+1]end end elseif aF==65 then elseif aF==66 then local
aG=aE.K as[aE.A]=aG ax=ax+1 elseif aF==67 then if ai then ai(as,ar,ao,an,ap)end
ax=ax+aE.E elseif aF==68 then elseif aF==69 then aE.E=aE.E+1 elseif aF==70 then
c('encountered unhandled CAPTURE')elseif aF==71 then as[aE.A]=aE.K-as[aE.C]
elseif aF==72 then as[aE.A]=aE.K/as[aE.C]elseif aF==73 then elseif aF==74 then
ax=ax+1 elseif aF==75 then ax=ax+1 elseif aF==76 then local aG=as[aE.A]if ad.
generalizedIteration and not I(aG)then local aH=au[ax+aE.D]if az[aH]==nil then
local function aI(...)for aJ,aK,aL,aM,T,U,V,W,X,Y,Z,_,aN,aO,aP,aQ,aR,aS,aT,aU,aV
,aW,aX,aY,aZ,a_,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,
bl,bm,bn,bo,bp,bq,br,bs,bt,bu,bv,bw,bx,by,bz,bA,bB,bC,bD,bE,bF,bG,bH,bI,bJ,bK,bL
,bM,bN,bO,bP,bQ,bR,bS,bT,bU,bV,bW,bX,bY,bZ,b_,b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,ca,
cb,cc,cd,ce,cf,cg,ch,ci,cj,ck,cl,cm,cn,co,cp,cq,cr,cs,ct,cu,cv,cw,cx,cy,cz,cA,cB
,cC,cD,cE,cF,cG,cH,cI,cJ,cK,cL,cM,cN,cO,cP,cQ,cR,cS,cT,cU,cV,cW,cX,cY,cZ,c_,c0,
c1,c2,c3,c4,c5,c6,c7,c8,c9,da,db,dc,dd,de,df,dg,dh,di,dj,dk,dl,dm,dn,dp,dq,dr,ds
,dt,du,dv,dw,dx,dy,dz,dA,dB,dC,dD,dE,dF,dG,dH,dI,dJ,dK,dL,dM in...do p({aJ,aK,aL
,aM,T,U,V,W,X,Y,Z,_,aN,aO,aP,aQ,aR,aS,aT,aU,aV,aW,aX,aY,aZ,a_,a0,a1,a2,a3,a4,a5,
a6,a7,a8,a9,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo,bp,bq,br,bs,bt,bu,bv,bw
,bx,by,bz,bA,bB,bC,bD,bE,bF,bG,bH,bI,bJ,bK,bL,bM,bN,bO,bP,bQ,bR,bS,bT,bU,bV,bW,
bX,bY,bZ,b_,b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,ca,cb,cc,cd,ce,cf,cg,ch,ci,cj,ck,cl,cm
,cn,co,cp,cq,cr,cs,ct,cu,cv,cw,cx,cy,cz,cA,cB,cC,cD,cE,cF,cG,cH,cI,cJ,cK,cL,cM,
cN,cO,cP,cQ,cR,cS,cT,cU,cV,cW,cX,cY,cZ,c_,c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,da,db,dc
,dd,de,df,dg,dh,di,dj,dk,dl,dm,dn,dp,dq,dr,ds,dt,du,dv,dw,dx,dy,dz,dA,dB,dC,dD,
dE,dF,dG,dH,dI,dJ,dK,dL,dM})end p(L)end az[aH]=o(aI)end end ax=ax+aE.D elseif aF
==77 then local aG=aE.KN if(as[aE.A]==nil)~=aG then ax=ax+aE.D else ax=ax+1 end
elseif aF==78 then local aG=aE.K local aH=aE.KN local aI=as[aE.A]if(H(aI)and(aI
==aG))~=aH then ax=ax+aE.D else ax=ax+1 end elseif aF==79 then local aG=aE.K
local aH=aE.KN local aI=as[aE.A]if(aI==aG)~=aH then ax=ax+aE.D else ax=ax+1 end
elseif aF==80 then local aG=aE.K local aH=aE.KN local aI=as[aE.A]if(aI==aG)~=aH
then ax=ax+aE.D else ax=ax+1 end elseif aF==81 then as[aE.A]=math.floor(as[aE.B]
/as[aE.C])elseif aF==82 then as[aE.A]=math.floor(as[aE.B]/aE.K)else c(
'Unsupported Opcode: '..aE.opname..' op: '..aF)end end for aG,aH in ay do aH.
value=aH.store[aH.index]aH.store=aH aH.index='value'ay[aG]=nil end for aG,aH in
az do r(aH)az[aG]=nil end end local function ar(...)local as=i(...)local at=k(ao
.maxstacksize)local au={len=0,list={}}h(as,1,ao.numparams,0,at)if ao.numparams<
as.n then local av=ao.numparams+1 local aw=as.n-ao.numparams au.len=aw h(as,av,
av+aw-1,1,au.list)end as=nil local av={pc=0,name='NONE'}local aw if ad.
errorHandling then aw=i(b(aq,av,at,ao.protos,ao.code,au))else aw=i(true,aq(av,at
,ao.protos,ao.code,au))end if aw[1]then return j(aw,2,aw.n)else local ax=aw[2]if
aj then aj(ax,at,av,ao,an,ap)end if G(ax)==false then if ad.allowProxyErrors
then c(ax)else ax=a(ax)end end if ao.lineinfoenabled then return c(g(
'Fiu VM Error { Name: %s Line: %s PC: %s Opcode: %s }: %s',ao.debugname,ao.
instructionlineinfo[av.pc],av.pc,av.name,ax),0)else return c(g(
'Fiu VM Error { Name: %s PC: %s Opcode: %s }: %s',ao.debugname,av.pc,av.name,ax)
,0)end end end if ad.errorHandling then return ar else return aq end end return
am(ab,af),al end return{luau_newsettings=M,luau_validatesettings=N,
luau_deserialize=S,luau_load=aa,luau_getcoverage=Q}
