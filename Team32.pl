


convertBinToDec(A,D):-
    convertBinToDec(A,D,0,0).

convertBinToDec(0,D,_,D).
convertBinToDec(A,D,Count,Acc):-
    A > 0,
    Md is A mod 10,
    Md = 1,
    pow(2,Count,Res1),
    Acc1 is Acc + Res1,
    Count1 is Count+1,
    A1 is A//10,
    convertBinToDec(A1,D,Count1,Acc1).
convertBinToDec(A,D,Count,Acc):-
    A > 0,
    Md is A mod 10,
    Md = 0,
    Count1 is Count+1,
    A1 is A//10,
    convertBinToDec(A1,D,Count1,Acc).

replaceIthItem(Item,[_|List],0,[Item|List]).

replaceIthItem(Item,[H|List],I,[H|Result]):-
    I>0,
    I1 is I - 1,
    replaceIthItem(Item,List,I1,Result).

splitEvery(N,L,R):-
		splitter(N,0,[],L,R).

splitter(_,_,Acc,[],[Acc]).

splitter(N,N,Acc,L,[Acc|T]):-
		L\==[],
		splitter(N,0,[],L,T).

splitter(N,I,Acc,[H|T],R):-
		I<N,
		I1 is I+1,
		append(Acc,[H],Acc1),
		splitter(N,I1,Acc1,T,R).

logBase2(Num,0):-Num=<1.
logBase2(Num,Res):-
    Num>1,
    Num1 is Num/2,
    logBase2(Num1,Res1),
    Res is Res1+1.

getNumBits(NumOfSets,Type,Cache,BitsNum):-
    (   Type = fullyAssoc,BitsNum is 0);
    (   Type = setAssoc , logBase2(NumOfSets,BitsNum));
    (   Type = directMap,length(Cache,N),
        logBase2(N,R1),BitsNum is R1).

fillZeros(String,0,String).

fillZeros(String,N,Res):-
    N>0,
    N1 is N-1,
    fillZeros(String,N1,R1),
    string_concat(0,R1,Res).



%--------Abdelrahman Fathy-------


getDataFromCache(Stringadd,Cache,Data,HopNum,setAssoc,SetsNum):-
		getNumBits(SetsNum,setAssoc,Cache,Idxbit),
		string_chars(Stringadd,LAddress),
		length(LAddress,Len),
		helpgetData(LAddress,Idxbit,Len,R),
		atom_number(R,Bin),
		convertBinToDec(Bin,Idx),
		length(Cache,Clen),
		helpGetSetSize(Clen,SetsNum,SetSize),
		splitEvery(SetSize,Cache,L),
		nth0(Idx,L,SS),
		TagBit is Len-Idxbit,
		Len1 is Len-Idxbit,
		helpgetData(LAddress,TagBit,Len1,R1),
		helpSearch(SS,HopNum,R1,Data).

helpSearch([H|_],0,Tag,Data):-
		H=item(tag(Tag), data(Data), 1, _).


helpSearch([H|T],HopNum,Tag,Data):-
		H\=item(tag(Tag), data(Data), 1, _),
		helpSearch(T,HopNum1,Tag,Data),
		HopNum is HopNum1+1.

helpGetSetSize(Clen,SetsNum,Size):-
		X is Clen mod SetsNum,
		X=0,
		Size is Clen//SetsNum.


helpGetSetSize(Clen,SetsNum,Size):-
		X is Clen mod SetsNum,
		X\=0,
		Size is Clen//SetsNum+1.

helpgetData(_,0,_,"").

helpgetData(L,Idxbit,I,R):-
		nth1(I,L,X),
		I1 is I-1,
		Idxbit1 is Idxbit-1,
		helpgetData(L,Idxbit1,I1,R1),
		string_concat(R1,X,R).

%---------------
convertAddress(Bin,SetsNum,Tag,Idx,setAssoc):-
		string_concat(Bin,"",AddString),
		string_chars(AddString,L),
		length(L,Len),
		logBase2(SetsNum,Idxbit),
		helpgetData(L,Idxbit,Len,R1),
		TagBit is Len-Idxbit,
		Len1 is Len-Idxbit,
		helpgetData(L,TagBit,Len1,R2),
		atom_numberh(R1,Idx),
		atom_numberh(R2,Tag).

atom_numberh("",0).
atom_numberh(A,X):- A\=="",atom_number(A,X).

%---------------


replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,setAssoc,SetsNum):-
		logBase2(SetsNum,Idxbits),
		creatStrBin(Idx,Idxbits,StrIdx),
		N is 6-Idxbits,
		creatStrBin(Tag,N,Tag1),
		string_concat(Tag1,StrIdx,StrMemIdx),
		atom_number(StrMemIdx,BinMemIdx),
		convertBinToDec(BinMemIdx,Memidx),
		nth0(Memidx,Mem,ItemData),
		convertBinToDec(Idx,Idx1),
		updateCache(OldCache,Tag1,Idx1,ItemData,SetsNum,NewCache).


creatStrBin(Tag,N,Tag1):-
		string_concat("",Tag,R1),
		string_chars(R1,L),
		length(L,X),
		Zero is N-X,
		fillZeros(R1,Zero,Tag1).


updateCache(OldCache,Tag,Idx,Data,SetsNum,NewCache):-
		length(OldCache,Clen),
		helpGetSetSize(Clen,SetsNum,Size),
		splitEvery(Size,OldCache,SplitOld),
		nth0(Idx,SplitOld,Set),
		nth0(0,Set,Item),
		getReplacedIdx(Set,Item,0,0,I),
		updateOrder(Set,Set1),
		replaceIthItem(item(tag(Tag),data(Data),1,0),Set1,I,ModifSet),
		replaceIthItem(ModifSet,SplitOld,Idx,SplitNew),
		splitEvery(Size,NewCache,SplitNew).

updateOrder([],[]).


updateOrder([item(Tag,D,1,O)|T],[item(Tag,D,1,O1)|Set1]):-
		O1 is O+1,
		updateOrder(T,Set1).

updateOrder([item(Tag,D,0,O)|T],[item(Tag,D,0,O)|Set1]):-
		updateOrder(T,Set1).

getReplacedIdx([],_,_,I,I).

getReplacedIdx([item(_,_,1,O)|T],item(_,_,1,O2),Iacc,_,I):-
		O>=O2,
		Iacc1 is Iacc+1,
		getReplacedIdx(T,item(_,_,1,O),Iacc1,Iacc,I).

getReplacedIdx([item(tag(_),_,1,O)|T],item(_,_,1,O2),Iacc,Ix,I):-
		O<O2,
		Iacc1 is Iacc+1,
		getReplacedIdx(T,item(_,_,1,O2),Iacc1,Ix,I).

getReplacedIdx([item(_,_,0,_)|_],_,Iacc,_,Iacc).
%----------------------

%---------Fekri--------

convertDictoBin(0,0,_).

convertDictoBin(A,B,C):-
    A > 0,
    pow(2,C,R),
    R1 is A - R,
    R1 >= 0,
    pow(10,C,R2),
    C1 is C-1,
    convertDictoBin(R1,B1,C1),
    B is B1+R2.

convertDictoBin(A,B,C):-
    A > 0,
    pow(2,C,R),
    R1 is A - R,
    R1 < 0,
    C1 is C-1,
    convertDictoBin(A,B1,C1),
    B = B1.
convertDictoBin(A,B):-
    logBase2(A,N1),
    convertDictoBin(A,B,N1).


getDataFromCache(StringAddress,[item(tag(Tag),data(Data),1,_)|_],Data,0,directMap,BitsNum,Index):-
    pow(2,BitsNum,Res),
    Index < Res,
    convertDictoBin(Index,BinaryNumber),
    atom_number(X,BinaryNumber),
    atom_length(X,N1),
    N2 is BitsNum - N1,
    fillZeros(X,N2,R3),
    string_concat(Tag,R3,StringAddress).

getDataFromCache(StringAddress,[item(tag(Tag),data(_),_,_)|Cache],Data,HopsNum,directMap,BitsNum,Index):-
    pow(2,BitsNum,Res),
    Index < Res,
    convertDictoBin(Index,BinaryNumber),
    atom_number(X,BinaryNumber),
    atom_length(X,N1),
    N2 is BitsNum - N1,
    fillZeros(X,N2,R3),
    \+string_concat(Tag,R3,StringAddress),
    Index1 is Index+1,
    getDataFromCache(StringAddress,Cache,Data,HopsNum,directMap,BitsNum,Index1).

getDataFromCache(StringAddress,Cache,Data,HopsNum,directMap,BitsNum):-
    getDataFromCache(StringAddress,Cache,Data,HopsNum,directMap,BitsNum,0).

convertAddress(Bin,BitsNum,Tag,Idx,directMap):-
    pow(10,BitsNum,Res),
    Idx is Bin mod Res,
    Tag is Bin // Res.

find_data(0,[X|_],X).

find_data(Idx,[_|Mem],Res):-
    Idx > 0,
    Idx1 is Idx - 1,
    find_data(Idx1,Mem,Res).

find_lengthTag([item(tag(Tag),data(_),_,_)|_],R):-
    atom_length(Tag,R).

copy([item(tag(_),data(_),_,_)|OldCach],[item(tag(Tag),data(_),1,0)|OldCach],Tag,0).

copy([item(tag(Tag1),data(Data),A,B)|OldCach],[item(tag(Tag1),data(Data),A,B)|NewCach],Tag,Idx):-
    Idx > 0,
    Idx1 is Idx -1,
    copy(OldCach,NewCach,Tag,Idx1).

replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,directMap,BitsNum):-
    atom_number(Sidx,Idx),
    atom_number(Stag,Tag),
    atom_length(Sidx,N1),
    atom_length(Stag,N2),
    F1 is BitsNum - N1,
    find_lengthTag(OldCache,N3),
    F2 is N3-N2,
    fillZeros(Sidx,F1,Sidx1),
    fillZeros(Stag,F2,Stag1),
    string_concat(Stag1,Sidx1,StringAddress),
    atom_number(StringAddress,NumberAddress),
    convertBinToDec(NumberAddress,NumberInDec),
    find_data(NumberInDec,Mem,ItemData),
    convertBinToDec(Idx,IdxinDec),
    copy(OldCache,NewCache,Stag1,IdxinDec),
    getDataFromCache(StringAddress,NewCache,ItemData,_,directMap,BitsNum).




%----------------------


%-------Mina-----------
convertDecToBin(0,"0").
convertDecToBin(1,"1").
convertDecToBin(N,B):-
	N>1,X is N mod 2,
	Y is N//2,
	convertDecToBin(Y,B1),
	atom_concat(B1, X, B).

getDataFromCache(StringAddress,Cashe,Data,HopsNum,fullyAssoc,_):-
	getDataFromCache(StringAddress,Cashe,Data,HopsNum,fullyAssoc,_,0).

getDataFromCache(StringAddress,[item(tag(S),data(Data),1,_)|_],Data,HopsNum,fullyAssoc,_,HopsNum):-
	atom_number(StringAddress,A),
	atom_number(S,A).

getDataFromCache(StringAddress,[item(tag(X),_,_,_)|T],Data,HopsNum,fullyAssoc,_,Acc):-
	atom_number(StringAddress,A),
	atom_number(X,B),
	A\==B,
	Acc1 is Acc+1,
	getDataFromCache(StringAddress,T,Data,HopsNum,fullyAssoc,_,Acc1).


convertAddress(Bin,_,Tag,_,fullyAssoc):-
	convertBinToDec(Bin,X1),
	convertDecToBin(X1,X2),
	atom_number(X2,Tag).


replaceInCache(Tag,_,Mem,OldCashe,NewCache,ItemData,fullyAssoc,_):-
	replaceInvalid(Tag,_,Mem,OldCashe,NewCasheNotOrdered,_,fullyAssoc,_),
	getNth(Tag,Mem,ItemData),
	updateOrder(NewCasheNotOrdered,NewCache).

replaceInCache(Tag,_,Mem,OldCashe,NewCashe,ItemData,fullyAssoc,_):-
	\+(replaceInvalid(Tag,_,Mem,OldCashe,NewCashe,ItemData,fullyAssoc,_)),
	replaceWithNum(Tag,_,Mem,OldCashe,NewCashe,ItemData,fullyAssoc,_),
	getNth(Tag,Mem,ItemData).




replaceInvalid(Tag,_,Mem,OldCashe,NewCashe,_,fullyAssoc,_):-
	OldCashe\=[],
	replaceInvalid(Tag,_,Mem,OldCashe,NewCashe,_,fullyAssoc,_,0,[]).


replaceInvalid(_,_,_,[],NewCashe,_,fullyAssoc,_,1,NewCashe).

replaceInvalid(Tag,_,Mem,[item(tag(_),data(_),0,_)|T],NewCashe,_,fullyAssoc,_,0,Acc):-
	getNth(Tag,Mem,NewCasheData),
	getBits(Tag,TagBits),
	atom_number(StringTag,Tag),
	Zeros is 6-TagBits,
	fillZeros(StringTag,Zeros,NewCasheTag),
	append(Acc,[item(tag(NewCasheTag),data(NewCasheData),1,-1)],NewAcc),
	replaceInvalid(_,_,_,T,NewCashe,_,fullyAssoc,_,1,NewAcc).

replaceInvalid(Tag,_,Mem,[item(tag(X),data(Y),1,O)|T],NewCashe,_,fullyAssoc,_,0,Acc):-
	append(Acc,[item(tag(X),data(Y),1,O)],NewAcc),
		replaceInvalid(Tag,_,Mem,T,NewCashe,_,fullyAssoc,_,0,NewAcc).


replaceInvalid(Tag,_,Mem,[H|T],NewCashe,_,fullyAssoc,_,1,Acc):-
	append(Acc,[H],NewAcc),
	replaceInvalid(Tag,_,Mem,T,NewCashe,_,fullyAssoc,_,1,NewAcc).

replaceWithNum(Tag,_,Mem,OldCashe,NewCashe,_,fullyAssoc,_):-
	OldCashe\=[],
	length(OldCashe,O),
	Order is O-1,
	replaceWithNum(Tag,_,Mem,OldCashe,NewCashe,_,fullyAssoc,_,Order,[]).

replaceWithNum(_,_,_,[],NewCashe,_,fullyAssoc,_,_,NewCashe).

replaceWithNum(Tag,_,Mem,[item(tag(_),data(_),_,Order)|T],NewCashe,_,fullyAssoc,_,Order,Acc):-
	getNth(Tag,Mem,NewCasheData),
	getBits(Tag,TagBits),
	atom_number(StringTag,Tag),
	Zeros is 6-TagBits,
	fillZeros(StringTag,Zeros,NewCasheTag),
	append(Acc,[item(tag(NewCasheTag),data(NewCasheData),1,0)],NewAcc),
	replaceWithNum(Tag,_,Mem,T,NewCashe,_,fullyAssoc,_,Order,NewAcc).

replaceWithNum(Tag,_,Mem,[item(tag(X),data(Y),Valid,Or)|T],NewCashe,_,fullyAssoc,_,Order,Acc):-
	Or\=Order,
	Or1 is Or+1,
	append(Acc,[item(tag(X),data(Y),Valid,Or1)],NewAcc),
	replaceWithNum(Tag,_,Mem,T,NewCashe,_,fullyAssoc,_,Order,NewAcc).


getBits(0,0).
getBits(Num,Bits):-
	Num >0,
	Num1 is Num//10,
	getBits(Num1,Bits1),
	Bits is Bits1 + 1.


getNth(Idx,List,Res):-
	convertBinToDec(Idx,I),
	get(I,List,Res).
get(0,[H|_],H).
get(Idx,[_|T],Res):-
	Idx>0,
	Idx1 is Idx-1,
	get(Idx1,T,Res).




%-----------------------


%----------------------

getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,hit):-
		getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
		NewCache = OldCache.

getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,miss):-
		\+getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
		atom_number(StringAddress,Address),
		convertAddress(Address,BitsNum,Tag,Idx,Type),
		replaceInCache(Tag,Idx,Mem,OldCache,NewCache,Data,Type,BitsNum).

runProgram([],OldCache,_,OldCache,[],[],Type,_).

runProgram([Address|AdressList],OldCache,Mem,FinalCache,[Data|OutputDataList],[Status|StatusList],Type,NumOfSets):-
	getNumBits(NumOfSets,Type,OldCache,BitsNum),
	(Type = setAssoc, Num = NumOfSets; Type \= setAssoc, Num = BitsNum),
	getData(Address,OldCache,Mem,NewCache,Data,HopsNum,Type,Num,Status),
	runProgram(AdressList,NewCache,Mem,FinalCache,OutputDataList,StatusList,Type,NumOfSets).

%-----------------

















