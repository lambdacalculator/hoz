%%%%%%%% CTM Chapter 4: Declarative Concurrency

%%%% 4.2

thread
   proc {Count N} if N>0 then {Count N-1} end end
in
   {Count 100}  % Book has 1000000
end

local X in
   X = thread 10*10 end + 100*100
   {Browse X}
end

local X1 Y1 X2 Y2 in
   thread {Browse X1} end
   thread {Browse Y1} end
   thread X1=all|roads|X2 end
   thread Y1=all|roams|Y2 end
   thread X2=lead|to|rome|_ end
   thread Y2=lead|to|rhodes|_ end
end

local X0 X1 X2 X3 in
   thread Y0 Y1 Y2 Y3 in
      {Browse [Y0 Y1 Y2 Y3]}
      Y0=X0+1
      Y1=X1+Y0
      Y2=X2+Y1
      Y3=X3+Y2
      {Browse completed}
   end
   X0=0
   X1=1
   X2=2
   X3=3
end

local ForAll L in
   proc {ForAll L P}
      case L of nil then skip
      [] X|L2 then {P X} {ForAll L2 P} end
   end
   thread {ForAll L Browse} end
   local L1 L2 in
      thread L=1|L1 end
      thread L1=2|3|L2 end
      thread L2=4|nil end
   end
end

local F Xs Ys Zs Map in
   fun {Map Xs F}
      case Xs of nil then nil
      [] X|Xr then thread {F X} end|{Map Xr F} end
   end
   {Browse thread {Map Xs F} end}
   Xs=1|2|Ys
   fun {F X} X*X end
   Ys=3|Zs
   Zs=nil
end

local Fib in
   fun {Fib X}
      if X=<2 then 1
      else thread {Fib X-1} end + {Fib X-2} end
   end
   {Browse {Fib 8}}  % Book has 25
end


%%%% 4.3

local Generate Sum in
   fun {Generate N Limit}
      if N<Limit then
         N|{Generate N+1 Limit}
      else nil end
   end
   fun {Sum Xs A}
      case Xs
      of X|Xr then {Sum Xr A+X}
      [] nil then A
      end
   end
   local Xs S in
      thread Xs={Generate 0 50} end  % Book has 150000
      thread S={Sum Xs 0} end
      {Browse S}
   end
end

local Generate FoldL Xs S in
   fun {FoldL L F U}
      case L of nil then U
      [] X|L2 then {FoldL L2 F {F U X}}
      end
   end
   fun {Generate N Limit}
      if N<Limit then
         N|{Generate N+1 Limit}
      else nil end
   end
   thread Xs={Generate 0 50} end  % Book has 150000
   thread S={FoldL Xs fun{$ X Y} X+Y end 0} end
   {Browse S}
end

local Generate Sum Xs S1 S2 S3 in
   fun {Generate N Limit}
      if N<Limit then
         N|{Generate N+1 Limit}
      else nil end
   end
   fun {Sum Xs A}
      case Xs
      of X|Xr then {Sum Xr A+X}
      [] nil then A
      end
   end
   % Multiple Readers
   thread Xs={Generate 0 50} end  % Book has 150000
   thread S1={Sum Xs 0} end
   thread S2={Sum Xs 0} end
   thread S3={Sum Xs 0} end
   {Browse [S1 S2 S3]}
end

local Filter in
   fun {Filter Xs F}
      case Xs
      of nil then nil
      [] X|Xr then
         if {F X} then X|{Filter Xr F}
         else {Filter Xr F} end
      end
   end

   local Generate Sum IsOdd Xs Ys S in
      fun {Generate N Limit}
         if N<Limit then
            N|{Generate N+1 Limit}
         else nil end
      end
      fun {Sum Xs A}
         case Xs
         of X|Xr then {Sum Xr A+X}
         [] nil then A
         end
      end
      
      fun {IsOdd X} X mod 2 \= 0 end
      thread Xs={Generate 0 50} end  % Book has 150000
      thread Ys={Filter Xs IsOdd} end
      thread S={Sum Ys 0} end
      {Browse S}
   end

   local Generate Sieve in
      fun {Generate N Limit}
         if N<Limit then
	 N|{Generate N+1 Limit}
         else nil end
      end
      local Sieve Xs Ys in
         fun {Sieve Xs}
	 case Xs
	 of nil then nil
	 [] X|Xr then Ys in
	    thread Ys={Filter Xr fun {$ Y} Y mod X \= 0 end} end
	    X|{Sieve Ys}
	 end
         end
         thread Xs={Generate 2 30} end  % Book has 100000
         thread Ys={Sieve Xs} end
         {Browse Ys}
      end
      local Sieve Xs Ys in
         fun {Sieve Xs M}
	 case Xs
	 of nil then nil
	 [] X|Xr then Ys in
	    if X=<M then
	       thread Ys={Filter Xr fun {$ Y} Y mod X \= 0 end} end
	    else Ys=Xr end
	    X|{Sieve Ys M}
	 end
         end
         thread Xs={Generate 2 100} end  % Book has 100000
         thread Ys={Sieve Xs 10} end
         {Browse Ys}
      end
   end
end

local DGenerate DSum Xs S Buffer XsB YsB SB in
   proc {DGenerate N Xs}
      case Xs of X|Xr then
         X=N
         {DGenerate N+1 Xr}
      end
   end
   fun {DSum ?Xs A Limit}
      if Limit>0 then
         X|Xr=Xs
      in
         {DSum Xr A+X Limit-1}
      else A end
   end
   thread {DGenerate 0 Xs} end    % Producer thread
   thread S={DSum Xs 0 50} end    % Consumer thread, Book has 150000
   {Browse S}

   proc {Buffer N ?Xs Ys}
      fun {Startup N ?Xs}
         if N==0 then Xs
         else Xr in Xs=_|Xr {Startup N-1 Xr} end
      end
      proc {AskLoop Ys ?Xs ?End}
         case Ys of Y|Yr then Xr End2 in
            Xs=Y|Xr
            % Get element from buffer
            End=_|End2 % Replenish the buffer
            {AskLoop Yr Xr End2}
         end
      end
      End={Startup N Xs}
   in
      {AskLoop Ys Xs End}
   end

   thread {DGenerate 0 XsB} end     % Producer thread
   thread {Buffer 4 XsB YsB} end    % Buffer thread
   thread SB={DSum YsB 0 50} end    % Consumer thread, Book has 150000
   {Browse XsB} {Browse YsB}
   {Browse SB}
end

local NotLoop NotG
   fun {NotLoop Xs}
      case Xs of X|Xr then (1-X)|{NotLoop Xr} end
   end
in
   fun {NotG Xs}
      thread {NotLoop Xs} end
   end
end

local GateMaker AndG OrG NandG NorG XorG NotLoop NotG FullAdder DelayG Latch Clock X Y Z C S in
   fun {GateMaker F}
      fun {$ Xs Ys}
         fun {GateLoop Xs Ys}
            case Xs#Ys of (X|Xr)#(Y|Yr) then
               {F X Y}|{GateLoop Xr Yr}
            end
         end
      in
         thread {GateLoop Xs Ys} end
      end
   end

   AndG ={GateMaker fun {$ X Y} X*Y end}
   OrG ={GateMaker fun {$ X Y} X+Y-X*Y end}
   NandG={GateMaker fun {$ X Y} 1-X*Y end}
   NorG ={GateMaker fun {$ X Y} 1-X-Y+X*Y end}
   XorG ={GateMaker fun {$ X Y} X+Y-2*X*Y end}

   proc {FullAdder X Y Z ?C ?S}
      K L M
   in
      K={AndG X Y}
      L={AndG Y Z}
      M={AndG X Z}
      C={OrG K {OrG L M}}
      S={XorG Z {XorG X Y}}
   end
   
   X=1|1|0|_
   Y=0|1|0|_
   Z=1|1|1|_
   
   {FullAdder X Y Z C S}
   {Browse inp(X Y Z)#sum(C S)}
   
   % copied from above
   fun {NotLoop Xs}
      case Xs of X|Xr then (1-X)|{NotLoop Xr} end
   end
   fun {NotG Xs}
      thread {NotLoop Xs} end
   end

   fun {DelayG Xs}
      0|Xs
   end

   fun {Latch C DI}
      DO X Y Z F
   in
      F={DelayG DO}
      X={AndG F C}
      Z={NotG C}
      Y={AndG Z DI}
      DO={OrG X Y}
      DO
   end

   fun {Clock}
      fun {Loop B}
         {Delay 5} B|{Loop B}  % Book had 1000 ms = 1 sec
      end
   in
      thread {Loop 1} end
   end
end


%%%% 4.5

local F1 F2 F3 A B C D X Y Z in
   fun lazy {F1 X} 1+X*(3+X*(3+X)) end
   fun lazy {F2 X} Y=X*X in Y*Y end
   fun lazy {F3 X} (X+1)*(X+1) end
   A={F1 10}
   B={F2 20}
   C={F3 30}
   D=A+B

   thread X={ByNeed fun {$} 3 end} end
   thread Y={ByNeed fun {$} 4 end} end
   thread Z=X+Y end
end

local X Z in
   thread X={ByNeed fun {$} 3 end} end
   thread X=3 end
   thread Z=X+4 end
end

local X Y Z in
   thread X={ByNeed fun {$} 3 end} end
   thread X=Y end
   thread if X==Y then Z=10 end end
end

local Generate Sum Xs S in
   fun lazy {Generate N}
      N|{Generate N+1}
   end
   fun {Sum Xs A Limit}
      if Limit>0 then
         case Xs of X|Xr then
            {Sum Xr A+X Limit-1}
         end
      else A end
   end
   Xs={Generate 0}
   % Producer
   S={Sum Xs 0 50} % Consumer, Book has 150000
   {Browse S}
end

local Generate Sum Xs S1 S2 S3 in
   fun lazy {Generate N}
      N|{Generate N+1}
   end
   fun {Sum Xs A Limit}
      if Limit>0 then
         case Xs of X|Xr then
            {Sum Xr A+X Limit-1}
         end
      else A end
   end
   Xs={Generate 0}
   thread S1={Sum Xs 0 30} end  % Book has 150000
   thread S2={Sum Xs 0 20} end  % Book has 100000
   thread S3={Sum Xs 0 10} end   % Book has 50000
end

local Drop Buffer1 Buffer2 in
   fun {Drop Xs N}   % Implements List.drop
      if N =< 0 then Xs
      else
         case Xs
         of nil then nil
         [] _|Xr then {Drop Xr N-1}
         end
      end
   end

   fun {Buffer1 In N}
      End={Drop In N}
      fun lazy {Loop In End}
         case In of I|In2 then
            I|{Loop In2 End.2}
         end
      end
   in
      {Loop In End}
   end

   fun {Buffer2 In N}
      End=thread {Drop In N} end
      fun lazy {Loop In End}
         case In of I|In2 then
            I|{Loop In2 thread End.2 end}
         end
      end
   in
      {Loop In End}
   end
end

local Times Merge H Touch in
   fun lazy {Times N H}
      case H of X|H2 then N*X|{Times N H2} end
   end

   fun lazy {Merge Xs Ys}
      case Xs#Ys of (X|Xr)#(Y|Yr) then
         if X<Y then X|{Merge Xr Ys}
         elseif X>Y then Y|{Merge Xs Yr}
         else X|{Merge Xr Yr}
         end
      end
   end

   H=1|{Merge {Times 2 H}
       {Merge {Times 3 H}
       {Times 5 H}}}
   {Browse H}

   proc {Touch N H}
      if N>0 then {Touch N-1 H.2} else skip end
   end

   {Touch 20 H}
end

local LAppend MakeLazy LMap LFrom LFlatten LReverse L LFilter in
   fun lazy {LAppend As Bs}
      case As
      of nil then Bs
      [] A|Ar then A|{LAppend Ar Bs}
      end
   end

   fun lazy {MakeLazy Ls}
      case Ls
      of X|Lr then X|{MakeLazy Lr}
      else nil end
   end

   fun lazy {LMap Xs F}
      case Xs
      of nil then nil
      [] X|Xr then {F X}|{LMap Xr F}
      end
   end

   fun {LFrom I J}
      fun lazy {LFromLoop I}
         if I>J then nil else I|{LFromLoop I+1} end
      end
      fun lazy {LFromInf I} I|{LFromInf I+1} end
   in
      if J==inf then {LFromInf I} else {LFromLoop I} end
   end

   fun {LFlatten Xs}
      fun lazy {LFlattenD Xs E}
         case Xs
         of nil then E
         [] X|Xr then
            {LFlattenD X {LFlattenD Xr E}}
         [] X then X|E
         end
      end
   in
      {LFlattenD Xs nil}
   end

   fun {LReverse S}
      fun lazy {Rev S R}
         case S
         of nil then R
         [] X|S2 then {Rev S2 X|R} end
      end
   in {Rev S nil} end
   L={LReverse [a b c]}
   {Browse L}

   fun lazy {LFilter L F}
      case L
      of nil then nil
      [] X|L2 then
         if {F X} then X|{LFilter L2 F} else {LFilter L2 F} end
      end
   end
end

local NewQueue Check Insert Delete LAppend Reverse in
   fun {NewQueue} q(0 nil 0 nil) end
   fun {Check Q}
      case Q of q(LenF F LenR R) then
         if LenF>=LenR then Q
         else q(LenF+LenR {LAppend F {Reverse R}} 0 nil) end
      end
   end
   fun {Insert Q X}
      case Q of q(LenF F LenR R) then
         {Check q(LenF F LenR+1 X|R)}
      end
   end
   fun {Delete Q X}
      case Q of q(LenF F LenR R) then F1 in
         F=X|F1 {Check q(LenF-1 F1 LenR R)}
      end
   end

   % copied from above
   fun lazy {LAppend As Bs}
      case As
      of nil then Bs
      [] A|Ar then A|{LAppend Ar Bs}
      end
   end
   fun {Reverse R}
      fun {Rev R A}
         case R
         of nil then A
         [] X|R2 then {Rev R2 X|A} end
      end
   in {Rev R nil} end
end

local Reverse in
   fun {Reverse R}
      fun {Rev R A}
         case R
         of nil then A
         [] X|R2 then {Rev R2 X|A} end
      end
   in {Rev R nil} end
end

local LAppend LAppRev Check in
   fun lazy {LAppend F B}
      case F
      of nil then B
      [] X|F2 then X|{LAppend F2 B}
      end
   end

   fun lazy {LAppRev F R B}
      case F#R
      of nil#[Y] then Y|B
      [] (X|F2)#(Y|R2) then X|{LAppRev F2 R2 Y|B}
      end
   end

   fun {Check Q}
      case Q of q(LenF F LenR R) then
         if LenR=<LenF then Q
         else q(LenF+LenR {LAppRev F R nil} 0 nil) end
      end
   end
end
