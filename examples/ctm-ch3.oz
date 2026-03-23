%%%%%%%% CTM Chapter 3: Declarative Programming Techniques

%%%% 3.2: Iterative computation

% Sqrt 1: Intial implementation
local Sqrt SqrtIter Improve GoodEnough Abs in
   fun {Sqrt X}
      local Guess=1.0 in
         {SqrtIter Guess X}
      end
   end
   fun {SqrtIter Guess X}
      if {GoodEnough Guess X} then Guess
      else {SqrtIter {Improve Guess X} X} end
   end
   fun {Improve Guess X}
      (Guess + X/Guess) / 2.0
   end
   fun {GoodEnough Guess X}
      {Abs X-Guess*Guess}/X < 0.00001
   end
   fun {Abs X} if X<0.0 then ~X else X end end

   {Browse {Sqrt 2.0}}
end

% Sqrt 2: Local block style (from text)
local
   fun {Abs X} if X<0.0 then ~X else X end end
   fun {Improve Guess X}
      (Guess + X/Guess) / 2.0
   end
   fun {GoodEnough Guess X}
      {Abs X-Guess*Guess}/X < 0.00001
   end
   fun {SqrtIter Guess X}
      if {GoodEnough Guess X} then Guess
      else {SqrtIter {Improve Guess X} X} end
   end
   fun {Sqrt X}
      local Guess=1.0 in
         {SqrtIter Guess X}
      end
   end
in
   {Browse {Sqrt 2.0}}
end

% Sqrt 3: Nested functions
local Sqrt in
   fun {Sqrt X}
      fun {Improve Guess X}
         (Guess + X/Guess) / 2.0
      end
      fun {Abs X} if X<0.0 then ~X else X end end
      fun {GoodEnough Guess X}
         {Abs X-Guess*Guess}/X < 0.00001
      end
      fun {SqrtIter Guess X}
         if {GoodEnough Guess X} then Guess
         else {SqrtIter {Improve Guess X} X} end
      end
      Guess=1.0
   in
      {SqrtIter Guess X}
   end
   {Browse {Sqrt 2.0}}
end

% Sqrt 4: Lexical scoping of X
local Sqrt in
   fun {Sqrt X}
      fun {Improve Guess}
         (Guess + X/Guess) / 2.0
      end
      fun {Abs X} if X<0.0 then ~X else X end end
      fun {GoodEnough Guess}
         {Abs X-Guess*Guess}/X < 0.00001
      end
      fun {SqrtIter Guess}
         if {GoodEnough Guess} then Guess
         else {SqrtIter {Improve Guess}} end
      end
      Guess=1.0
   in
      {SqrtIter Guess}
   end
   {Browse {Sqrt 2.0}}
end

% Sqrt 5: More lexical scoping
local Sqrt in
   fun {Sqrt X}
      fun {Improve Guess}
         (Guess + X/Guess) / 2.0
      end
      fun {Abs X} if X<0.0 then ~X else X end end
      fun {GoodEnough Guess}
         {Abs X-Guess*Guess}/X < 0.00001
      end
      fun {SqrtIter Guess}
         if {GoodEnough Guess} then Guess
         else {SqrtIter {Improve Guess}} end
      end
      Guess=1.0
   in
      {SqrtIter Guess}
   end
   {Browse {Sqrt 2.0}}
end

%%%% 3.3: Recursive Computation
local Fact in
   fun {Fact N}
      if N==0 then 1
      elseif N>0 then N*{Fact N-1}
      else raise domainError end end
   end
   {Browse {Fact 5}}
end

local Fact in
   fun {Fact N}
      local
         fun {FactIter N A}
            if N==0 then A
            elseif N>0 then {FactIter N-1 A*N}
            else raise domainError end end
         end
      in
         {FactIter N 1}
      end
   end
   {Browse {Fact 6}}
end

%%%% 3.4: Programming with recursion
local Length Append Nth SumList Reverse IterLength Merge Split in
   fun {Length Ls}
      case Ls of nil then 0
      [] _|Lr then 1+{Length Lr}
      end
   end
   {Browse {Length [a b c]}}

   fun {Append Ls Ms}
      case Ls of nil then Ms
      [] X|Lr then X|{Append Lr Ms}
      end
   end
   {Browse {Append [a b c] [d e f]}}

   fun {Nth Xs N}
      if N==1 then Xs.1
      elseif N>1 then {Nth Xs.2 N-1}
      end
   end
   {Browse {Nth [a b c d] 4}}

   fun {SumList Xs}
      case Xs of nil then 0
      [] X|Xr then X+{SumList Xr}
      end
   end
   {Browse {SumList [1 2 3]}}

   fun {Reverse Xs}
      case Xs of nil then nil
      [] X|Xr then
         {Append {Reverse Xr} [X]}
      end
   end
   {Browse {Reverse [a b c]}}

   % Iterative Length
   local Length
      fun {IterLength I Ys}
         case Ys of nil then I
         [] _|Yr then {IterLength I+1 Yr}
         end
      end
   in
      fun {Length Xs} {IterLength 0 Xs} end
      {Browse {Length [1 2 3]}}
   end

   % Iterative Reverse
   local Reverse
      fun {IterReverse Rs Ys}
         case Ys
         of nil then Rs
         [] Y|Yr then {IterReverse Y|Rs Yr}
         end
      end
   in
      fun {Reverse Xs}
         {IterReverse nil Xs}
      end
      {Browse {Reverse [a b c]}}
   end


   % Merge Sort
   fun {Merge Xs Ys}
      case Xs # Ys
      of nil # Ys then Ys
      [] Xs # nil then Xs
      [] (X|Xr) # (Y|Yr) then
         if X<Y then X|{Merge Xr Ys}
         else Y|{Merge Xs Yr} end
      end
   end

   proc {Split Xs ?Ys ?Zs}
      case Xs
      of nil then Ys=nil Zs=nil
      [] [X] then Ys=[X] Zs=nil
      [] X1|X2|Xr then Yr Zr in
         Ys=X1|Yr
         Zs=X2|Zr
         {Split Xr Yr Zr}
      end
   end

   local MergeSort in
      fun {MergeSort Xs}
         case Xs
         of nil then nil
         [] [X] then [X]
         else Ys Zs in
            {Split Xs Ys Zs}
            {Merge {MergeSort Ys} {MergeSort Zs}}
         end
      end
      {Browse {MergeSort [3 1 4 1 5 9 2 6 5 3 5]}}
   end

   % Bottom-Up Merge Sort
   local MergeSort in
      fun {MergeSort Xs}
         fun {MergeSortAcc L1 N}
            if N==0 then nil # L1
            elseif N==1 then [L1.1] # L1.2
            elseif N>1 then
               NL=N div 2
               NR=N-NL
               Ys # L2 = {MergeSortAcc L1 NL}
               Zs # L3 = {MergeSortAcc L2 NR}
            in
               {Merge Ys Zs} # L3
            end
         end
      in
         {MergeSortAcc Xs {Length Xs}}.1
      end
      {Browse {MergeSort [3 1 4 1 5 9 2 6 5 3 5]}}
   end

   % Difference Lists
   local AppendD Flatten FlattenD IsList in
      % Copied from above

      % Check top level for list constructor
      fun {IsList L}
         case L of nil then true
         [] _|_ then true
         else false end
      end

      fun {AppendD D1 D2}
         S1#E1=D1
         S2#E2=D2
      in
         E1=S2
         S1#E2
      end

      % 1. Naive Flatten
      fun {Flatten Xs}
         case Xs of nil then nil
         [] X|Xr andthen {IsList X} then
            {Append {Flatten X} {Flatten Xr}}
         [] X|Xr then X|{Flatten Xr}
         end
      end
      {Browse {Flatten [[a b] [[c] [d]] nil [e [f]]]}}

      % 2. Flatten with Difference Lists (Tuples)
      local Flatten in
         proc {FlattenD Xs ?Ds}
            case Xs of nil then Y in Ds=Y#Y
            [] X|Xr andthen {IsList X} then Y1 Y2 Y4 in
               Ds=Y1#Y4
               {FlattenD X Y1#Y2}
               {FlattenD Xr Y2#Y4}
            [] X|Xr then Y1 Y2 in
               Ds=(X|Y1)#Y2
               {FlattenD Xr Y1#Y2}
            end
         end
         fun {Flatten Xs} Ys in {FlattenD Xs Ys#nil} Ys end
         {Browse {Flatten [[a b] [[c] [d]] nil [e [f]]]}}
      end

      % 3. Flatten with Difference Lists (Split)
      local Flatten in
         fun {Flatten Xs}
            proc {FlattenD Xs ?S E}
               case Xs
               of nil then S=E
               [] X|Xr andthen {IsList X} then Y2 in
                  {FlattenD X S Y2}
                  {FlattenD Xr Y2 E}
               [] X|Xr then Y1 in
                  S=X|Y1
                  {FlattenD Xr Y1 E}
               end
            end Ys
         in
            {FlattenD Xs Ys nil} Ys
         end
         {Browse {Flatten [[a b] [[c] [d]] nil [e [f]]]}}
      end

      % 4. Flatten with FlattenD Function and Accumulator
      local Flatten in
         fun {Flatten Xs}
            fun {FlattenD Xs E}
               case Xs
               of nil then E
               [] X|Xr andthen {IsList X} then
                  {FlattenD X {FlattenD Xr E}}
               [] X|Xr then
                  X|{FlattenD Xr E}
               end
            end
         in
            {FlattenD Xs nil}
         end
         {Browse {Flatten [[a b] [[c] [d]] nil [e [f]]]}}
      end
   end

   % Queues (Amortized and Worst-Case)
   local NewQueue Check Insert Delete IsEmpty BFS BFSQueue in
      fun {NewQueue} q(nil nil) end
      fun {Check Q}
         case Q of q(nil R) then q({Reverse R} nil) else Q end
      end
      fun {Insert Q X}
         case Q of q(F R) then {Check q(F X|R)} end
      end
      fun {Delete Q X}
         case Q of q(F R) then F1 in F=X|F1 {Check q(F1 R)} end
      end
      fun {IsEmpty Q}
         case Q of q(F R) then F==nil end
      end

      % Test Queue
      local Q1 Q2 Q3 Q4 Q5 Q6 Q7 in
         Q1={NewQueue}
         Q2={Insert Q1 peter}
         Q3={Insert Q2 paul}
         local X in Q4={Delete Q3 X} {Browse X} end
         Q5={Insert Q4 mary}
         local X in Q6={Delete Q5 X} {Browse X} end
         local X in Q7={Delete Q6 X} {Browse X} end
      end

      % Constant Time Queue (Banker's Queue? No, this is ID List?)
      local NewQueue Insert Delete IsEmpty in
         fun {NewQueue} X in q(0 X X) end
         fun {Insert Q X}
            case Q of q(N S E) then E1 in E=X|E1 q(N+1 S E1) end
         end
         fun {Delete Q X}
            case Q of q(N S E) then S1 in S=X|S1 q(N-1 S1 E) end
         end
         fun {IsEmpty Q}
            case Q of q(N S E) then N==0 end
         end
      end

      local BFSQueue in
         proc {BFS T}
            fun {TreeInsert Q T}
               if T\=leaf then {Insert Q T} else Q end
            end
            proc {BFSQueue Q1}
               if {IsEmpty Q1} then skip
               else X Q2={Delete Q1 X} tree(Key Val L R)=X in
                  {Browse Key#Val}
                  {BFSQueue {TreeInsert {TreeInsert Q2 L} R}}
               end
            end
         in
            {BFSQueue {TreeInsert {NewQueue} T}}
         end
      end

      % Trees: Lookup, Insert, Delete, DFS, BFS
      local Lookup Insert Delete RemoveSmallest DFS DFSAcc BFSAcc in
         fun {Lookup X T}
            case T of leaf then notfound
            [] tree(Y V T1 T2) then
               if X<Y then {Lookup X T1}
               elseif X>Y then {Lookup X T2}
               else found(V) end
            end
         end


         fun {Insert X V T}
            case T of leaf then tree(X V leaf leaf)
            [] tree(Y W T1 T2) andthen X==Y then tree(X V T1 T2)
            [] tree(Y W T1 T2) andthen X<Y then tree(Y W {Insert X V T1} T2)
            [] tree(Y W T1 T2) andthen X>Y then tree(Y W T1 {Insert X V T2})
            end
         end

         fun {Delete X T}
            case T of leaf then leaf
            [] tree(Y W T1 T2) andthen X==Y then
               case {RemoveSmallest T2}
               of none then T1
               [] Yp#Vp#Tp then tree(Yp Vp T1 Tp)
               end
            [] tree(Y W T1 T2) andthen X<Y then tree(Y W {Delete X T1} T2)
            [] tree(Y W T1 T2) andthen X>Y then tree(Y W T1 {Delete X T2})
            end
         end

         fun {RemoveSmallest T}
            case T of leaf then none
            [] tree(Y V T1 T2) then
               case {RemoveSmallest T1}
               of none then Y#V#T2
               [] Yp#Vp#Tp then Yp#Vp#tree(Y V Tp T2)
               end
            end
         end

         proc {DFS T}
            case T of leaf then skip
            [] tree(Key Val L R) then
               {DFS L} {Browse Key#Val} {DFS R}
            end
         end

         local T1 T2 T3 T4 in
            T1={Insert 10 ten leaf}
            T2={Insert 5 five T1}
            T3={Insert 15 fifteen T2}
            T4={Insert 2 two T3}
            {BFS T4}
         end
      end
   end
end

%%%% 3.6: Higher-order programming
local Sqrt QuadraticEquation RS X1 X2 in
   % Define Sqrt used here
   fun {Sqrt X} X end % Mock

   local A=1.0 B=3.0 C=2.0 D RealSol X1 X2 in
      D=B*B-4.0*A*C
      if D>=0.0 then
         RealSol=true
         X1=(~B+{Sqrt D})/(2.0*A)
         X2=(~B-{Sqrt D})/(2.0*A)
      else
         RealSol=false
         X1=(~B)/(2.0*A)
         X2={Sqrt ~D}/(2.0*A)
      end
      {Browse RealSol#X1#X2}
   end

   proc {QuadraticEquation A B C ?RealSol ?X1 ?X2}
      local D=B*B-4.0*A*C in
         if D>=0.0 then
            RealSol=true
            X1=(~B+{Sqrt D})/(2.0*A)
            X2=(~B-{Sqrt D})/(2.0*A)
         else
            RealSol=false
            X1=(~B)/(2.0*A)
            X2={Sqrt ~D}/(2.0*A)
         end
      end
   end

   {QuadraticEquation 1.0 3.0 2.0 RS X1 X2}
   {Browse RS#X1#X2}
end

local SumList FoldR ProductList Some GenericMergeSort FoldL Map Filter in
   fun {FoldR L F U}
      case L of nil then U
      [] X|L1 then {F X {FoldR L1 F U}}
      end
   end

   fun {SumList L}
      {FoldR L fun {$ X Y} X+Y end 0}
   end

   fun {ProductList L}
      {FoldR L fun {$ X Y} X*Y end 1}
   end

   fun {Some L}
      {FoldR L fun {$ X Y} X orelse Y end false}
   end

   fun {GenericMergeSort F Xs}
      fun {Merge Xs Ys}
         case Xs # Ys
         of nil # Ys then Ys
         [] Xs # nil then Xs
         [] (X|Xr) # (Y|Yr) then
            if {F X Y} then X|{Merge Xr Ys}
            else Y|{Merge Xs Yr} end
         end
      end
      proc {Split Xs ?Ys ?Zs}
         case Xs
         of nil then Ys=nil Zs=nil
         [] [X] then Ys=[X] Zs=nil
         [] X1|X2|Xr then Yr Zr in
            Ys=X1|Yr
            Zs=X2|Zr
            {Split Xr Yr Zr}
         end
      end
      fun {MergeSort Xs}
         case Xs of nil then nil [] [X] then [X]
         else Ys Zs in {Split Xs Ys Zs} {Merge {MergeSort Ys} {MergeSort Zs}} end
      end
   in
      {MergeSort Xs}
   end

   fun {FoldL L F U}
      case L of nil then U
      [] X|L2 then {FoldL L2 F {F U X}}
      end
   end

   fun {Map Xs F}
      case Xs of nil then nil
      [] X|Xr then {F X}|{Map Xr F}
      end
   end

   fun {Filter Xs F}
      case Xs of nil then nil
      [] X|Xr andthen {F X} then X|{Filter Xr F}
      [] X|Xr then {Filter Xr F}
      end
   end
end

%%%% 3.7: Abstract data types
local NewDictionary Put CondGet Domain Map in
   fun {NewDictionary} nil end
   fun {Put Ds Key Value}
      case Ds of nil then [Key#Value]
      [] (K#V)|Dr andthen Key==K then (Key#Value) | Dr
      [] (K#V)|Dr andthen K>Key then (Key#Value)|(K#V)|Dr
      [] (K#V)|Dr andthen K<Key then (K#V)|{Put Dr Key Value}
      end
   end
   fun {CondGet Ds Key Default}
      case Ds of nil then Default
      [] (K#V)|Dr andthen Key==K then V
      [] (K#V)|Dr andthen K>Key then Default
      [] (K#V)|Dr andthen K<Key then {CondGet Dr Key Default}
      end
   end
   fun {Map Xs F}
      case Xs of nil then nil
      [] X|Xr then {F X}|{Map Xr F}
      end
   end
   fun {Domain Ds}
      {Map Ds fun {$ K#_} K end} % Requires Map
   end
end

local NewWrapper Wrap Unwrap NewStack Push Pop IsEmpty in
   proc {NewWrapper ?Wrap ?Unwrap}
      local Key={NewName} in
         fun {Wrap X} fun {$ K} if K==Key then X end end end
         fun {Unwrap W} {W Key} end
      end
   end

   {NewWrapper Wrap Unwrap}

   fun {NewStack} {Wrap nil} end
   fun {Push S E} {Wrap E|{Unwrap S}} end
   fun {Pop S E}
      case {Unwrap S} of X|S1 then E=X {Wrap S1} end
   end
   fun {IsEmpty S} {Unwrap S}==nil end
end
