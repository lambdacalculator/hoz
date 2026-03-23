%%%%%%%% CTM Chapter 1: Introduction to Programming Concepts

%%%% 1.1 - 1.3: Basic Arithmetic and Functions
local V Fact Comb in
   {Browse 9999*9999}

   V = 9999*9999
   {Browse V*V}

   {Browse 1*2*3*4*5*6*7*8*9*10}

   fun {Fact N}
      if N==0 then 1 else N*{Fact N-1} end
   end

   {Browse {Fact 10}}
   {Browse {Fact 100}}

   fun {Comb N R}
      {Fact N} div ({Fact R}*{Fact N-R})
   end

   {Browse {Comb 10 3}}
end

%%%% 1.4: Lists
local H T L in
   {Browse [5 6 7 8]}

   H=5
   T=[6 7 8]
   {Browse H|T}

   L=[5 6 7 8]
   {Browse L.1}
   {Browse L.2}

   case L of H1|T1 then {Browse H1} {Browse T1} end
end

%%%% 1.5 - End: Pascal's Triangle and Scoped Helpers
local ShiftLeft ShiftRight AddList Pascal FastPascal Ints PascalList PascalList2 GenericPascal OpList Add Xor in

   % Helper functions shared across sections
   fun {ShiftLeft L}
      case L of H|T then
         H|{ShiftLeft T}
      else [0] end
   end

   fun {ShiftRight L} 0|L end

   fun {AddList L1 L2}
      case L1 of H1|T1 then
         case L2 of H2|T2 then
            H1+H2|{AddList T1 T2}
         end
      else nil end
   end

   % 1.5: Basic Pascal
   fun {Pascal N}
      if N==1 then [1]
      else
         {AddList {ShiftLeft {Pascal N-1}}
         {ShiftRight {Pascal N-1}}}
      end
   end
   % {Browse {Pascal 20}}  % Calculated faster below

   % 1.7: Fast Pascal
   local FastPascal in
      fun {FastPascal N}
         if N==1 then [1]
         else L in
            L={FastPascal N-1}
            {AddList {ShiftLeft L} {ShiftRight L}}
         end
      end
      {Browse {FastPascal 20}}  % Generates the missing {Pascal 20} output above
      {Browse {FastPascal 30}}
   end

   % 1.8: Lazy Lists
   local Ints PascalList PascalList2 L in
      fun lazy {Ints N}
         N|{Ints N+1}
      end

      L={Ints 0}
      {Browse L}
      {Browse L.1}

      case L of A|B|C|_ then {Browse A+B+C} end

      fun lazy {PascalList Row}
         Row|{PascalList
         {AddList {ShiftLeft Row}
         {ShiftRight Row}}}
      end

      local L2 in
         L2={PascalList [1]}
         {Browse L2}
         {Browse L2.1}
         {Browse L2.2.1}
      end

      fun {PascalList2 N Row}
         if N==1 then [Row]
         else
            Row|{PascalList2 N-1
            {AddList {ShiftLeft Row}
            {ShiftRight Row}}}
         end
      end
      {Browse {PascalList2 10 [1]}}
   end

   % 1.9: Higher-Order Programming
   local GenericPascal OpList Add FastPascal Xor in
      fun {GenericPascal Op N}
         if N==1 then [1]
         else L in
            L={GenericPascal Op N-1}
            {OpList Op {ShiftLeft L} {ShiftRight L}}
         end
      end

      fun {OpList Op L1 L2}
         case L1 of H1|T1 then
            case L2 of H2|T2 then
               {Op H1 H2}|{OpList Op T1 T2}
            end
         else nil end
      end

      fun {Add X Y} X+Y end

      {Browse {GenericPascal Add 5}}

      % Redefining FastPascal using Generic
      fun {FastPascal N} {GenericPascal Add N} end

      fun {Xor X Y} if X==Y then 0 else 1 end end

      {Browse {GenericPascal Xor 6}}
   end

   % 1.10: Threaded Pascal
   thread P in
      P={Pascal 10}  % CTM has 30
      {Browse P}
   end
   {Browse 99*99}


   % 1.12: Cells
   local C in
      C={NewCell 0}
      C:=@C+1
      {Browse @C}
   end

   local C FastPascal GenericPascal OpList Add in
      % Dependencies from 1.9 needed here locally
      fun {GenericPascal Op N}
         if N==1 then [1]
         else L in
            L={GenericPascal Op N-1}
            {OpList Op {ShiftLeft L} {ShiftRight L}}
         end
      end

      fun {OpList Op L1 L2}
         case L1 of H1|T1 then
            case L2 of H2|T2 then
               {Op H1 H2}|{OpList Op T1 T2}
            end
         else nil end
      end

      fun {Add X Y} X+Y end

      C={NewCell 0}
      fun {FastPascal N}
         C:=@C+1
         {GenericPascal Add N}
      end

      % Verify function works and cell updates
      {Browse {FastPascal 5}}
      {Browse @C}
   end
end
