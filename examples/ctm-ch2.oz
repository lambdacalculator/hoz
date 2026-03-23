%%%%%%%% CTM Chapter 2: Declarative Computation Model

%%%% 2.3: Kernel language
local X Y Z T in
   X=5 Y=10
   T=(X>=Y)
   if T then Z=X else Z=Y end
end

local X Y Z in
   X=5 Y=10
   if X>=Y then Z=X else Z=Y end
end

%%%% 2.4: Kernel language semantics
local A B C D in
   A=11
   B=2
   C=A+B
   D=C*C
end

local X in
   X=1
   local X in
      X=2
      {Browse X}
   end
   {Browse X}
end

local Max in
   proc {Max X Y ?Z}
      if X>=Y then Z=X else Z=Y end
   end
end

local Y LB in
   Y=10
   proc {LB X ?Z}
      if X>=Y then Z=X else Z=Y end
   end
   % Y is defined and bound in the outer scope. LB captures this static scope.
   local Y=15 Z in
      {LB 5 Z}
      {Browse Z}
   end
end

local P Q in
   proc {Q X} {Browse stat(X)} end
   proc {P X} {Q X} end
   local Q in
      proc {Q X} {Browse dyn(X)} end
      {P hello}
   end
end

local Loop10 in
   proc {Loop10 I}
      if I==10 then skip
      else
         {Browse I}
         {Loop10 I+1}
      end
   end
   {Loop10 0}
end

%%%% 2.6: Exceptions
local Eval in
   fun {Eval E}
      if {IsNumber E} then E
      else
         case E of plus(X Y) then {Eval X}+{Eval Y}
         [] times(X Y) then {Eval X}*{Eval Y}
         else raise illFormedExpr(E) end
         end
      end
   end

   try
      {Browse {Eval plus(plus(5 5) 10)}}
      {Browse {Eval times(6 11)}}
      {Browse {Eval minus(7 10)}}
   catch illFormedExpr(E) then
      {Browse '*** Illegal expression '#E#' ***'}
   end
end
