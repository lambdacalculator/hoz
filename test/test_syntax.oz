% Test Suite: Syntax & Control Flow
% Covers: Skip, Local, Variable Binding, If/Else, Case, Procedures, Functions, Sugar

local X Y Z U V W in
  % 1. Directives & Skip
  skip
  %show full
  {Browse 'Syntax Test Start'}

  % 2. Variable Binding and Literals
  X = 10
  Y = 20
  Z = X
  {Browse X} % 10

  % 3. Arithmetic Expressions (Binary Ops)
  U = ((X + Y) * 2)
  {Browse U} % 60
  {Browse 10 div 3} % 3
  {Browse 10 mod 3} % 1

  % 4. Conditional Statements
  if X < Y then
     {Browse 'If < OK'}
  else
     {Browse 'If < FAIL'}
  end

  if X > Y then
     {Browse 'If > FAIL'}
  elseif X =< Y then % Test =< sugar
     {Browse 'If <= OK'}
  end

  if X >= Y then
     {Browse 'If >= FAIL'}
  end

  if X \= Y then 
     {Browse 'If != OK'}
  end

  if X == Y then
     {Browse 'If == FAIL'}
  else
     {Browse 'Else OK'} 
  end

  % 5. Case Statements
  V = rec(a:1 b:2)
  case V of rec(a:A b:B) then
     {Browse A} % 1
  else
     {Browse 'Case FAIL'}
  end
  
  % Nested Case
  local L in
      L = [10 20]
      case L of H|T then 
         {Browse H} % 10
      end 
  end

  % 6. Procedures and Functions
  local P F R in
      proc {P A ?Res}
         Res = A * 2
      end
      {P 10 R}
      {Browse R} % 20
      
      fun {F A}
         A + 1
      end
      {Browse {F 10}} % 11
  end

  % 7. Anonymous Proc/Fun
  local P2 F2 VRes in
     P2 = proc {$ A ?Res} Res = A + A end
     {P2 5 VRes}
     {Browse VRes} % 10

     F2 = fun {$ A} A * A end
     {Browse {F2 5}} % 25
  end

  % 8. Short-circuit Boolean Logic
  if true orelse {Browse 'OrElse FAIL'} then {Browse 'OrElse OK'} end
  if false andthen {Browse 'AndThen FAIL'} then skip else {Browse 'AndThen OK'} end

end
