local
   TestIf
in
   {Browse 'Test 1: Basic ByNeed'}
   local X Y Z in
      X={ByNeed fun {$} 3 end}
      Y={ByNeed fun {$} 4 end}
      Z=X+Y 
      {Browse z(Z)}
   end

   {Browse 'Test 2: Multiple triggers (X=Y aliasing)'}
   local X Y Z in
      X={ByNeed fun {$} 3 end}
      Y={ByNeed fun {$} 3 end}
      X=Y
      Z=X+1 
      {Browse z_aliased(Z)}
   end

   {Browse 'Test 4: Triggered by If'}
   local X Y in
      X={ByNeed fun {$} true end}
      if X then Y=found_true else Y=found_false end 
      {Browse y(Y)}
   end
end
