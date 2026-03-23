% Test Suite: Exceptions
% Covers: Try/Catch/Raise

local E in
  % 1. Basic Try/Catch
  try
    {Browse 'Try 1'}
    raise oops end
    {Browse 'FAIL 1'}
  catch X then
    {Browse 'Caught 1'}
    {Browse X} % oops
  end

  % 2. Nested Try/Catch with Re-raise
  try
    try 
       raise inner end
    catch I then
       {Browse 'Caught Inner'}
       raise outer end
    end
  catch O then
    {Browse 'Caught Outer'} 
    {Browse O} % outer
  end
  
  % 3. Try/Finally pattern (simulated via elaboration in full syntax)
  % Note: Hoz parser supports try ... catch ... [finally ...]
  try
     {Browse 'Try Finally Body'}
  catch E then
     {Browse 'Catch in Finally'}
  finally
     {Browse 'Finally Block Executed'}
  end
end
