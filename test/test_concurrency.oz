% Test Suite: Concurrency
% Covers: Threads, ByNeed (Lazy)

local X Y Lz Force in
   % 1. Threads
   thread
      {Browse 'Thread 1 Start'}
      X = 100
   end
   
   thread
      {Browse 'Thread 2 Start'}
      Y = 200
   end
   
   % Wait for binding (dataflow)
   {Browse X + Y} % 300

   % 2. ByNeed (Lazy) - Enabled to test deadlock detection loop
   {ByNeed fun {$} 42 end Lz} 
   Force = Lz + 10
   {Browse Force} % 52

   % 3. Lazy Function Syntax
   local F LazyVal in
      fun lazy {F X} X * 10 end
      LazyVal = {F 5}
      {Browse 'Lazy Created'}
      {Browse LazyVal + 0} % Force it (50)
   end

   % 4. ReadOnly Resumption
   local A B in
      B = !!A
      thread 
         {Browse 'Waiting for A'}
         {Browse B + 10} % Should suspend until A is bound
      end
      A = 5
   end
end
