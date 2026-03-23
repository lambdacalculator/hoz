local Clock C in
   fun {Clock}
      fun {Loop B}
         {Delay 100} B|{Loop B}
      end
   in
      thread {Loop 1} end
   end

   C = {Clock}
   {Browse C}
end
