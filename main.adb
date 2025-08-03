-- this is the executable file 
-- running the life support of a submarine
--
-- it initialises the system
-- then runs for ever in a loop which
--   1) reads the temperature from console
--   2) monitors the cooling system so that the cooling
--      system is activated if the temperature is too high
--   3) prints out the status
-- 
--  the loop_invariant expresses that the system stays safe all the time.

pragma SPARK_Mode (On);

with driver;  use driver;




procedure Main is
 
begin
   Init;
   loop
      pragma Loop_Invariant (Is_Safe(System_State));
      Driver.TakeValues;
      pragma Assert (System_State.Pressure > 0.0);
      
      loop 
         
         exit when System_State.SafeMaybe = Safe;

         Driver.DisplayPartialPressures;

         Driver.changeGasRatio;
         Driver.CalcPartialPressures;
         Driver.EvalPartialPressures;
         
      end loop; 
      Driver.CalcPartialPressures;
      Driver.DisplayFinalPP;

   end loop;
end Main;


