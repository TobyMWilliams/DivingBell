pragma SPARK_Mode(On);


with AS_Io_Wrapper;use  AS_Io_Wrapper;
--with Ada.Float_Text_IO; use Ada.Float_Text_IO;


--with SPARK.Text_IO; use SPARK.Text_IO;






package body Driver is



   function CalculatePressure (Depth : Integer) return Float is
      P : Float;
   begin

      P := Float(Depth) / 10.0 + 1.0;
      pragma Assert(P >= 2.0);
      -- pragma Assert(P <= Float'Last);
      return P;
   end CalculatePressure;



   procedure TakeValues is
      Depth          : Integer;
      P              : Float;
      PP             : PartialPressures;
      Gas_Fractions  : GasFractions;
      safer          : Safety;
   begin

      TakeDepth(Depth);
      pragma Assert(Depth >= 10 );

      P    := CalculatePressure(Depth);

      pragma Assert (P >= 2.0 );

      TakeF(Gas_Fractions);

      CalculatePartialPressures(Gas_Fractions, P, PP);
      EvaluatePartialPressures(PP, safer);

      System_State := (pressure            =>  P,
                       Partial_Pressures   => PP,
                       Gas_Fraction        => Gas_Fractions,
                       SafeMaybe           =>  Safer);
      pragma Assert (System_State.pressure > 0.0 );

   end TakeValues;






   procedure TakeDepth (Depth : out Integer)  is
      temp  : Integer;
   begin
      AS_Put_Line("Enter a value for Depth in metres (10 <= X <= 701): ");

      loop
         AS_Get(temp);

         exit when ( temp >=10) and (temp <= 701);
         AS_Put("please enter a value between 0 and 701");
         AS_Put_Line(" ");
      end loop;
      Depth := temp;
      pragma Assert(Depth >- 10) ;

   end TakeDepth;



   procedure TakeF (Gas_Fractions : out GasFractions) is
      FractionO2, FractionCO2, FractionHe : Integer;
      notGas_Fractions                    : notGasFractions;

   begin
      AS_Put_Line("Enter values for gas fractions:");
      loop
         AS_Put_Line("Enter a value (>= 0): ");
         AS_Put_Line("Enter FO2 value:");
         TakeFraction(FractionO2);

         AS_Put_Line("Enter a value (>= 0): ");
         AS_Put_Line("Enter FCO2 value:");
         TakeFraction(FractionCO2);

         AS_Put_Line("Enter a value (>= 0): ");
         AS_Put_Line("Enter FHe value:");
         TakeFraction(FractionHe);



         --pragma Assert(total < Integer'Last);

         --exiting when there is a value input for one of the gases ( the cabin isn't a vaccuum)
         exit when FractionO2 > 0 or FractionCO2 > 0 or FractionHe > 0;
         AS_Put_Line("Sum of gas fractions must be positive.");
      end loop;
      notGas_Fractions := (FractionO2, FractionCO2, FractionHe);
      Gas_Fractions    := FloatGasFractions(notGas_Fractions);
      pragma Assert(Gas_Fractions.fO2 + Gas_Fractions.fCO2 + Gas_Fractions.fHe <= Float'Last) ;
      pragma Assert(Gas_Fractions.fO2  >= 0.0);
      pragma Assert(Gas_Fractions.fCO2 >= 0.0);
      pragma Assert(Gas_Fractions.fHe  >= 0.0);


   end TakeF;


   --helper function for takeF
   procedure TakeFraction(Fraction : out Integer) is
   begin
      loop
         AS_Get(Fraction);
         exit when Fraction >= 0;

      end loop;

      pragma Assert(Fraction >= 0);
   end TakeFraction;










   procedure CalculatePartialPressures (GF : in GasFractions ; pressure : in Float ; PP : out PartialPressures) is
      TotalPP    : Float;
      Correction : Float;
   begin

      pragma Assert (pressure >= 2.0);
      pragma Assert( GF.fO2 * pressure <= Float'Last) ;
      pragma Assert( GF.fCO2 * pressure <= Float'Last) ;
      pragma Assert( GF.fHe * pressure <= Float'Last) ;




      PP.ppO2  := GF.fO2  * pressure;
      PP.ppCO2 := GF.fCO2 * pressure;
      PP.ppHe  := GF.fHe  * pressure;



      pragma assert (PP.ppO2  > 0.0);
      pragma assert (PP.ppCO2  > 0.0);
      pragma assert (PP.ppHe  > 0.0);

      TotalPP := PP.ppO2 + PP.ppCO2 + PP.ppHe;

      if TotalPP /= Pressure then
         Correction   := pressure / TotalPP;
         PP.ppO2      := PP.ppO2  * Correction;
         PP.ppCO2     := PP.ppCO2 * Correction;
         PP.ppHe      := PP.ppHe  * Correction;
      end if;


   end CalculatePartialPressures;


   procedure CalcPartialPressures is
      TotalPP    : Float;
      Correction : Float;
   begin

      pragma Assert (System_State.pressure > 2.0);

      System_State.Partial_Pressures.ppO2  := System_State.Gas_Fraction.fO2  * System_State.pressure;
      System_State.Partial_Pressures.ppCO2 := System_State.Gas_Fraction.fCO2 * System_State.pressure;
      System_State.Partial_Pressures.ppHe  := System_State.Gas_Fraction.fHe  * System_State.pressure;

      TotalPP := System_State.Partial_Pressures.ppO2 + System_State.Partial_Pressures.ppCO2 + System_State.Partial_Pressures.ppHe;
      pragma Assert (TotalPP > 0.0);
      if TotalPP /= System_State.Pressure then
         Correction   := (System_State.pressure) / TotalPP;
         System_State.Partial_Pressures.ppO2  := System_State.Partial_Pressures.ppO2  * Correction;
         System_State.Partial_Pressures.ppCO2 := System_State.Partial_Pressures.ppCO2 * Correction;
         System_State.Partial_Pressures.ppHe  := System_State.Partial_Pressures.ppHe  * Correction;
      end if;

   end CalcPartialPressures;




   procedure DisplayPartialPressures is
      Tolerance     : constant Float := 1.0E-6;

   begin
      pragma Assert(abs(System_State.Partial_Pressures.ppO2 + System_State.Partial_Pressures.ppCO2 + System_State.Partial_Pressures.ppHe - (System_State.pressure)) <= Tolerance,
                    "Partial pressures must sum to the total pressure within tolerance.");

      AS_Put("Total Pressure is: ");
      AS_Put_Line(Float'Image(System_State.pressure));

      AS_Put("sum of Partial Pressures: ");
      AS_Put_Line(Float'Image(System_State.Partial_Pressures.ppO2 + System_State.Partial_Pressures.ppCO2 + System_State.Partial_Pressures.ppHe));

      AS_Put("Partial Pressure of O2: ");
      AS_Put_Line(Float'Image(System_State.Partial_Pressures.ppO2));

      AS_Put("Partial Pressure of CO2: ");
      AS_Put_Line(Float'Image(System_State.Partial_Pressures.ppCO2));

      AS_Put("Partial Pressure of He: ");
      AS_Put_Line(Float'Image(System_State.Partial_Pressures.ppHe));
      AS_Put_Line(" ");


   end DisplayPartialPressures;


   procedure EvaluatePartialPressures( PP : in PartialPressures  ; safer : out Safety) is

   begin


      if not (pp.ppO2 < Float(safeO2'First) or pp.ppO2 > Float(safeO2'Last)) and not (pp.ppCO2 > Float(safeCO2'Last)) then
         AS_Put_Line("System is safe.");
         safer  := Safe;
      else
         safer  := Unsafe;
      end if;
   end EvaluatePartialPressures;


   procedure EvalPartialPressures is

   begin


      if not (System_State.Partial_Pressures.ppO2 < Float(safeO2'First) or System_State.Partial_Pressures.ppO2 > Float(safeO2'Last)) and not (System_State.Partial_Pressures.ppCO2 > Float(safeCO2'Last)) then
         AS_Put_Line("System is safe.");
         System_State.safeMaybe  := Safe;
      else
         System_State.safeMaybe  := Unsafe;
      end if;
   end EvalPartialPressures;

   --todo fix values
   procedure ChangeGasRatio is
      Gas_Fractions : GasFractions;
      -- SumGF         : Float       := System_State.Gas_Fraction.fO2 + System_State.Gas_Fraction.fCO2 + System_State.Gas_Fraction.fHe;


   begin
      if System_State.SafeMaybe = Unsafe then
         AS_Put_Line(" ");
         -- Adjust CO2
         if System_State.Partial_Pressures.ppCO2 > Float(SafeCO2'Last) then
            AS_Put("Reducing CO2...");
            AS_Put_Line("scrubbing CO2 and venting O2 and He.");
         elsif System_State.Partial_Pressures.ppCO2 < Float(SafeCO2'First) then
            AS_Put_Line("Increasing CO2...");

         end if;

         -- output actions
         if System_State.Partial_Pressures.ppO2 > Float(SafeO2'Last) then
            AS_Put("Reducing O2...");
            AS_Put_Line("Venting He to reduce O2 concentration.");

         elsif System_State.Partial_Pressures.ppO2 < Float(SafeO2'First) then
            AS_Put("Increasing O2...");
            AS_Put_Line("Venting O2 to increase the concentration of O2 in cabin. ");

         end if;

         -- change values if they aren't safe in original mixture
         --if both the following conditionals pass, the gasFractions are acceptable on initial input
         if (System_State.Partial_Pressures.ppO2 > Float(SafeO2'Last)) or  ( System_State.Partial_Pressures.ppO2 < Float(SafeO2'First) )then

            Gas_Fractions.fO2         := IdealO2  / System_State.pressure;
            Gas_Fractions.fCO2        := IdealCO2 / System_State.pressure;
            Gas_Fractions.fHe         := 1.0 - (Gas_Fractions.fO2 + Gas_Fractions.fCO2);
            System_State.Gas_Fraction := normaliseGasFractions(Gas_Fractions);

         elsif(System_State.Partial_Pressures.ppCO2 > Float(SafeCO2'Last)) then

            Gas_Fractions.fO2         := IdealO2  / System_State.pressure;
            Gas_Fractions.fCO2        := IdealCO2 / System_State.pressure;
            Gas_Fractions.fHe         := 1.0 - (Gas_Fractions.fO2 + Gas_Fractions.fCO2);
            System_State.Gas_Fraction := normaliseGasFractions(Gas_Fractions);

         end if;
         AS_Put_Line(" ");
         pragma Assert (System_State.pressure > 0.0);



      end if;
   end ChangeGasRatio;

   function normaliseGasFractions(Gas_Fractions : GasFractions) return GasFractions is

      Sum    : Float;
      Result : GasFractions       := Gas_Fractions;


   begin
      sum := Gas_Fractions.fO2 + Gas_Fractions.fCO2 + Gas_Fractions.fHe;
      if Sum /= 1.0  then
         Result.fO2 := Gas_Fractions.fO2 / Sum;
         Result.fCO2 := Gas_Fractions.fCO2 / Sum;
         Result.fHe := Gas_Fractions.fHe / Sum;
      end if;
      pragma Assert(Result.fO2 + Result.fCO2 + Result.fHe = 1.0);
      return Result;
   end NormaliseGasFractions;




   function FloatGasFractions(notGas_Fractions : notGasFractions) return GasFractions is

      Sum           : Integer := notGas_Fractions.ffO2 + notGas_Fractions.ffCO2 + notGas_Fractions.ffHe;
      Gas_Fractions : GasFractions;
      Total         : Float;

      -- Result : GasFractions := (fO2 => 0.0, fCO2 => 0.0, fHe => 0.0);
   begin
      pragma Assert(Sum > 0, "Sum of fractions must be greater than 0."); -- Validate inputs

      if Sum = 1  then
         --the values are therefore norm anyway
         Gas_Fractions := (Float(notGas_Fractions.ffO2) ,
                           Float(notGas_Fractions.ffCO2),
                           Float(notGas_Fractions.ffHe));
         return Gas_Fractions;
      else
         Gas_Fractions  := (Float(notGas_Fractions.ffO2)  / Float(sum) ,
                            Float(notGas_Fractions.ffCO2) / Float(sum),
                            Float(notGas_Fractions.ffHe)  / float(sum));
      end if;

      pragma Assert(Gas_Fractions.fO2 >= 0.0 and Gas_Fractions.fO2   <= 1.0,
                    "O2 fraction out of bounds.");
      pragma Assert(Gas_Fractions.fCO2 >= 0.0 and Gas_Fractions.fCO2 <= 1.0,
                    "CO2 fraction out of bounds.");
      pragma Assert(Gas_Fractions.fHe >= 0.0 and Gas_Fractions.fHe   <= 1.0,
                    "He fraction out of bounds.");

      Total := Gas_Fractions.fO2 + Gas_Fractions.fCO2 + Gas_Fractions.fHe;


      pragma Assert(abs(Total - (1.0)) <= Tolerance);

      return Gas_Fractions;


   end FloatGasFractions;




   procedure displayFinalPP is
      Tolerance     : constant Float := 1.0E-6;

   begin
      pragma Assert(abs(System_State.Partial_Pressures.ppO2 + System_State.Partial_Pressures.ppCO2 + System_State.Partial_Pressures.ppHe - (System_State.pressure)) <= Tolerance,"Partial pressures must sum to the total pressure within tolerance.");

      AS_Put_Line(" ");

      AS_Put("Total Pressure is: ");
      AS_Put_Line(Float'Image(System_State.pressure));

      AS_Put("sum of Partial Pressures: ");
      AS_Put_Line(Float'Image(System_State.Partial_Pressures.ppO2 + System_State.Partial_Pressures.ppCO2 + System_State.Partial_Pressures.ppHe));


      AS_Put("FINAL Partial Pressure of O2: ");
      AS_Put_Line(Float'Image(System_State.Partial_Pressures.ppO2));

      AS_Put("FINAL Partial Pressure of CO2: ");
      AS_Put_Line(Float'Image(System_State.Partial_Pressures.ppCO2));

      AS_Put("FINAL Partial Pressure of He: ");
      AS_Put_Line(Float'Image(System_State.Partial_Pressures.ppHe));


      AS_Put_Line(" ");
      AS_Put_Line(" ");
   end displayFinalPP;



   procedure Init is
   begin
      AS_Init_Standard_Input;
      AS_Init_Standard_Output;


      System_State := (pressure            =>  1.0,
                       Partial_Pressures   => (ppO2 => 1.0, ppCO2 => 0.0, ppHe => 2.0),
                       Gas_Fraction        => (fO2 => 0.0, fCO2 => 0.0, fHe => 0.0),
                       SafeMaybe           =>  Safe);
   end Init;




end Driver;
