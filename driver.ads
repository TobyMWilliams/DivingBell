pragma SPARK_Mode(On);


with SPARK.Text_IO;use  SPARK.Text_IO;
--with Text_IO; use Text_IO;


package Driver is




   type safeO2 is new Float range 0.4 .. 1.6;
   type safeCO2 is new Float range 0.0 .. 0.01;

   IdealO2  : Float := 1.0;
   IdealCO2 : Float := 0.003;


   type GasFractions is record
      fO2  : Float;
      fCO2 : Float;
      fHe  : Float;
   end record;


   type notGasFractions is record
      ffO2  : Integer;
      ffCO2 : Integer;
      ffHe  : Integer;
   end record;

   type PartialPressures is record
      ppO2  : Float;
      ppCO2 : Float;
      ppHe  : Float;
   end record;

   type Safety is (Safe, Unsafe);

   type Safety_System_Type is
      record
         pressure          : Float;
         Partial_Pressures : PartialPressures;
         Gas_Fraction      : GasFractions;
         SafeMaybe         : Safety;
      end record;

   System_State : Safety_System_Type;



   Tolerance     : Float := 1.0E-6;




   function CalculatePressure(Depth : Integer) return Float with
     Pre => (Depth >= 10),
     Post => (CalculatePressure'Result >= 2.0 and CalculatePressure'result < Float'Last),
     Depends => (CalculatePressure'Result => Depth);

   --and CalculatePressure'Result = Float(Depth) / 10.0 + 1.0);

   procedure takeValues with
     Global => (In_Out => Standard_Input, Output => (System_State, Standard_Output)),
     Depends => ((System_State, Standard_Input, Standard_Output) => standard_Input);


   procedure TakeDepth(Depth : out Integer )  with
     Global => (In_Out => (Standard_Output, Standard_Input)),
     Depends => (Standard_Output => (Standard_Output, Standard_Input),
                 Standard_Input  => Standard_Input,
                 Depth => standard_Input),
     Post => Depth >= 10;


     --Global => (In_Out => (Standard_Output, Standard_Input)),   Depends => ((Standard_Output, Depth)  => Standard_Input, null => (Standard_Output));
   -- Depends => ((Standard_Input, Standard_Output) => Standard_Input ,  (Standard_Output, Depth) => Standard_Output);


   procedure TakeF(Gas_Fractions : out GasFractions) with
     Global => (In_Out => (Standard_Output, Standard_Input)),
     Depends => (Gas_Fractions  => (Standard_Input),
                 Standard_Input  => Standard_Input,
                 Standard_Output => (Standard_Output, Standard_Input)),
     Post => Gas_Fractions.fO2 >= 0.0 and Gas_Fractions.fco2 >= 0.0 and Gas_Fractions.fHe >= 0.0 and
   Gas_Fractions.fO2 + Gas_Fractions.fCO2 + Gas_Fractions.fHe > 0.0;




   procedure TakeFraction (Fraction : out Integer) with
     Global => (In_Out => (Standard_Output, Standard_Input)),
     Depends => (Standard_Output => (Standard_Output,Standard_Input),
                 Standard_Input  => Standard_Input,
                 Fraction => standard_Input),
     Post => Fraction >= 0;


   procedure CalculatePartialPressures(
                                       GF : in GasFractions;
                                       pressure : in Float;
                                       PP : out PartialPressures
                                      ) with
     Pre => pressure >= 2.0 and
     GF.fO2 >= 0.0 and
     GF.fCO2 >= 0.0 and
     GF.fHe >= 0.0,
     Post => PP.ppO2 >= 0.0 and
     PP.ppCO2 >= 0.0 and
     PP.ppHe >= 0.0,
     Depends => (PP => (GF, pressure));


   procedure CalcPartialPressures with
     Global => (In_Out => System_State),
     Pre => System_State.pressure > 0.0 and
     System_State.Gas_Fraction.fO2 >= 0.0 and
     System_State.Gas_Fraction.fCO2 >= 0.0 and
     System_State.Gas_Fraction.fHe >= 0.0,
     Post => System_State.Partial_Pressures.ppO2 >= 0.0 and
     System_State.Partial_Pressures.ppCO2 >= 0.0 and
     System_State.Partial_Pressures.ppHe >= 0.0,
     Depends => (System_State => System_State);

   function Is_Safe (Status : Safety_System_Type) return Boolean is
     (if Status.Partial_Pressures.ppO2      >= Float(safeO2'First) and
          Status.Partial_Pressures.ppO2     <= Float(safeO2'Last) and
          Status.Partial_Pressures.ppCO2    >= Float(safeCO2'First) and
          Status.Partial_Pressures.ppCO2    <= Float(safeCO2'Last) then System_State.SafeMaybe = safe
      else System_State.SafeMaybe = Unsafe);


   procedure DisplayPartialPressures with

     Global => (Input => System_State, Output => Standard_Output),
     Depends => (Standard_Output => System_State),
     Pre => System_State.Partial_Pressures.ppO2 >= 0.0 and
     System_State.Partial_Pressures.ppCO2 >= 0.0 and
     System_State.Partial_Pressures.ppHe >= 0.0;


   procedure EvaluatePartialPressures(PP : in PartialPressures ; Safer : out Safety)  with
     Global => (In_Out => Standard_Output),
     Depends => ( Standard_Output => (Standard_Output, PP),
                  Safer => PP);



   procedure EvalPartialPressures  with
     Global => (In_Out => (Standard_Output, System_State)),
     Depends => (System_State => ( System_State), Standard_Output => (Standard_Output, System_State));



   procedure ChangeGasRatio with
     Global => (In_Out => (System_State), Input =>  (IdealO2, IdealCO2), Output => Standard_Output),
     Pre => System_State.Pressure > 0.0,
     Depends => ((System_State) => (IdealCO2, idealo2, System_State),
                 Standard_Output => System_State);



   function NormaliseGasFractions(Gas_Fractions : GasFractions) return GasFractions with
     Pre => Gas_Fractions.fO2 >= 0.0 and Gas_Fractions.fCO2 >= 0.0 and Gas_Fractions.fHe >= 0.0,
     Post => NormaliseGasFractions'Result.fO2 + NormaliseGasFractions'Result.fCO2 + NormaliseGasFractions'Result.fHe = 1.0,
     Depends => (NormaliseGasFractions'Result => Gas_Fractions);

   function FloatGasFractions(notGas_Fractions : notGasFractions
                             ) return GasFractions with
     Pre => notGas_Fractions.ffO2 >= 0
     and notGas_Fractions.ffCO2 >= 0
     and notGas_Fractions.ffHe >= 0,
     Post => FloatGasFractions'Result.fO2
           + FloatGasFractions'Result.fCO2
           + FloatGasFractions'Result.fHe = 1.0  and
           FloatGasFractions'Result.fO2 >= 0.0 and
           FloatGasFractions'Result.fCO2 >= 0.0 and
           FloatGasFractions'Result.fHe >= 0.0,
     Depends => (FloatGasFractions'Result => notGas_Fractions);



   procedure DisplayFinalPP with
     Global => (Input => System_State, Output => Standard_Output),
     Pre => System_State.Partial_Pressures.ppO2 >= 0.0 and
     System_State.Partial_Pressures.ppCO2 >= 0.0 and
     System_State.Partial_Pressures.ppHe >= 0.0,
     Depends => (Standard_Output => System_State);






   procedure Init with
     Global => (Output => (  Standard_Output, Standard_Input, System_State)),
     Depends => ((Standard_Output, Standard_Input, System_State) => null),
     Post    => Is_Safe(System_State);

end Driver;
