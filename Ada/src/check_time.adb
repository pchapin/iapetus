---------------------------------------------------------------------------
-- FILE       : check_time.adb
-- SUBJECT    : Test procedure for the various time utilities.
-- PROGRAMMER : (C) Copyright 2010 by Vermont Technical College
--
---------------------------------------------------------------------------
with Ada.Calendar;
with Ada.Text_IO;
with Astronomical_Time;

procedure Check_Time is 

   procedure Check_Julian_Day is
      type Test_Date is
         record
            Y : Ada.Calendar.Year_Number;
            M : Ada.Calendar.Month_Number;
            D : Ada.Calendar.Day_Number;
            S : Ada.Calendar.Day_Duration;
         end record;

      Test_Data : array(1 .. 9) of Test_Date :=
        (( Y => 1901, M => 1, D =>  1, S => 0.0     ),   -- JD = 2_415_385.5
         ( Y => 1987, M => 1, D => 27, S => 0.0     ),   -- JD = 2_446_822.5
         ( Y => 1987, M => 6, D => 19, S => 43_200.0),   -- JD = 2_446_966.0
         ( Y => 1988, M => 1, D => 27, S => 0.0     ),   -- JD = 2_447_187.5
         ( Y => 1988, M => 6, D => 19, S => 43_200.0),   -- JD = 2_447_332.0
         ( Y => 1999, M => 1, D =>  1, S => 0.0     ),   -- JD = 2_451_179.5
         ( Y => 2000, M => 1, D =>  1, S => 43_200.0),   -- JD = 2_451_545.0
         ( Y => 2050, M => 9, D =>  7, S => 22_135.0),   -- JD = 2_470_056.756
         ( Y => 2100, M => 2, D => 28, S => 86_399.0));  -- JD = 2_488_128.5 - e

      Now : Ada.Calendar.Time;
      JD  : Astronomical_Time.Julian_Day;
   begin
      for I in Test_Data'Range loop
         Now := Ada.Calendar.Time_Of
           (Year    => Test_Data(I).Y,
            Month   => Test_Data(I).M,
            Day     => Test_Data(I).D,
            Seconds => Test_Data(I).S);

         JD  := Astronomical_Time.To_JD(Now);
         Astronomical_Time.Put(JD);
         Ada.Text_IO.New_Line;
      end loop;
      
      -- I'm not sure Interval_Between works right. Let's do a quick check now.
      -- Eventually this should be broken out into a different test procedure, etc, etc.
      --
      Now := Ada.Calendar.Time_Of(Year => 2000, Month => 1, Day => 1, Seconds => 43_200.0);
      JD  := Astronomical_Time.To_JD(Now);
      if Astronomical_Time.Interval_Between(JD, JD) /= 0.0 then
          Ada.Text_IO.Put_Line("Interval_Between two identical Julian days is not zero!");
      end if;
   end Check_Julian_Day;

begin
   Check_Julian_Day;
end Check_Time;
