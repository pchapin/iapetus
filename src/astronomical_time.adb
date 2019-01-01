---------------------------------------------------------------------------
-- FILE       : astronomical_time-utilities.adb
-- SUBJECT    : Package containing various time manipulation utilties.
-- PROGRAMMER : (C) Copyright 2010 by Vermont Technical College
--
---------------------------------------------------------------------------

package body Astronomical_Time is

   package Julian_IO is new Ada.Text_IO.Fixed_IO(Julian_Day);

   -- Reference: "Astronomical Algorithms," second edition, by Jean Meeus. Published
   -- by Willmann-Bell, copyright 1998, ISBN=0-943396-61-1; Chapter 7. This method
   -- only works for dates between 1900-03-01 and 2100-02-28.
   --
   function To_JD(Now : Ada.Calendar.Time) return Julian_Day is
      -- We need slightly different subtypes than provided by Ada.Calendar.
      subtype Internal_Year_Number  is Integer range 1900 .. 2100;
      subtype Internal_Month_Number is Integer range 1 .. 14;

      Adjusted_Year  : Internal_Year_Number  := Ada.Calendar.Year(Now);
      Adjusted_Month : Internal_Month_Number := Ada.Calendar.Month(Now);
      Temp : Integer;
   begin
      -- Before we do anything else make sure we are processing a date we can handle.
      if Adjusted_Year = 2100 and Adjusted_Month > 2 then
         raise Constraint_Error;
      end if;

      -- January and February are considered the 13th and 14th months of the previous year.
      if Adjusted_Month <= 2 then
         Adjusted_Year  := Adjusted_Year  -  1;
         Adjusted_Month := Adjusted_Month + 12;
      end if;

      Temp := Adjusted_Year + 4716;
      Temp := (365 * Temp) + (Temp / 4);
      Temp := Temp + (30 * (Adjusted_Month + 1)) + ((6 * (Adjusted_Month + 1))/10);

      return (Julian_Day(Temp + Ada.Calendar.Day(Now)) - 1537.5) +
                Ada.Calendar.Seconds(Now)/86_400.0;  -- Handles fractional days.
   end To_JD;


   function Advance_JD(Now : Julian_Day; Interval : Duration) return Julian_Day is
   begin
      return Now + (Interval / 86_400.0);
   end Advance_JD;


   -- Returns the distance (always non-negative) between the given dates.
   function Interval_Between(Left : Julian_Day; Right : Julian_Day) return Duration is
      Result : Duration;
   begin
      if Left > Right then
         Result := 86_400.0 * (Left - Right);
      else
         Result := 86_400.0 * (Right - Left);
      end if;
      return Result;
   end Interval_Between;


   -- Takes in a string, and output a Julien Day
   procedure Get(Input : in out Ada.Text_IO.File_Type; Item : out Julian_Day) is
   begin
      Julian_IO.Get(Input, Item);
   end Get;


   procedure Put(JD : Julian_Day) is
   begin
      Julian_IO.Put(JD);
   end Put;


   procedure Put(File_Name : in Ada.Text_IO.File_Type; JD : Julian_Day) is
   begin
      Julian_IO.Put(File_Name, JD);
   end Put;


   function Greater(Left : Julian_Day; Right : Julian_Day) return Boolean is
   begin
      return Left > Right;
   end Greater;


end Astronomical_Time;
