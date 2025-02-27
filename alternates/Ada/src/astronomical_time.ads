---------------------------------------------------------------------------
-- FILE       : astronomical_time.ads
-- SUBJECT    : Package to manage various forms of time as used in astronomy.
-- PROGRAMMER : (C) Copyright 2010 by Vermont Technical College
--
---------------------------------------------------------------------------
with Ada.Calendar;
with Ada.Text_IO;

package Astronomical_Time is

    type Julian_Day is private;

   -- Returns the Julian Day corresponding to the given date/time (assumed to be UTC). This
   -- function only supports dates in the range 1901-01-01 to 2100-02-28. The lower bound of
   -- this range is a restriction imposed by Ada.Calendar.Time. If the upper bound is exceeded,
   -- Constraint_Error is raised.
   --
   function To_JD(Now : Ada.Calendar.Time) return Julian_Day;

   -- Advances the Julian Day by the specified number of seconds.
   function Advance_JD(Now : Julian_Day; Interval : Duration) return Julian_Day;

   -- Returns the distance (always non-negative) between the given dates.
   function Interval_Between(Left : Julian_Day; Right : Julian_Day) return Duration;

   -- Reads a Julian Day from a text file.
   procedure Get(Input : in out Ada.Text_IO.File_Type; Item : out Julian_Day);

   -- Writes a Julian Day to the standard output device.
   procedure Put(JD : Julian_Day);

   -- Writes a Julian Day to a text file.
   procedure Put(File_Name : in Ada.Text_IO.File_Type; JD : Julian_Day);

   -- Relational operators for Julian days.
   function Greater(Left : Julian_Day; Right : Julian_Day) return Boolean;

private
   -- The range below is as tight as possible (to help catch subtle errors). The upper bound is
   -- defined by the highest JD value we support (2100-02-28 at the end of the day). It goes a
   -- bit above the highest required value because of the way the JD is being calculated. See
   -- the package body for the specifics.
   --
   type Julian_Day is delta 0.001 range 2_415_385.5 .. 2_489_665.0;

end Astronomical_Time;
