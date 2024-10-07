---------------------------------------------------------------------------
-- FILE       : iapetus.adb
-- SUBJECT    : Main program of the Iapetus web application
-- PROGRAMMER : (C) Copyright 2013 by Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

with Ada.Text_IO;
with AWS.Messages;
with AWS.MIME;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with AWS.Utils;

procedure Iapetus is

   WS : AWS.Server.HTTP;

   function To_File_Name(URI : String) return String
      with Pre => URI'Length > 0;

   -- TODO (SECURITY): Make sure the URI does not contain any ".." elements. NOTE: AWS might already be doing this.
   function To_File_Name(URI : String) return String is
      -- TODO: Make the base URI configurable.
      Base_URI : constant String := "../../data";
   begin
      if URI(URI'Last) = '/' then
         return Base_URI & URI & "index.html";
      else
         return Base_URI & URI;
      end if;
   end To_File_Name;


   function Service(Request : AWS.Status.Data) return AWS.Response.Data is
      URI       : constant String := AWS.Status.URI(Request);
      File_Name : constant String := To_File_Name(URI);
   begin
      Ada.Text_IO.Put_Line("Requested URI: " & URI);
      if AWS.Utils.Is_Regular_File(File_Name) then
         return AWS.Response.File(Content_Type => AWS.MIME.Content_Type(File_Name), Filename => File_Name);
      else
         return AWS.Response.Acknowledge(AWS.Messages.S404, "<p>Not found: " & URI & "</p>");
      end if;
   end Service;

begin
   -- Initialize.
   AWS.MIME.Add_Extension("xht", "application/xhtml+xml");
   AWS.MIME.Add_Extension("xsl", "text/xsl");  -- Should really be application/xslt+xml.

   -- TODO: Create a better logger.
   Ada.Text_IO.Put_Line("*** Iapetus Running! ***");
   AWS.Server.Start(WS, "Iapetus", Service'Unrestricted_Access, Port => 8000);
   delay 60.0;
end Iapetus;
