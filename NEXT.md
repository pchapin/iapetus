
Next Steps
==========

Right now the server only runs for one minute and then shuts itself off (a bit uncleanly, it
seems like... doesn't appear to close the listening port). It should be shut off on command and
clean itself up properly.

Should the observation data be moved into a private GitHub repository? That would make it easier
to set up and maintain when Iapetus is running on a real server. I should probably change
Iapetus to automatically look in the right place for style sheets rather than relying on
hacked paths in the observation data files.
