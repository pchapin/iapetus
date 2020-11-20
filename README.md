
Iapetus
=======

Iapetus is a web application that allows amateur astronmers to prepare observing sessions,
record their observations, and build a community. It is intended to offer features supporting
both individual observers and groups of observers. It is intended to provide reference material,
personal logs, picture galleries, wikis, blogs, and more. In its most mature form Iapetus would
be a comprehensive, "one-stop" site for all things related to making, recording, and sharing
amateur astronomical observations.

As a secondary, but still important goal, Iapetus is also inteded to be a learning vehicle. I
intend to use this project to finally get into web application programming in a significant way.
At the time of this writing (November 2017) my skills in this important area of software
development are meger. Working on Iapetus will hopefully change that.

Currently the project is little more than a place holder, but... well... you have to start
somewhere!

Setup
-----

Right now I assume you will be building Iapetus from source. There is no installer. You will
need a recent version of the GNAT Ada compiler, GPS, and the AWS web server library. Build the
project src/iapetus.gpr. The executable will be in src/build/iapetus.

Place your personal observations in a folder named 'data' as a sibling of the source tree. Use
'index.html' as the index file. Use '.xhtml' as the extension for XHTML documents. If you
include any AOML marked up documents, check out the AOML repository into an AOML folder that is
also a sibling of the source tree (and thus of the data folder). Specify the style sheet as
'../AOML/AOML.xsl'. You might want to do the same thing for events.xsl and sky.xsl as well. Note
that some of this hackery will likely be removed in the future (Iapetus will be programmed to
search the AOML folder for style sheets automatically).

Peter C. Chapin  
chapinp@acm.org
