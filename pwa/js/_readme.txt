Files in this folder have "../" path prefixes which may need stripping.

They are "backup copies" for validating the transpiler, and should *NOT*
be modified, corrected, or otherwise experimented on in here.

You may of course either copy these files up one directory, remove those
path prefixes, and run/edit that, or better yet run pwa/p2js and convert 
the equivalent .exw from the ../phix directory, and run/edit that.

The poc directory contains "proof of concept" samples, in other words 
hand-crafted code that is reasonably ready for addition to pGUI.js,
and/or a template for the code that pwa/p2js.exw should be emitting.
Some of them may now be fully integrated (there should be a comment in
the individual files to indicate that) but can still be useful should
something suddenly stop working or start going wrong.

Unlike pwa/js, there is no particular reason for you not to edit files 
directly in pwa/js/poc.

Sometimes, "pipe dreams" may end up in /poc that pwa/p2js is a very long
way off actually supporting... (again there should be a comment in the 
individual files to indicate that)
