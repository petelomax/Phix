maintain toc.txt and index.txt by hand

Start a new file by copying mt.htm, replace "Introduction" and insert on the blank line(9)

When makephix.exw is run:
<head> is replaced with header.htm
<body> is replaced with body.htm (banner.htm is used for phix.htm, the top-level file)
</head> is replaced with trailer.htm
<toc> is constructed from toc.txt, in the style of toc.htm (see below)
<these> <get> <stripped> and the final </div> </stripped> </get> </these> </toc> </banner>
 exist so that Edita/Tools/reindent can be used. You can also use Edita/tools/htmlise eucode,
 as long as the <!--eucode> </eucode--> and <pre> </pre> are in column 1.

The indentation in toc.txt is critical to constructing the toc and navigation panes.
The descriptive text is obtained from the h1 on line 7 of the corresponding .htm file.


