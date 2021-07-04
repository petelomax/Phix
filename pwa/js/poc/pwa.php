<!DOCTYPE html>
<html lang="en" >
 <head>
  <meta charset="utf-8"/>
  <meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1"/>
  <meta name="viewport" content="width=device-width"/>
  <meta name="description" content="Test drop/paste/edit file"/>
  <title>Drop Test</title>
  <link rel="stylesheet" href="drop.css"/>
 </head>
 <body>
  <!--
    DEV IupDialog/IupBrowseButton/IupMultiLine(===IupText(MULTILINE=YES)),
        or maybe even an IupSyntaxColouredText(LANG=exw/js/html/css)...)
  -->
<?php
$qs = $_SERVER['QUERY_STRING'];
if ($qs=="") {
    $content = "";
    $path = "";
} else {
    $path = 'src/' . $qs . ".exw";
    if (file_exists($path)) {
        $content = file_get_contents($path);
    } else {
        $content = $path . " not found";
        $path = "";
    }
}
?>
  <h2>Test load/choose/drop/paste/edit file</h2>
  <span class="template-2col">
   <div>
    <input type="file" name="fileElem" id="fileElem" accept=".exw,.htm,.html,.js,.css"/>
    <label class="button" for="fileElem">Choose File</label>
    <div type="text" class="label truncate" id="filename"><?php echo (isset($path))?$path:'';?></div>
    <div id="drop-area">
     <textarea id="input" placeholder="Type, paste, drag and drop a file here, or click the button above..."><?php echo $content; ?></textarea>
    </div>
   </div>
   <div>
    <label><b>Output</b></label>
    <textarea id="output">
    </textarea>
   </div>
  </span>
  <script src="drop.js"></script>
 </body>
</html>

