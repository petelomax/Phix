 <?php
 if(isset($_POST['bt_submit']))
 {
 $file = "try_it.php";
 $fp = fopen($file, 'w');
 $content = $_POST['txt_data'];
 fwrite($fp, $content);
  fclose($fp);
  }
 ?>
<html>
 <head>
<script type="text/javascript">
function data_submit() { document.form1.txt_data.value = document.form1.txt_html.value; }
</script>
  <title>Try It yourself Online Editor</title>
 </head>
 <body>
  <form name="form1" id="form1" method="post" action="" >
   <table width="100%" cellspacing="0" cellpadding="0" border="1">
    <tr>
      <td> <input type="submit" name="bt_submit" value="Click to Execute " align="top" onClick="data_submit();"/></td>
      <td align="center">Output</td>
    </tr>
    <tr>
      <td width="50%" >
        <input type="text"  name="txt_data" value="" style="visibility:hidden;" />
         <textarea rows="35" width="90px" height="550px" cols="77" name="txt_html">
<?php
 if(isset($_POST['bt_submit'])) {
   echo trim($content);
 } else {
   echo "<html>\n";
   echo "<body>\n";
   echo "<h1>Hello World!!!</h1>\n";
   echo "</body>\n";
   echo "</html>\n";
 } ?>
         </textarea>
      </td>
      <td width="50%" style="border-width:10px;border-style:none;">
       <iframe height="550px" width="100%" src="try_it.php" name="iframe_a"></iframe>
      </td>
    </tr>
   </table>
  </form>
 </body>
</html>
