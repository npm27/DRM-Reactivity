<?php
    if (!isset($text) || $text === '') { 
        $text = 'How likely are you to correctly recall each of the items you just studied?|Type your response on a scale from 0-100 and then press Enter (0 = WILL NOT REMEMBER; 100 = DEFINITELY WILL REMEMBER).';
    }

    $texts = explode('|', $text);
    $mainText = array_shift($texts);
?>

<div class="textcenter">
    <div><?php echo isset($text) ? $text : ""; ?></div>

<br>

<br>
  
<div class="textcenter">
    <input name="JOL" type="text" value="" autocomplete="off" class="forceNumeric textcenter collectorInput">
    <button class="collectorButton" id="FormSubmitButton">Submit</button>
</div>
