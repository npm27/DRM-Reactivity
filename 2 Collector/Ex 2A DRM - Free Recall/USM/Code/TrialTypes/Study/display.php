<style>
    #content {	width: 90%;  max-width: 800px;  }
</style>

<div><?php echo isset($text) ? $text : ""; ?></div>
<div class="study">
    <span class="study-left"   ><?php echo $cue;    ?></span>

</div>
<!-- include form to collect RT and advance page -->
<div class="textcenter">
    <button class="collectorButton collectorAdvance" 
    id="FormSubmitButton" type="submit" autofocus>Next</button>
</div>