<?php
/*  Collector
    A program for running experiments on the web
    Copyright 2012-2015 Mikey Garcia & Nate Kornell
 */
    require 'initiateCollector.php';
    
    $title = 'Basic Information';
    require $_PATH->get('Header');
?>
<style>    
    #content {
        width:auto;
        min-width: 400px;
        /*Make the flexchild, form, fit the basic info content size*/
    }
</style>
<form id="content" class="basicInfo" name="Demographics"
      action="<?= $_PATH->get('Basic Info Record') ?>" method="post" autocomplete="off">
    
    <fieldset>
        <legend><h1>Basic Information</h1></legend>
        
        
        <section class="radioButtons">
            <h3>Gender</h3>
            <label><input name="Gender" type="radio" value="Male"   required/>Male</label>
            <label><input name="Gender" type="radio" value="Female" required/>Female</label>
            <label><input name="Gender" type="radio" value="Other"  required/>Other</label>
        </section>
        
        
        <section>
            <label>
                <h3>Age</h3>
                <input name="Age" class="wide collectorInput" type="text"
                pattern="[0-9][0-9]" value="" autocomplete="off" required/>
            </label>
        </section>
        
        
        <section>
            <label>
                <h3>Education</h3>
                <select name="Education" class="wide collectorInput" required>
                    <option value="" default selected>Select Level</option>
                    <option>Some High School</option>
                    <option>High School Graduate</option>
                    <option>Some College, no degree</option>
                    <option>Associates degree</option>
                    <option>Bachelors degree</option>
                    <option>Graduate degree (Masters, Doctorate, etc.)</option>
                </select>
            </label>
        </section>
        
        
        <!-- <section class="radioButtons">
            <h3>Are you Hispanic?</h3>
            <label><input name="Hispanic" type="radio" value="Yes"   required/>Yes</label>
            <label><input name="Hispanic" type="radio" value="No"    required/>No</label>
        </section> -->
        
        
        <section>
            <label>
                <h3>Ethnicity</h3>
                <select name="Race" required class="wide collectorInput">
                    <option value="" default selected>Select one</option>
                    <option>American Indian/Alaskan Native</option>
                    <option>Asian/Pacific Islander</option>
                    <option>Black</option>
                    <option>White</option>
                    <option>Other/unknown</option>
                </select>
            </label>
        </section>
        
        
        <section class="radioButtons">
            <h3>Do you speak english fluently?</h3>
            <label><input name="Fluent" type="radio" value="Yes"   required/>Yes</label>
            <label><input name="Fluent" type="radio" value="No"    required/>No</label>
        </section>
        
        
        <section>
            <label>
                <h3>At what age did you start learning English?</h3>
                <input name="AgeEnglish" type="text" value="" autocomplete="off" class="wide collectorInput"/>
                <div class="small shim">If English is your first language please enter 0.</div>
            </label>
        </section>
        
        
        <section>
            <label>
                <h3>What is your country of residence?</h3>
                <input name="Country" type="text" value="" autocomplete="off" class="wide collectorInput"/>
            </label>
        </section>
        
        
        <section class="consent">
            <legend><h3>Informed Consent</h3></legend>
            <textarea readonly>The the purpose of this experiment is to examine basic memory processes. You will be presented with different lists of words and your memory for these words will be studied. The words will be presented in different font types. After study, you will complete a memory test. The study is expected to last approximately 20 minutes, and MSU students will receive 20 minutes credits towards their research requirement. Prolific participants will be financially compensated at a rate of $9/hour USD ($3.00/session). Aside from these types of compensation, participants can expect to learn about some of the current issues being investigated in human memory. The researchers do not expect any risks from participating in this study other than the possibility of slight boredom during task completion. Your participation will be kept confidential. Identifiable information will not be linked to the data and therefore you could not be identified based on the data. For Prolific participants, participation is anonymous as identifying information is not needed to provide compensation. For all participants, all electronic data collected will be password protected on a laboratory computers. You are free to discontinue at anytime. Your data will be deleted immediately after you withdraw. For student participants, alternative options are available at the discretion of your instructor. Please contact your instructor for information about alternative participation options. For Prolific participants, there are not alternative participation options available. This project and the consent form have been reviewed the Institutional Review Board at Midwestern State University. Any questions should be directed to nicholas.maxwell@msutexas.edu</textarea>
            <label>
                <span class="shim">Check this box if you have read, understand, 
                    and agree to the Informed Consent above.</span>
                <input type="checkbox" name="consent" required/>
            </label>
        </section>
        
        
        <section>
            <button class="collectorButton">Submit Basic Info</button>
        </section>
        
    </fieldset>
</form>

<?php
    require $_PATH->get('Footer');
