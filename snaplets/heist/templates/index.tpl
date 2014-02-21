<apply template="base">
  <script>  
    $(document).ready(function () {   
       $("#scrapeForm").submit(function () {
            $(".submitButton").attr("disabled", true);
            $(".error").css('opacity', 0.0);
            $(".msge").css('opacity', 0.0);
            return true;
       });
    });
    function errVid() {
       $("#flair").append('<iframe width="420" height="315" src="//www.youtube-nocookie.com/embed/MSHaCzb3yYk?rel=0&autoplay=1" frameborder="0" allowfullscreen></iframe>');  
    }
  </script>
  <form id="scrapeForm" action="/pdf/scrape/" method="POST">
    <div class="urlBox">
      <div class="inputWrapper">
        <label for="url">http://</label>
        <input type="text" name="url" id="url" placeholder="www.bbc.co.uk"/>
        <button type="submit" class="submitButton">PDF</button>
      </div>
    </div>
  </form>
  <error>
    <div style="display:${display}" class="error">
      We're <span class="fl" onclick="errVid()"> so sorry</span>.
      <message/>
      <div id="flair">        
      </div>
    </div>
  </error>
  <msg>
    <div style="display:${display}" class="msg"><message/></div>
  </msg>
</apply>
