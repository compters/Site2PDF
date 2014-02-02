<apply template="base">
  <div class="textContent">
    <h3>Feedback</h3>
    <form id="feedbackForm" action="/feedback/submit/" method="POST">
    <div class="feedback">
      <div class="prefix">
        I think ...&nbsp;
      </div>
      <textarea name="feedbackField" id="feedbackField" rows="4" placeholder="Type your thoughts"></textarea>
      <button type="submit" class="submitFeedbackButton">... and that's all I have to say on the matter.</button>
    </div>
    </form>
  </div>
</apply>
