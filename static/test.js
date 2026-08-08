(() => {
  const app = document.querySelector('#test-app');
  if (!app) return;

  const attemptId = app.dataset.attemptId;
  const moduleId = app.dataset.moduleId;
  const questionId = Number(app.dataset.questionId);
  const deadline = Number(app.dataset.deadline) * 1000;
  const csrf = app.dataset.csrf;
  const resultsUrl = app.dataset.resultsUrl;
  const saveUrl = `/api/attempts/${attemptId}/modules/${moduleId}/save`;
  const submitUrl = `/api/attempts/${attemptId}/modules/${moduleId}/submit`;
  const state = document.querySelector('#save-state');
  let timerHandle;
  let debounce;
  let submitting = false;

  const request = async (url, payload) => {
    const response = await fetch(url, {
      method: 'POST',
      credentials: 'same-origin',
      headers: {'Content-Type': 'application/json', 'X-CSRF-Token': csrf},
      body: JSON.stringify(payload || {})
    });
    const data = await response.json().catch(() => ({}));
    if (!response.ok) throw new Error(data.error || 'Request failed');
    return data;
  };

  const setSaveState = (kind, text) => {
    state.className = kind;
    state.textContent = text;
  };

  const save = async (payload) => {
    setSaveState('saving', 'Saving…');
    try {
      await request(saveUrl, {question_id: questionId, ...payload});
      setSaveState('', 'Saved');
    } catch (error) {
      setSaveState('error', error.message);
    }
  };

  document.querySelectorAll('input[name="answer"]').forEach(input => {
    input.addEventListener('change', () => save({answer: input.value}));
  });

  const spr = document.querySelector('#answer-input');
  if (spr) {
    const preview = document.querySelector('#spr-preview');
    let typesetQueue = Promise.resolve();

    const mathEl = (tag, text) => {
      const el = document.createElementNS('http://www.w3.org/1998/Math/MathML', tag);
      if (text !== undefined) el.textContent = text;
      return el;
    };

    const buildPreviewMath = (value) => {
      const math = mathEl('math');
      const normalized = value.replace(/[\s,]/g, '').replace(/−/g, '-');
      const fraction = normalized.match(/^(-?\d+(?:\.\d+)?)\/(-?\d+(?:\.\d+)?)$/);
      if (fraction) {
        const mfrac = mathEl('mfrac');
        mfrac.append(mathEl('mn', fraction[1]), mathEl('mn', fraction[2]));
        math.appendChild(mfrac);
      } else if (/^-?\d+(?:\.\d+)?(?:e[+-]?\d+)?$/i.test(normalized)) {
        math.appendChild(mathEl('mn', normalized));
      } else {
        math.appendChild(mathEl('mtext', value));
      }
      return math;
    };

    const renderPreview = () => {
      if (!preview) return;
      preview.textContent = '';
      if (!spr.value.trim()) return;
      preview.appendChild(buildPreviewMath(spr.value.trim()));
      typesetQueue = typesetQueue.then(() => {
        if (window.MathJax && MathJax.startup) {
          return MathJax.startup.promise
            .then(() => MathJax.typesetPromise([preview]))
            .catch(() => {});
        }
      });
    };

    spr.addEventListener('input', () => {
      renderPreview();
      clearTimeout(debounce);
      debounce = setTimeout(() => save({answer: spr.value}), 450);
    });
    spr.addEventListener('blur', () => {
      clearTimeout(debounce);
      save({answer: spr.value});
    });
    renderPreview();
  }

  const flag = document.querySelector('#flag-input');
  if (flag) flag.addEventListener('change', () => save({flagged: flag.checked}));

  const submit = async (askConfirmation) => {
    if (submitting) return;
    if (askConfirmation && !window.confirm('Submit this module? Unanswered questions will be marked as skipped.')) return;
    submitting = true;
    clearInterval(timerHandle);
    setSaveState('saving', 'Submitting…');
    try {
      await request(submitUrl, {});
      window.location.assign(resultsUrl);
    } catch (error) {
      submitting = false;
      setSaveState('error', `${error.message}. Your responses are locked and grading can be retried.`);
    }
  };

  const submitButton = document.querySelector('#submit-module');
  if (submitButton) submitButton.addEventListener('click', () => submit(true));

  const timer = document.querySelector('#timer');
  const tick = () => {
    const remaining = Math.max(0, Math.ceil((deadline - Date.now()) / 1000));
    const minutes = Math.floor(remaining / 60);
    const seconds = remaining % 60;
    timer.textContent = `${minutes}:${String(seconds).padStart(2, '0')}`;
    timer.classList.toggle('warning', remaining <= 300);
    if (remaining <= 0) submit(false);
  };
  tick();
  timerHandle = setInterval(tick, 1000);
})();
