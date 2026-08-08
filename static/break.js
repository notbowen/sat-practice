// Countdown for the 10-minute break between the Reading & Writing and Math
// sections, shown on the attempt page. Reloads once every break has elapsed
// so the "Start module" button reappears without a manual refresh.
(() => {
  const timers = document.querySelectorAll('[data-break-until]');
  if (!timers.length) return;

  const handle = setInterval(tick, 1000);

  function tick() {
    const now = Date.now();
    let active = false;
    timers.forEach(el => {
      const until = Number(el.dataset.breakUntil) * 1000;
      const remaining = Math.max(0, Math.ceil((until - now) / 1000));
      const minutes = Math.floor(remaining / 60);
      const seconds = remaining % 60;
      el.textContent = `Break — ${minutes}:${String(seconds).padStart(2, '0')} remaining`;
      if (remaining > 0) active = true;
    });
    if (!active) {
      clearInterval(handle);
      window.location.reload();
    }
  }

  tick();
})();
