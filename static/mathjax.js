// MathJax is vendored locally (static/mathjax/mml-svg.js) so the strict
// content-security-policy (script-src 'self') stays intact. Question bank
// content uses MathML, including <mfenced>, which browsers no longer render
// natively; MathJax's MathML input + SVG output handles it without web fonts.
window.MathJax = {
  svg: { fontCache: 'global' },
  options: {
    skipHtmlTags: ['script', 'noscript', 'style', 'textarea', 'pre', 'code', 'input', 'select', 'option']
  }
};

(() => {
  const script = document.createElement('script');
  script.src = '/static/mathjax/mml-svg.js';
  script.async = true;
  document.head.appendChild(script);
})();
