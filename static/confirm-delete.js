// Double confirmation before deleting a practice set. Attached to every
// delete form (dashboard rows and the attempt page); the first confirmation
// names the set, the second guards against an accidental first click.
(() => {
  document.querySelectorAll('form[action$="/delete"]').forEach(form => {
    form.addEventListener('submit', event => {
      event.preventDefault();
      const label = form.dataset.deleteLabel || 'this practice set';
      if (!window.confirm(`Delete ${label}? Its answers, flags and scores will be permanently removed.`)) return;
      if (!window.confirm('Really delete? This cannot be undone.')) return;
      form.submit();
    });
  });
})();
