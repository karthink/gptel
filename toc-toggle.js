// TOC toggle button: toggle TOC visibility on click
document.addEventListener('DOMContentLoaded', function() {
  var btn = document.getElementById('toc-toggle-btn');
  var toc = document.getElementById('table-of-contents-1');
  if (!btn || !toc) return;
  btn.addEventListener('click', function() {
    if (toc.classList.contains('visible')) {
      toc.classList.remove('visible');
    } else {
      toc.classList.add('visible');
    }
  });
  function updateTOCOnResize() {
    var isWide = window.matchMedia('(min-width: 1501px)').matches;
    if (isWide) {
      toc.classList.add('visible');
      btn.style.display = 'none';
      toc.style.transform = '';
      toc.style.opacity = '';
      toc.style.pointerEvents = '';
    } else {
      btn.style.display = 'block';
      toc.classList.remove('visible');
      toc.style.transform = '';
      toc.style.opacity = '';
      toc.style.pointerEvents = '';
    }
  }
  window.addEventListener('resize', updateTOCOnResize);
  window.addEventListener('DOMContentLoaded', updateTOCOnResize);
  updateTOCOnResize();
});

// Dark theme toggle
window.addEventListener('DOMContentLoaded', function() {
    function toggleDarkTheme() {
        const body = document.body;
        const html = document.documentElement;

        if (body.classList.contains('dark-theme') || html.classList.contains('dark-theme')) {
            body.classList.remove('dark-theme');
            html.classList.remove('dark-theme');
            localStorage.setItem('theme', 'light');
        } else {
            body.classList.add('dark-theme');
            html.classList.add('dark-theme');
            localStorage.setItem('theme', 'dark');
        }
    }

  const button = document.getElementById('dark-theme-toggle');
  if (button) {
    button.addEventListener('click', toggleDarkTheme);
  }

  // Apply saved theme
  const savedTheme = localStorage.getItem('theme');
    if ((savedTheme === 'dark') ||
    (!savedTheme && window.matchMedia &&
     window.matchMedia('(prefers-color-scheme: dark)').matches)) {
    document.body.classList.add('dark-theme');
    document.documentElement.classList.add('dark-theme');
  }
});

