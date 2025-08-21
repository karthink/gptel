// TOC toggle button: toggle TOC visibility on click
document.addEventListener('DOMContentLoaded', function() {
  var btn = document.getElementById('toc-toggle-btn');
  var toc = document.getElementById('table-of-contents-1');
  if (!btn || !toc) return;
  btn.addEventListener('click', function(event) {
    event.stopPropagation();
    if (toc.classList.contains('visible')) {
      toc.classList.remove('visible');
      document.removeEventListener('mousedown', onOutsideClick);
    } else {
      toc.classList.add('visible');
      setTimeout(function() {
        document.addEventListener('mousedown', onOutsideClick);
      }, 0);
    }
  });

  function onOutsideClick(e) {
    if (!toc.contains(e.target) && e.target !== btn) {
      toc.classList.remove('visible');
      document.removeEventListener('mousedown', onOutsideClick);
    }
  }
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
        const button = document.getElementById('dark-theme-toggle');
        const isDark = body.classList.contains('dark-theme') || html.classList.contains('dark-theme');
        if (isDark) {
            body.classList.remove('dark-theme');
            html.classList.remove('dark-theme');
            if (button) button.setAttribute('aria-pressed', 'false');
            localStorage.setItem('theme', 'light');
        } else {
            body.classList.add('dark-theme');
            html.classList.add('dark-theme');
            if (button) button.setAttribute('aria-pressed', 'true');
            localStorage.setItem('theme', 'dark');
        }
    }

    const button = document.getElementById('dark-theme-toggle');
    if (button) {
        button.addEventListener('click', toggleDarkTheme);
    }

    // Set correct icon on load
    function setInitialThemeButtonState() {
        const button = document.getElementById('dark-theme-toggle');
        const isDark = document.body.classList.contains('dark-theme') || document.documentElement.classList.contains('dark-theme');
        if (button) button.setAttribute('aria-pressed', isDark ? 'true' : 'false');
    }

    // Apply saved theme
    const savedTheme = localStorage.getItem('theme');
    if ((savedTheme === 'dark') ||
        (!savedTheme && window.matchMedia &&
         window.matchMedia('(prefers-color-scheme: dark)').matches)) {
        document.body.classList.add('dark-theme');
        document.documentElement.classList.add('dark-theme');
    }
    setInitialThemeButtonState();
});

