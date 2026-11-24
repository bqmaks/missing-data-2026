/* Extra course navigation, independent of the shared theme. No network calls. */
(() => {
  const start = () => {
    const deck = window.Reveal;
    if (!deck?.isReady()) { setTimeout(start, 50); return; }
    const params = new URLSearchParams(location.search);
    const slides = Array.from(document.querySelectorAll('.slides section[data-source]'));
    const initialId = decodeURIComponent(location.hash.replace(/^#\/?/, ''));
    const initialSlide = slides.find(s => s.id === initialId);
    let all = params.get('layers') === 'all' || params.has('print-pdf') ||
      Boolean(initialSlide && initialSlide.dataset.layer !== 'core');
    const parked = new Map();
    const tools = document.createElement('nav');
    tools.className = 'course-tools'; tools.setAttribute('aria-label', 'Управление курсом');
    tools.innerHTML = '<button id="course-layers" type="button">Все слои · D</button>' +
      '<button id="course-notes" type="button" aria-pressed="false">Заметки · N</button>' +
      '<button id="course-overview" type="button">Обзор · O</button>' +
      '<a href="reading.html">Чтение</a><a href="downloads/missing_data_practice.R" download>R-файл</a>';
    document.body.append(tools);
    const panel = document.createElement('aside');
    panel.className = 'notes-panel'; panel.setAttribute('aria-label', 'Заметки докладчика');
    document.body.append(panel);
    const updateNotes = () => {
      panel.innerHTML = deck.getCurrentSlide()?.querySelector('aside.notes')?.innerHTML || 'Заметок к этому слайду нет.';
      const current = deck.getCurrentSlide();
      const visible = slides.filter(s => s.isConnected);
      const footer = document.querySelector('.mat-footer-num');
      if (footer) footer.textContent = `${visible.indexOf(current) + 1} / ${visible.length}`;
    };
    const layers = () => {
      const current = deck.getCurrentSlide();
      if (!all && current?.dataset.layer !== 'core') deck.slide(0, 0);
      slides.forEach(slide => {
        if (slide.dataset.layer === 'core') return;
        if (!all && slide.isConnected) {
          const marker = document.createComment(slide.id);
          slide.replaceWith(marker); parked.set(slide, marker);
        } else if (all && parked.has(slide)) {
          parked.get(slide).replaceWith(slide); parked.delete(slide);
        }
      });
      deck.sync();
      document.getElementById('course-layers').setAttribute('aria-pressed', String(all));
      const url = new URL(location.href);
      if (all) url.searchParams.set('layers', 'all'); else url.searchParams.delete('layers');
      history.replaceState(null, '', url);
      updateNotes();
    };
    const toggleLayers = () => { all = !all; layers(); };
    const toggleNotes = () => {
      panel.classList.toggle('open'); updateNotes();
      document.getElementById('course-notes').setAttribute('aria-pressed', String(panel.classList.contains('open')));
    };
    document.getElementById('course-layers').onclick = toggleLayers;
    document.getElementById('course-notes').onclick = toggleNotes;
    document.getElementById('course-overview').onclick = () => deck.toggleOverview();
    deck.addKeyBinding({keyCode: 68, key: 'D', description: 'Все слои'}, toggleLayers);
    deck.addKeyBinding({keyCode: 78, key: 'N', description: 'Заметки'}, toggleNotes);
    deck.addKeyBinding({keyCode: 79, key: 'O', description: 'Оглавление'}, () => deck.toggleOverview());
    deck.on('slidechanged', updateNotes);
    document.addEventListener('click', event => {
      const link = event.target.closest('a.slide-ref');
      if (!link) return;
      const target = slides.find(s => link.getAttribute('href') === `#/${s.id}`);
      if (!target) return;
      event.preventDefault();
      if (!target.isConnected) { all = true; layers(); }
      const {h, v} = deck.getIndices(target);
      deck.slide(h, v);
    });
    layers();
    // Preserve direct links, including optional slides without a layers query.
    if (initialSlide) {
      const {h, v} = deck.getIndices(initialSlide);
      deck.slide(h, v);
    }
    window.courseReady = true;
  };
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start); else start();
})();
