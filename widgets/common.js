// Палитра mice (D21), согласована с assets/course.css и r/slides.R.
export const courseColors = Object.freeze({observed: "#006CC2", missing: "#B61A51", imputed: "#B61A51"});

export function element(tag, text, attrs = {}) {
  const n = document.createElement(tag);
  if (text !== undefined) n.textContent = text;
  for (const [k, v] of Object.entries(attrs)) n.setAttribute(k, v);
  return n;
}
export function button(label, action, parent) {
  const b = element('button', label, {type: 'button'});
  b.addEventListener('click', action); parent.append(b); return b;
}
export function controlsKeyboard(node) {
  node.addEventListener('keydown', event => {
    if (event.target.closest('button,input,select') &&
        ['ArrowLeft','ArrowRight','ArrowUp','ArrowDown','Home','End','PageUp','PageDown',' '].includes(event.key))
      event.stopPropagation();
  });
}
export function duration(ms) {
  return matchMedia('(prefers-reduced-motion: reduce)').matches ? 0 : ms;
}
