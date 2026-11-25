"""Build the ring-free cell pose from the shared mascot's face and ears."""

from copy import deepcopy
from pathlib import Path
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[1]
NS = "http://www.w3.org/2000/svg"
MISSING_COLOR = "#b61a51"
OUTLINE_WIDTH = "3.6"
ET.register_namespace("", NS)


def element(tag, **attrs):
    return ET.Element(f"{{{NS}}}{tag}", attrs)


source = ET.parse(ROOT / "assets/mice-mascot.svg")
mouse = source.find(f".//{{{NS}}}g[@id='mascot-mouse']")
if mouse is None:
    raise ValueError("Missing shared mascot")

svg = element("svg", width="224", height="214", viewBox="0 0 224 214",
              role="img", **{"aria-labelledby": "cell-mouse-title cell-mouse-desc"})
title = element("title", id="cell-mouse-title")
title.text = "Мышь держится за нижнюю границу ячейки"
svg.append(title)
desc = element("desc", id="cell-mouse-desc")
desc.text = ("Поза персонажа курса без кольца. Мордочка и уши взяты из mice-mascot.svg, "
             "перерисованного по стикеру mice авторства Jaden M. Walters: "
             "https://amices.org/mice/. Горизонтальная опора и центры лапок находятся "
             "на y=190; нижние 24 единицы выходят за границу ячейки. "
             "Контур 3.6 единицы и детали имеют цвет пропусков #B61A51.")
svg.append(desc)
defs = element("defs")
clip = element("clipPath", id="cell-mouse-crop")
clip.append(element("rect", width="224", height="190"))
defs.append(clip)
svg.append(defs)
body = element("g", id="cell-mouse-body", fill="#ffffff", stroke=MISSING_COLOR,
               **{"stroke-width": OUTLINE_WIDTH, "stroke-linecap": "round",
                  "stroke-linejoin": "round", "clip-path": "url(#cell-mouse-crop)"})
body.append(element("path", id="cell-mouse-torso",
                    d="M140 102 C161 111 185 132 193 147 C187 160 176 171 175 187 L164 204 L105 204 C102 181 102 160 104 145 Z"))
for part_id in ("mascot-ears", "mascot-head", "mascot-face"):
    part = mouse.find(f"{{{NS}}}*[@id='{part_id}']")
    if part is None:
        raise ValueError(f"Missing {part_id}")
    part = deepcopy(part)
    for detail in part.iter():
        if detail.get("stroke") not in (None, "none"):
            detail.set("stroke", MISSING_COLOR)
        if detail.get("fill") not in (None, "none", "#ffffff"):
            detail.set("fill", MISSING_COLOR)
        if detail.get("stroke-width") is not None:
            detail.set("stroke-width", f"{float(detail.get('stroke-width')) * 1.5:g}")
    body.append(part)
svg.append(body)
svg.append(element("path", id="cell-mouse-edge", d="M0 190 H224", fill="none",
                   stroke="#d7dce1", **{"stroke-width": "1", "vector-effect": "non-scaling-stroke"}))
paws = element("g", id="cell-mouse-paws", fill="#ffffff", stroke=MISSING_COLOR,
               **{"stroke-width": OUTLINE_WIDTH, "stroke-linecap": "round", "stroke-linejoin": "round"})
for side, x, angle, radius in (("left", 105, -8, 10.2), ("right", 171, 8, 9.2)):
    paw = element("g", id=f"cell-mouse-paw-{side}", transform=f"translate({x} 190) rotate({angle})")
    paw.append(element("ellipse", rx=str(radius), ry="15.5"))
    paw.append(element("path", d="M-4 7 L-4 14 M2 6 L2 15", fill="none", **{"stroke-width": "2.4"}))
    paws.append(paw)
svg.append(paws)
ET.indent(svg, space="  ")
(ROOT / "assets/mice-cell.svg").write_text(ET.tostring(svg, encoding="unicode") + "\n")
print("Cell pose updated from assets/mice-mascot.svg")
