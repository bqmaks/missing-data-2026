"""Embed the reusable mascot into the self-contained title SVG."""

from copy import deepcopy
from pathlib import Path
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[1]
NS = {"svg": "http://www.w3.org/2000/svg"}
ET.register_namespace("", NS["svg"])

mascot = ET.parse(ROOT / "assets/mice-mascot.svg")
title_path = ROOT / "assets/title-missing-matrix.svg"
title = ET.parse(title_path)
art = mascot.find("svg:g[@id='mascot-art']", NS)
placement = title.find("svg:g[@id='mouse-emerging']", NS)
if art is None or placement is None:
    raise ValueError("Missing mascot artwork or title placement group")

placement.clear()
placement.set("id", "mouse-emerging")
placement.set("transform", "translate(92 170) scale(1.10)")
placement.append(deepcopy(art))
ET.indent(title, space="  ")
title.write(title_path, encoding="unicode")
with title_path.open("a") as output:
    output.write("\n")
print("Title mascot updated from assets/mice-mascot.svg")
