"""Import the author-provided PPTX in presentation order, preserving its content.

Run explicitly after reviewing changed course-inputs. The files in that directory
are never modified. Uses only the Python standard library.
"""
from pathlib import Path
from xml.etree import ElementTree as ET
from zipfile import ZipFile
import hashlib
import json
import posixpath
import re

ROOT = Path(__file__).resolve().parents[1]
NS = {"p": "http://schemas.openxmlformats.org/presentationml/2006/main",
      "a": "http://schemas.openxmlformats.org/drawingml/2006/main",
      "r": "http://schemas.openxmlformats.org/officeDocument/2006/relationships"}


def paragraphs(node):
    return ["".join(t.text or "" for t in p.findall(".//a:t", NS))
            for p in node.findall(".//a:p", NS)]


def relationships(archive, path):
    folder, name = posixpath.split(path)
    rel = f"{folder}/_rels/{name}.rels"
    if rel not in archive.namelist():
        return {}
    return {r.attrib["Id"]: posixpath.normpath(posixpath.join(folder, r.attrib["Target"]))
            for r in ET.fromstring(archive.read(rel))
            if r.attrib.get("TargetMode") != "External"}


def position(node):
    tr = node.find(".//a:xfrm", NS)
    if tr is None:
        tr = node.find("p:xfrm", NS)
    if tr is None:
        return [0, 0, 0, 0]
    off, ext = tr.find("a:off", NS), tr.find("a:ext", NS)
    return [int(off.get("x", 0)), int(off.get("y", 0)),
            int(ext.get("cx", 0)), int(ext.get("cy", 0))]


def main():
    source = ROOT / "course-inputs/missing_data_full_course-v2.pptx"
    plan = (ROOT / "course-inputs/content-plan.md").read_text()
    mapping = {}
    for line in plan.splitlines():
        cells = [c.strip() for c in line.split("|")]
        if len(cells) >= 5 and re.fullmatch(r"v2:\d+", cells[1]):
            layer = next(c for c in cells[3:5] if c in ("C", "D", "A"))
            mapping[int(cells[1][3:])] = (cells[2], {"C":"core", "D":"deep", "A":"appendix"}[layer])
    target = ROOT / "slides"
    target.mkdir(exist_ok=True)
    media_dir = ROOT / "assets/source"
    media_dir.mkdir(parents=True, exist_ok=True)
    slides = []
    with ZipFile(source) as z:
        rels = relationships(z, "ppt/presentation.xml")
        pres = ET.fromstring(z.read("ppt/presentation.xml"))
        size = pres.find("p:sldSz", NS)
        for number, entry in enumerate(pres.findall(".//p:sldId", NS), 1):
            path = rels[entry.get("{" + NS["r"] + "}id")]
            sr = relationships(z, path)
            shapes = []
            tree = ET.fromstring(z.read(path)).find("p:cSld/p:spTree", NS)
            for shape in tree:
                kind = shape.tag.split("}")[-1]
                if kind not in ("sp", "pic", "graphicFrame", "grpSp"):
                    continue
                text = [p for p in paragraphs(shape) if p.strip()]
                item = {"kind": kind, "position": position(shape), "paragraphs": text}
                fill = shape.find("p:spPr/a:solidFill/a:srgbClr", NS)
                if fill is not None:
                    item["fill"] = fill.get("val")
                if kind == "pic":
                    blip = shape.find(".//a:blip", NS)
                    image = sr.get(blip.get("{" + NS["r"] + "}embed"))
                    name = Path(image).name
                    (media_dir / name).write_bytes(z.read(image))
                    item["image"] = "assets/source/" + name
                table = shape.find(".//a:tbl", NS)
                if table is not None:
                    item["table"] = [["\n".join(paragraphs(cell)) for cell in row.findall("a:tc", NS)]
                                     for row in table.findall("a:tr", NS)]
                if text or kind == "pic":
                    shapes.append(item)
            notes, flags, widget_notes = [], [], []
            for rel in sr.values():
                if "/notesSlides/" not in rel:
                    continue
                nt = ET.fromstring(z.read(rel))
                for sp in nt.findall(".//p:sp", NS):
                    ph = sp.find("p:nvSpPr/p:nvPr/p:ph", NS)
                    if ph is None or ph.get("type") != "body":
                        continue
                    for p in paragraphs(sp):
                        if not p.strip() or p.startswith("[АУДИАЛЬНЫЙ СЦЕНАРИЙ"):
                            continue
                        if "[ПРОВЕРИТЬ" in p:
                            flags.append(p)
                        elif "[ИНТЕРАКТИВ ДЛЯ HTML]" in p:
                            widget_notes.append(p)
                        else:
                            notes.append(p)
            sid, layer = mapping[number]
            slides.append({"number": number, "id": sid, "layer": layer,
                           "part": 1 if number <= 42 else 2 if number <= 68 else 3,
                           "xml": path, "shapes": shapes, "notes": notes,
                           "review": flags, "widget_notes": widget_notes})
    assert len(slides) == len(mapping) == 89
    result = {"source": source.name, "sha256": hashlib.sha256(source.read_bytes()).hexdigest(),
              "size": [int(size.get("cx")), int(size.get("cy"))], "slides": slides}
    (target / "source-deck.json").write_text(json.dumps(result, ensure_ascii=False, indent=2) + "\n")
    print(f"Imported {len(slides)} slides in presentation order; source SHA256 {result['sha256']}")
    for slide in slides:
        if slide["review"]:
            print(f"Review: v2:{slide['number']} ({slide['id']}): {len(slide['review'])} note(s)")


if __name__ == "__main__":
    main()
