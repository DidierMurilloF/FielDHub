"""Normalize generated documentation links and build navigation."""
from pathlib import Path
import re
import os
import csv
import yaml

root = Path(".mkdocs-src")
base = "https://didiermurillof.github.io/FielDHub/"
for page in root.rglob("*.md"):
    text = page.read_text()
    def link(match):
        url = match.group(1)
        if url.startswith(base):
            url = os.path.relpath(root / url[len(base):], page.parent)
        if url.startswith(("https://", "http://")):
            return "](" + url + ")"
        if url == ".":
            url = "index.md"
        if url.startswith("vignettes/") and url.endswith(".Rmd"):
            url = url.replace("vignettes/", "articles/", 1).replace(".Rmd", ".md")
        if url == "deployment/README.md":
            url = "https://github.com/DidierMurilloF/FielDHub/blob/master/deployment/README.md"
        if url == "NEWS.md":
            url = "/news/index.md"
        url = re.sub(r"[.]html(?=#|$)", ".md", url)
        return "](" + url + ")"
    text = re.sub(r"\]\(([^)]+)\)", link, text)
    page.write_text(text)
def page_title(path):
    text = path.read_text()
    if text.startswith("---"):
        metadata = yaml.safe_load(text.split("---", 2)[1]) or {}
        if metadata.get("title"):
            return metadata["title"]
    heading = re.search(r"^#{1,6} (.+)$", text, re.MULTILINE)
    return heading.group(1) if heading else path.stem

references = sorted(p.name for p in (root / "reference").glob("*.md") if p.name != "index.md")
(root / "reference/index.md").write_text("# Reference\n\n" + "\n".join(
    f"- [{Path(name).stem} — {page_title(root / 'reference' / name)}]({name})" for name in references))
config = yaml.safe_load(Path("mkdocs.yml").read_text())
background = ["methods", "scientific_validation", "architecture", "extending_fieldhub"]
articles = [p.stem for p in (root / "articles").glob("*.md") if p.stem not in background + ["index"]]
config["nav"] = [
    {"Home": "index.md"}, {"Get started": "get-started.md"},
    {"Designs": [{"Articles": "articles/index.md"}] + [{name.replace("_", " ").title(): f"articles/{name}.md"} for name in sorted(articles)]},
    {"Background": [{name.replace("_", " ").title(): f"articles/{name}.md"} for name in background]},
    {"Reference": [{"Overview": "reference/index.md"}] + [{Path(name).stem: f"reference/{name}"} for name in references]},
    {"News": [{"Changelog": "news/index.md"}] + [{page_title(p): str(p.relative_to(root))} for p in sorted((root / "articles/news").glob("*.md"))]}]
Path("mkdocs.yml").write_text(yaml.safe_dump(config, sort_keys=False))

if (root / "topics.csv").exists():
    groups = {}
    for row in csv.DictReader((root / "topics.csv").open()):
        group = groups.setdefault(row["group"], [])
        target = row["guide"].removeprefix(base).replace(".html", ".md")
        entry = {row["label"].removeprefix("New - "): target}
        if not any(target in item.values() for item in group):
            group.append(entry)
    config["nav"][2] = {"Designs": [{key: value} for key, value in groups.items()]}
    Path("mkdocs.yml").write_text(yaml.safe_dump(config, sort_keys=False))

def targets(node):
    if isinstance(node, str):
        return {node}
    return set().union(*map(targets, node.values() if isinstance(node, dict) else node))

# Every vignette gets a page: articles that are not design guides go to Background.
listed = targets(config["nav"])
config["nav"][3]["Background"] += [
    {page_title(root / f"articles/{name}.md"): f"articles/{name}.md"}
    for name in sorted(articles) if f"articles/{name}.md" not in listed]
Path("mkdocs.yml").write_text(yaml.safe_dump(config, sort_keys=False))

reference_groups = {
    "Designs": [],
    "Allocation and optimization": [],
    "Reading, writing and reproducing": [],
    "App": [],
    "Results and shared arguments": [],
}
allocation = {"do_optim", "swap_pairs", "split_families"}
io = {"read_design", "write_design", "reproduce_design", "reproduce_simulation", "design_formats"}
designs = {row["engine"] for row in csv.DictReader((root / "topics.csv").open())}
for name in references:
    topic = Path(name).stem
    if topic in allocation:
        group = "Allocation and optimization"
    elif topic in io:
        group = "Reading, writing and reproducing"
    elif topic == "run_app":
        group = "App"
    elif topic in designs:
        group = "Designs"
    else:
        group = "Results and shared arguments"
    reference_groups[group].append({topic: f"reference/{name}"})
config["nav"][4] = {"Reference": [{"Overview": "reference/index.md"}] +
    [{group: items} for group, items in reference_groups.items() if items]}
Path("mkdocs.yml").write_text(yaml.safe_dump(config, sort_keys=False))

# The Articles page lists every vignette by title, grouped as in the menu.
def lines(items, depth):
    out = []
    for item in items:
        (title, target), = item.items()
        if isinstance(target, list):
            out += ["", "#" * depth + " " + title, ""] + lines(target, depth + 1)
        else:
            out.append(f"- [{title}]({os.path.relpath(target, 'articles')})")
    return out

sections = [entry for entry in config["nav"] if next(iter(entry)) in ("Designs", "Background", "News")]
(root / "articles/index.md").write_text("# Articles\n" + "\n".join(lines(sections, 2)) + "\n")
config["nav"].insert(2, {"Articles": "articles/index.md"})
Path("mkdocs.yml").write_text(yaml.safe_dump(config, sort_keys=False))
