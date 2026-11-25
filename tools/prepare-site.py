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
def front_matter(path):
    text = path.read_text()
    return (yaml.safe_load(text.split("---", 2)[1]) or {}) if text.startswith("---") else {}

def page_title(path):
    text = path.read_text()
    title = front_matter(path).get("title")
    if title:
        return title
    heading = re.search(r"^#{1,6} (.+?)(?: \{#[^}]*\})?$", text, re.MULTILINE)
    return heading.group(1) if heading else path.stem

references = sorted(p.name for p in (root / "reference").glob("*.md") if p.name != "index.md")
topics = list(csv.DictReader((root / "topics.csv").open()))
config = yaml.safe_load(Path("mkdocs.yml").read_text())

# Designs are grouped as in the app's menus. Each has a guide: an article, or
# its reference page when no article has been written yet.
design_groups = {}
functions = {}
for row in topics:
    guide = row["guide"].removeprefix(base).replace(".html", ".md")
    group = design_groups.setdefault(row["group"], [])
    if not any(guide in item.values() for item in group):
        group.append({row["label"].removeprefix("New - "): guide})
        functions[guide] = row["engine"]

# Every other vignette is a background article.
background = ["methods", "simulating_traits", "scientific_validation", "architecture", "extending_fieldhub"]
background += sorted(p.stem for p in (root / "articles").glob("*.md")
                     if p.stem not in background + ["index"] and f"articles/{p.name}" not in functions)
background = [{page_title(root / f"articles/{name}.md"): f"articles/{name}.md"} for name in background]
news = [{"Changelog": "news/index.md"}] + [
    {page_title(p): str(p.relative_to(root))} for p in sorted((root / "articles/news").glob("*.md"), reverse=True)]

reference_groups = {
    "Designs": [],
    "Allocation and optimization": [],
    "Reading, writing and reproducing": [],
    "App": [],
    "Results and shared arguments": [],
}
allocation = {"do_optim", "swap_pairs", "split_families"}
io = {"read_design", "write_design", "reproduce_design", "reproduce_simulation", "design_formats"}
designs = {row["engine"] for row in topics}
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
reference_groups = {group: items for group, items in reference_groups.items() if items}

# A page has one place in the menus, so the Designs menu lists the articles
# only; a design whose guide is its reference page is found under Reference.
config["nav"] = [
    {"Home": "index.md"},
    {"Get started": "get-started.md"},
    {"Articles": "articles/index.md"},
    {"Designs": [{group: [item for item in items if "".join(item.values()).startswith("articles/")]}
                 for group, items in design_groups.items()]},
    {"Background": background},
    {"Reference": [{"Overview": "reference/index.md"}] + [{group: items} for group, items in reference_groups.items()]},
    {"News": news},
    {"About": [{"Authors and citation": "authors.md"}, {"Contributing": "CONTRIBUTING.md"},
               {"Code of conduct": "CODE_OF_CONDUCT.md"}, {"License": "LICENSE.md"},
               {"Releasing": "RELEASING.md"}]}]
Path("mkdocs.yml").write_text(yaml.safe_dump(config, sort_keys=False))

# The reference index lists every topic with its title, grouped as in the menu.
index = ["---", "title: Function reference", "---", "# Reference", "",
         "Every exported function and help topic. The same pages are available in R with `help()`.",
         "", '<div class="fieldhub-reference" markdown>']
for group, items in reference_groups.items():
    index += ["", f"## {group}", "", "| Topic | Description |", "|---|---|"]
    for item in items:
        (topic, target), = item.items()
        index.append(f"| [`{topic}`]({Path(target).name}) | {page_title(root / target).rstrip('.')} |")
(root / "reference/index.md").write_text("\n".join(index + ["", "</div>"]) + "\n")

# The Articles page shows every vignette as a card, grouped as in the menus.
def cards(items, fallback=""):
    out = ['<div class="grid cards fieldhub-cards" markdown>', ""]
    for item in items:
        (title, target), = item.items()
        text = front_matter(root / target).get("description") or fallback or page_title(root / target)
        out += [f"-   [{title}]({os.path.relpath(target, 'articles')})", "", f"    {text.rstrip('.')}.", ""]
        if target in functions:
            out += [f"    `{functions[target]}()`", ""]
    return out + ["</div>", ""]

articles = ["---", "title: Articles",
            "description: Guides for every FielDHub design, background articles and release notes.",
            "---", "# Articles", "",
            "Each design has a guide that builds it twice: in the Shiny app and with its R function.", "",
            "## Designs", ""]
for group, items in design_groups.items():
    articles += [f"### {group}", ""] + cards(items)
articles += ["## Background", ""] + cards(background)
articles += ["## News", ""] + cards(news[:1], "Every change in every release.") + cards(news[1:], "Release announcement.")
(root / "articles/index.md").write_text("\n".join(articles))
