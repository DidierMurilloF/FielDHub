"""Check application and source-document public URLs against the built site."""
from pathlib import Path
from urllib.parse import urlsplit
import re

base = "https://didiermurillof.github.io/FielDHub/"
urls = set(Path(".mkdocs-src/app-urls.txt").read_text().splitlines())
for name in ("README.Rmd", "index.Rmd", "R/app_content.R"):
    urls.update(re.findall(re.escape(base) + r'[^\s<>"\)]+', Path(name).read_text()))
required = {"news/index.html", "reference/index.html", "CONTRIBUTING.html",
            "CODE_OF_CONDUCT.html", "LICENSE.html", "authors.html"}
for url in urls:
    if "%s" in url:
        continue  # Runtime-expanded URLs are supplied by fieldhub_help_topics().
    path = urlsplit(url).path.removeprefix("/FielDHub/")
    required.add(path + "index.html" if not path or path.endswith("/") else path)
required.update(Path("website/legacy-urls.txt").read_text().splitlines())
missing = sorted(path for path in required if not (Path(".mkdocs-site") / path).is_file())
if missing:
    raise SystemExit("Missing legacy pages: " + ", ".join(missing))
print(f"Verified {len(required)} public page paths.")

from html.parser import HTMLParser

class Links(HTMLParser):
    def __init__(self, text):
        super().__init__()
        self.links = []
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        attributes = dict(attrs)
        key = "href" if tag == "a" else "src"
        if tag in ("a", "img", "script") and key in attributes:
            self.links.append(attributes[key])

from urllib.parse import unquote
site = Path(".mkdocs-site").resolve()
errors = []
for page in site.rglob("*.html"):
    for href in Links(page.read_text()).links:
        url = urlsplit(href)
        if url.scheme or url.netloc or not url.path:
            continue
        target = (page.parent / unquote(url.path)).resolve()
        if url.path.startswith("/FielDHub/"):
            target = site / url.path.removeprefix("/FielDHub/")
        if target.is_dir():
            target = target / "index.html"
        if not target.exists():
            errors.append(f"{page.relative_to(site)}: {href}")
if errors:
    raise SystemExit("\n".join(errors))
print("All local page links and image/script assets resolve.")
