def on_nav(nav, *, config, files):
    if nav.homepage and config.site_url:
        nav.homepage.canonical_url = config.site_url


def on_post_build(config):
    from pathlib import Path
    import shutil

    site = Path(config.site_dir)
    redirects = {"fieldLayout": "field_layout", "pipe": "index"}
    for old, new in redirects.items():
        target = f"{new}.html"
        canonical = f"{config.site_url}reference/{target}"
        (site / "reference" / f"{old}.html").write_text(
            f'<!doctype html><html lang="en"><head><meta charset="utf-8">'
            f'<title>FielDHub reference</title><link rel="canonical" href="{canonical}">'
            f'<meta http-equiv="refresh" content="0;url={target}"></head>'
            f'<body><a href="{target}">Continue to the reference page</a></body></html>'
        )
    # The historical /dev/ routes serve the current documentation with
    # production canonical URLs so search engines index one copy.
    published = list(site.iterdir())
    for source in published:
        if source.name == "dev":
            continue
        target = site / "dev" / source.name
        if source.is_dir():
            shutil.copytree(source, target, dirs_exist_ok=True)
        else:
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(source, target)

def on_page_markdown(markdown, *, page, config, files):
    # The home page uses the site description.
    if not page.is_homepage and not page.meta.get("description"):
        page.meta["description"] = f"{page.title} — FielDHub R package and Shiny documentation."
    return markdown
