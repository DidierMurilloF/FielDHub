def on_nav(nav, *, config, files):
    # The public home URL is the project root, while legacy articles and
    # reference topics use explicit .html filenames.
    if nav.homepage and config.site_url:
        nav.homepage.canonical_url = config.site_url
        for item in nav.items:
            if item.is_link and item.title == "Get started":
                item.url = f"{nav.homepage.url}#install-and-launch"
