# Installed-package deployment

From the repository root:

```sh
docker build --tag fieldhub-local .
docker run --rm --network none --entrypoint Rscript fieldhub-local --vanilla check-runtime.R
docker run --rm --publish 127.0.0.1:3838:3838 fieldhub-local
```

Open `http://127.0.0.1:3838` on the host. The image starts the installed package,
not the development launcher, and runs as UID/GID 10001. It does not need a
golem config file, source loader, runtime package installation or writable R
library. The default is synchronous execution (`workers = 0L`); mirai is locked
and installed for deployments that explicitly opt into workers. Put shared
internet-facing deployments behind a maintained HTTPS/authentication proxy;
Shiny itself does not supply access control.

## Reproducibility and updates

The Rocker R 4.5.3 base is pinned to a multi-platform image digest and the full
app dependency closure is recorded in `renv.lock`. The renv bootstrap archive
is separately pinned and SHA-256 checked. Restoring never follows current
package versions. The runtime check verifies every recorded package version,
the R version, non-root identity, app construction and a fixed-seed replay.

The lock controls R dependencies, not the availability of upstream archives or
every operating-system update: `apt-get` resolves security-updated system
libraries during the build. Preserve the built image digest with release
evidence; do not claim bit-for-bit reproducible images from the R lock alone.
See the [Rocker reproducibility guidance](https://rocker-project.org/use/reproducibility.html).

To update dependencies, first prepare and test the desired versions in a
separate R library, then run `Rscript tools/lock-deployment.R .`. This command
snapshots installed versions without initializing renv in the repository or
updating your library. Review the resulting diff. If renv or R changes, update
the Docker bootstrap checksum/base digest and static contract check together.
Build again, run the runtime contract, and verify actual app startup manually.
Do not commit private library paths, credentials or local environment files.

`Rscript tools/check-deployment.R .` checks the source configuration; it does
not prove the image builds. The `deployment` workflow builds the image and
runs the installed non-root contract without network access. It does not
publish an image. A maintainer must run that workflow and preserve its result
before a release; local package tests are not deployment evidence.
