# Run the Shiny Application

Run the Shiny Application

## Usage

``` r
run_app(..., launch.browser = TRUE, workers = 0L)
```

## Arguments

- ...:

  Unused, for extensibility

- launch.browser:

  Logical. If \`TRUE\`, the application is launched in the system's
  default web browser.

- workers:

  Non-negative integer. Background workers; the default `0L` keeps
  synchronous execution and does not start any worker processes.

## Value

A shiny app object

## Details

Constructing the app does not change R options. On startup, uploads are
limited to 100 MiB (100 \* 1024^2 bytes); when the app stops, the
previous upload-limit option is restored, including an unset option.
Shiny uses a process-wide upload limit, shared by the app's sessions
while it is running. Closing one session does not restore the limit for
others.

The standard app packages are installed with FielDHub. If any are
unavailable, this function signals a `fieldhub_dependency_error` with an
installation command and the missing package names in its `packages`
field. It never installs packages automatically. Only the
background-worker backend is optional.

With `workers > 0`, an installed package and the optional mirai package
run design jobs in a private background pool. Other sessions remain
responsive while a design runs. The pool closes when the app stops.
Without mirai, or with development source, execution stays synchronous
with a message.
