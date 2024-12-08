# R-SVN CI tool

A mirror of R [svn-trunk](https://svn.r-project.org/R/trunk/) for tracking changes and testing patches using GitHub actions. 

## How it works

This mirror synchronises every 15 minutes with the official R [svn server](https://svn.r-project.org/R/trunk/). Each new commit triggers a build on every platform, and the results are published on: https://contributor.r-project.org/svn-dashboard, which shows an overview of the most recent revisions, including links to the build logs.

The Github icon in the last column links to the GitHub actions page for each build, where you can also download the generated Windows installer. These installers are not signed, they are only for testing.

## Testing patches

You can also use this mirror to generate and test patches that you want to propose for base-R:

 1. Fork this repository by clicking the fork button on the top of the page.
 2. Push your changes to your forked repository.
 3. Send a pull request with your changes back to this repo.

When you send the PR, GitHub actions will start building and testing your changes on all platforms and build the Windows installer.

Because this is a mirror, we will not merge your changes. To propose your changes to the R community, send an email to the [r-devel mailing list](https://www.r-project.org/mail.html) and include a link (or copy) of the patch file of your PR. You can easily get the patch by appending `.diff` to the url of the pull request. For example [pr#7](https://github.com/r-devel/r-svn/pull/7) would be: `https://github.com/r-devel/r-svn/pull/7.diff`. You can also submit a description of your proposed change along with a patch to the [R bug reporting system](https://bugs.r-project.org/); see [Bug Reporting in R](https://www.r-project.org/bugs.html) for more details.

## The build system

In the same way you can test patches, you can use pull requests to experiment with changes in the build matrix.

Build scripts for GitHub actions are always stored in the [workflows](./workflows) directory. The [build-svn.yaml](./workflows/build-svn.yaml) file lists commands used to prepare the server and build R on each of the platforms. Here you can easily enable/disable features, or add another flavor to the mix.

## Build locally

The [`examples`](./examples) directory contains scripts to show how to build R locally. Basically these scripts just let you run the same steps as the CI in [`build-svn.yaml`](./workflows/build-svn.yaml) for a given platform:

 - [`build-r-macos.sh`](./examples/build-r-macos.sh): do a full prep, build, and check on MacOS.

## Develop locally

For local development, `git status` should accurately reflect the state of the repository.
A default clone will have the following two problems:

- A large amount of untracked unignored files.
- A change in `Makefile.in` necessitated by having to build from a Git clone.

The [`local`](./local) directory contains files to help with these issues:

- [`dot-gitignore`](./local/dot-gitignore): A `.gitignore` file ready to copy to the root of the project.
- [`post-checkout`](./local/post-checkout): A script to update the `SVNINFO` file after each checkout or commit.

Run the [`setup.sh`](./local/setup.sh) script for automated setup.

For creating `compile_commands.json` for use with LSPs (autocompletion in your IDE), use the [bear](https://github.com/rizsotto/Bear) tool:

```sh
bear -- make -j4
```
