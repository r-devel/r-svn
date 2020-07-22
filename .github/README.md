# R-SVN CI tool

A mirror of R [svn-trunk](https://svn.r-project.org/R/trunk/) for tracking changes and testing patches using GitHub actions. 

## How it works

This mirror synchronises every 15 minutes with the official R [svn server](https://svn.r-project.org/R/trunk/). Each new commit triggers a build on every platform, and the results are published on: https://r-devel.github.io, which shows an overview of the most recent revisions, including links to the build logs. 

The Github icon in the last column links to the GitHub actions page for each build, where you can also download the generated Windows installer. These installers are not signed, they are only for testing.

## Testing patches

You can also use this mirror to generate and test patches that you want to propose for base-R:

 1. Fork this repository by clicking the fork button on the top of the page.
 2. Push your changes to your forked repository.
 3. Send a pull request with your changes back to this repo.

When you send the PR, GitHub actions will start building and testing your changes on all platforms and build the Windows installer.

Obviously we cannot merge your changes because this is a mirror. To propose your changes to the R community, send an email to the [r-devel mailing list](https://www.r-project.org/mail.html) and include a link (or copy) of the patch file of your PR. You can get easily the patch by appending `.diff` to the url of the pull request. For example [pr#7](https://github.com/r-devel/r-svn/pull/7) would be: `https://github.com/r-devel/r-svn/pull/7.diff`. You can also submit a description of your propopoed change along with a patch to the [R bug reporting system](https://bugs.r-project.org/); see [Bug Reporting in R](https://www.r-project.org/bugs.html) for more details.
