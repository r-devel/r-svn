# R-SVN CI mirror

A community maintained git-mirror of the svn for tracking changes and testing patches using GitHub actions.

## How it works

The mirror at <https://github.com/r-devel/r-svn> synchronises every few minutes with the official R [svn server](https://svn.r-project.org/R/trunk/). Each new commit triggers a build on every platform, and the results are published a [dashboard](https://contributor.r-project.org/svn-dashboard) which shows an overview of the most recent revisions, including links to the build logs.

The workflows and scripts used to configure, build, and check R on various platforms are maintained by the community at: https://github.com/r-devel/actions.

## Testing patches

You can also use this mirror to generate and test patches that you want to propose for base-R:

 1. Fork the repository by clicking the fork button on the top of the page.
 2. Push your changes to your forked repository.
 3. Send a pull request with your changes back to this repo.

When you send the PR, GitHub actions will start building and testing your changes on all platforms and build the Windows installers.

Because this is a mirror, we can not merge your changes. To propose your changes to the R community, send an email to the [r-devel mailing list](https://www.r-project.org/mail.html) and include a link (or copy) of the patch file of your PR. You can easily get the patch by appending `.diff` to the url of the pull request. For example [pr#7](https://github.com/r-devel/r-svn/pull/7) would be: `https://github.com/r-devel/r-svn/pull/7.diff`. You can also submit a description of your proposed change along with a patch to the [R bug reporting system](https://bugs.r-project.org/); see [Bug Reporting in R](https://www.r-project.org/bugs.html) for more details.
